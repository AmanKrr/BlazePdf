use crate::dom::dom_tree::{Document, ElementNode, Node};
use crate::style::owned_css::{OwnedDeclaration, OwnedRule, OwnedStylesheet};
use lightningcss::error::{Error as LcssError, ParserError};
use lightningcss::printer::PrinterOptions;
use lightningcss::rules::{style::StyleRule, CssRule};
use lightningcss::stylesheet::{ParserOptions, StyleSheet as LightningStyleSheet};
use lightningcss::traits::ToCss;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Represents final set of CSS properties each element gets.
#[derive(Default, Clone, Debug)]
pub struct ComputedStyle {
    pub properties: HashMap<String, String>,
}

impl ComputedStyle {
    pub fn new() -> Self {
        ComputedStyle {
            properties: HashMap::new(),
        }
    }
}

pub fn parse(css_snippet: &str, dom_document: &Document) {
    let owned_sheet = parse_and_own_css(css_snippet).expect("Failed to parse & own CSS");
    apply_owned_css_to_dom(&dom_document, &owned_sheet);
}

/// Parse a raw CSS string (LightningCSS) and convert it to a fully-owned stylesheet.
fn parse_and_own_css(css_text: &str) -> Result<OwnedStylesheet, ParserError<'_>> {
    let parser_opts = ParserOptions::default();

    // The `.map_err(|e: LcssError<ParserError>| e.error)` call extracts the *inner*
    // `ParserError` from LightningCSS's `Error<ParserError>` type so that
    // our function returns `Result<_, ParserError<'_>>` (no wrapper).
    let sheet = LightningStyleSheet::parse(css_text, parser_opts)
        .map_err(|e: LcssError<ParserError<'_>>| e.kind)?;

    // We'll build an OwnedStylesheet from the returned rules
    let mut owned_rules = Vec::new();

    for rule in &sheet.rules.0 {
        match rule {
            CssRule::Style(style_rule) => {
                let owned = convert_style_rule(style_rule);
                owned_rules.push(owned);
            }
            CssRule::Media(media_rule) => {
                // Optionally handle media queries. This example flattens nested style rules:
                for inner_rule in &media_rule.rules.0 {
                    if let CssRule::Style(sr) = inner_rule {
                        let owned = convert_style_rule(sr);
                        owned_rules.push(owned);
                    }
                }
            }
            // For PDF, you might skip @font-face, @keyframes, etc.
            _ => {}
        }
    }

    Ok(OwnedStylesheet { rules: owned_rules })
}

/// Helper to copy a single StyleRule's selectors + declarations into OwnedRule.
fn convert_style_rule<'a>(style_rule: &StyleRule<'a>) -> OwnedRule {
    let mut selectors_vec = Vec::new();
    for selector in &style_rule.selectors.0 {
        if let Ok(sel_str) = selector.to_css_string(Default::default()) {
            selectors_vec.push(sel_str);
        }
    }

    // The DeclarationBlock
    let block = &style_rule.declarations;

    // We'll combine normal + important declarations into one vector,
    // tagging the latter with `is_important = true`.
    let mut decls_vec = Vec::new();

    // Normal declarations
    for property in &block.declarations {
        let property_name = property.property_id().name().to_string();
        let property_value = property
            .value_to_css_string(PrinterOptions::default())
            .unwrap();
        decls_vec.push(OwnedDeclaration {
            property: property_name,
            value: property_value,
            // important: false,
        });
    }

    // !important declarations
    for property in &block.important_declarations {
        let property_name = property.property_id().name().to_string();
        let property_value = property
            .value_to_css_string(PrinterOptions::default())
            .unwrap();
        decls_vec.push(OwnedDeclaration {
            property: property_name,
            value: property_value,
            // important: true,
        });
    }

    OwnedRule {
        selectors: selectors_vec,
        declarations: decls_vec,
    }
}

/// Apply an OwnedStylesheet to a Document, walking the DOM and computing styles.
pub fn apply_owned_css_to_dom(document: &Document, stylesheet: &OwnedStylesheet) {
    apply_owned_styles_recursive(&document.root, &stylesheet.rules);
}

/// Recursively walk the DOM
fn apply_owned_styles_recursive(node_handle: &Rc<RefCell<Node>>, rules: &[OwnedRule]) {
    // Mutably borrow the Node
    let mut node_borrow = node_handle.borrow_mut();

    match &mut *node_borrow {
        Node::DocumentRoot(doc_root) => {
            // Recurse on each child
            for child_rc in &doc_root.children {
                apply_owned_styles_recursive(child_rc, rules);
            }
        }
        Node::Element(elem_node) => {
            let computed_style = compute_element_style(&elem_node, rules);

            // Build your style attribute
            let mut style_vec = Vec::new();
            for (k, v) in &computed_style.properties {
                style_vec.push(format!("{}: {}", k, v));
            }

            if !style_vec.is_empty() {
                // Now we can mutate `elem_node.attributes` because
                // we have a mutable borrow of `elem_node`
                elem_node
                    .attributes
                    .insert("style".to_string(), style_vec.join("; "));
            }

            // Recurse on children
            for child_rc in &elem_node.children {
                apply_owned_styles_recursive(child_rc, rules);
            }
        }
        Node::Text(_) => {}
    }
}

/// Build a final ComputedStyle by matching each OwnedRule's selectors to this element.
fn compute_element_style(elem_node: &ElementNode, rules: &[OwnedRule]) -> ComputedStyle {
    let mut final_style = ComputedStyle::new();

    for rule in rules {
        if element_matches_rule(elem_node, rule) {
            // Copy each property => value into final_style
            for decl in &rule.declarations {
                final_style
                    .properties
                    .insert(decl.property.clone(), decl.value.clone());
            }
        }
    }
    final_style
}

/// True if any of the rule’s selectors match the element.
fn element_matches_rule(elem_node: &ElementNode, rule: &OwnedRule) -> bool {
    for sel_str in &rule.selectors {
        if simple_match_selector(elem_node, sel_str) {
            return true;
        }
    }
    false
}

/// Very naive matching logic for .class, #id, or tag names
fn simple_match_selector(elem_node: &ElementNode, selector_str: &str) -> bool {
    let trimmed = selector_str.trim();
    if let Some(class_part) = trimmed.strip_prefix('.') {
        // see if element has class=class_part
        return has_class(elem_node, class_part);
    }
    if let Some(id_part) = trimmed.strip_prefix('#') {
        // see if element has id=id_part
        return has_id(elem_node, id_part);
    }
    // else assume tag
    elem_node.tag.eq_ignore_ascii_case(trimmed)
}

fn has_class(elem_node: &ElementNode, class_name: &str) -> bool {
    for (attr_name, attr_val) in &elem_node.attributes {
        if attr_name.eq_ignore_ascii_case("class") {
            let classes: Vec<_> = attr_val.split_whitespace().collect();
            if classes.contains(&class_name) {
                return true;
            }
        }
    }
    false
}

fn has_id(elem_node: &ElementNode, id_val: &str) -> bool {
    for (attr_name, attr_val) in &elem_node.attributes {
        if attr_name.eq_ignore_ascii_case("id") && attr_val == id_val {
            return true;
        }
    }
    false
}
