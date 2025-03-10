use crate::dom::dom_tree::{Document, Node};
use crate::parser::dom_indices::DomIndices;
use crate::style::css_matcher::{
    compute_computed_style, matches_complex_selector, parse_selector, CssRule,
};
use crate::style::owned_css::{OwnedDeclaration, OwnedRule, OwnedStylesheet};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use lightningcss::error::ParserError;
use lightningcss::printer::PrinterOptions;
use lightningcss::rules::style::StyleRule;
use lightningcss::traits::ToCss;
use taffy::prelude::*;

use super::css_matcher::ComputedStyle;

/// A newtype wrapper for Rc<RefCell<Node>> that implements Hash and Eq based on pointer identity.
#[derive(Clone)]
struct NodeWrapper(Rc<RefCell<Node>>);

impl PartialEq for NodeWrapper {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for NodeWrapper {}

impl Hash for NodeWrapper {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.0) as usize).hash(state)
    }
}

/// Top-level integration function.
/// Given a CSS snippet and a DOM document, it:
/// 1. Extracts inline <style> blocks from the DOM.
/// 2. Parses and converts the combined CSS to an OwnedStylesheet.
/// 3. Builds global indices.
/// 4. Applies CSS rules by matching them to DOM nodes and merging them via compute_computed_style.
/// 5. Injects the resulting computed styles into the DOM as inline "style" attributes.
pub fn parse(css_snippet: &str, dom_document: &Document) {
    // 1. Extract inline <style> blocks.
    let inline_style_blocks = extract_style_blocks(dom_document);
    let combined_css = format!("{}\n{}", css_snippet, inline_style_blocks);

    // 2. Parse and own the CSS.
    let owned_sheet = parse_and_own_css(&combined_css).expect("Failed to parse & own CSS");

    // 3. Build global DOM indices.
    let indices = DomIndices::build(dom_document);

    // 4. Apply CSS rules using the indices.
    apply_css_with_indices(dom_document, &owned_sheet, &indices);
}

/// Recursively extracts CSS from all <style> blocks in the DOM.
fn extract_style_blocks(document: &Document) -> String {
    let mut styles = String::new();
    extract_styles_recursive(&document.root, &mut styles);
    styles
}

fn extract_styles_recursive(node: &Rc<RefCell<Node>>, styles: &mut String) {
    let node_borrow = node.borrow();
    match &*node_borrow {
        Node::DocumentRoot(root) => {
            for child in &root.children {
                extract_styles_recursive(child, styles);
            }
        }
        Node::Element(elem) => {
            if elem.tag.eq_ignore_ascii_case("style") {
                for child in &elem.children {
                    if let Node::Text(text) = &*child.borrow() {
                        styles.push_str(text);
                        styles.push('\n');
                    }
                }
            }
            for child in &elem.children {
                extract_styles_recursive(child, styles);
            }
        }
        Node::Text(_) => {}
    }
}

/// Parse raw CSS (external + inline) using LightningCSS and convert it to an OwnedStylesheet.
fn parse_and_own_css(css_text: &str) -> Result<OwnedStylesheet, ParserError<'_>> {
    use lightningcss::stylesheet::ParserOptions;
    let parser_opts = ParserOptions::default();
    let sheet =
        lightningcss::stylesheet::StyleSheet::parse(css_text, parser_opts).map_err(|e| e.kind)?;
    let mut owned_rules = Vec::new();
    for rule in &sheet.rules.0 {
        match rule {
            lightningcss::rules::CssRule::Style(style_rule) => {
                let owned = convert_style_rule(style_rule);
                owned_rules.push(owned);
            }
            lightningcss::rules::CssRule::Media(media_rule) => {
                for inner_rule in &media_rule.rules.0 {
                    if let lightningcss::rules::CssRule::Style(sr) = inner_rule {
                        let owned = convert_style_rule(sr);
                        owned_rules.push(owned);
                    }
                }
            }
            _ => {}
        }
    }
    Ok(OwnedStylesheet { rules: owned_rules })
}

/// Convert a LightningCSS StyleRule into an OwnedRule.
fn convert_style_rule<'a>(style_rule: &StyleRule<'a>) -> OwnedRule {
    let mut selectors_vec = Vec::new();
    for selector in &style_rule.selectors.0 {
        if let Ok(sel_str) = selector.to_css_string(Default::default()) {
            selectors_vec.push(sel_str);
        }
    }
    let block = &style_rule.declarations;
    let mut decls_vec = Vec::new();
    for property in &block.declarations {
        let property_name = property.property_id().name().to_string();
        let property_value = property
            .value_to_css_string(PrinterOptions::default())
            .unwrap();
        decls_vec.push(OwnedDeclaration {
            property: property_name,
            value: property_value,
        });
    }
    for property in &block.important_declarations {
        let property_name = property.property_id().name().to_string();
        let property_value = property
            .value_to_css_string(PrinterOptions::default())
            .unwrap();
        decls_vec.push(OwnedDeclaration {
            property: property_name,
            value: property_value,
        });
    }
    OwnedRule {
        selectors: selectors_vec,
        declarations: decls_vec,
    }
}

/// Applies CSS rules to the DOM using global indices.
/// For each rule, it:
/// - Parses the selector into a ComplexSelector,
/// - Finds candidate DOM nodes using the indices,
/// - Checks full matching,
/// - Collects matched rules for each node,
/// - And then merges them via compute_computed_style.
pub fn apply_css_with_indices(
    document: &Document,
    stylesheet: &OwnedStylesheet,
    indices: &DomIndices,
) {
    let mut matched_map: HashMap<*const RefCell<Node>, Vec<CssRule>> = HashMap::new();

    // Iterate over each OwnedRule in the stylesheet.
    for rule in &stylesheet.rules {
        // For each selector string in the rule...
        for sel_str in &rule.selectors {
            // Parse the selector into our ComplexSelector.
            let complex_sel = parse_selector(sel_str);
            // Use global indices to narrow down candidates.
            let candidates = find_candidates_for_selector(&complex_sel, indices);
            for candidate in candidates {
                if matches_complex_selector(&candidate, &complex_sel) {
                    let key = Rc::as_ptr(&candidate);
                    matched_map
                        .entry(key)
                        .or_insert_with(Vec::new)
                        // Convert the OwnedRule into our internal CssRule.
                        .push(CssRule {
                            selector: complex_sel.clone(),
                            declarations: rule
                                .declarations
                                .clone()
                                .into_iter()
                                .map(|decl| (decl.property, decl.value))
                                .collect(),
                            source_order: 0,
                        });
                }
            }
        }
    }

    // For each candidate node, merge the matched rules into one computed style.
    let mut computed_styles: HashMap<*const RefCell<Node>, ComputedStyle> = HashMap::new();
    for (node_ptr, rules) in matched_map {
        let merged = compute_computed_style(rules, None);
        computed_styles.insert(node_ptr, merged);
    }

    // Inject the computed styles into the DOM.
    inject_computed_styles(&document.root, &computed_styles);
}

/// Finds candidate nodes for a given ComplexSelector using the global indices.
fn find_candidates_for_selector(
    complex: &crate::style::css_matcher::ComplexSelector,
    indices: &DomIndices,
) -> Vec<Rc<RefCell<Node>>> {
    let key = &complex.key;
    let mut candidate_set: Option<HashSet<NodeWrapper>> = None;
    if let Some(ref id) = key.id {
        if let Some(node) = indices.id_map.get(id) {
            let mut set = HashSet::new();
            set.insert(NodeWrapper(Rc::clone(node)));
            candidate_set = Some(set);
        } else {
            return vec![];
        }
    }
    for class in &key.classes {
        if let Some(nodes) = indices.class_map.get(class) {
            let set: HashSet<_> = nodes.iter().map(|rc| NodeWrapper(Rc::clone(rc))).collect();
            candidate_set = match candidate_set {
                Some(prev) => Some(prev.intersection(&set).cloned().collect()),
                None => Some(set),
            };
        } else {
            return vec![];
        }
    }
    if let Some(ref tag) = key.tag {
        if let Some(nodes) = indices.tag_map.get(&tag.to_lowercase()) {
            let set: HashSet<_> = nodes.iter().map(|rc| NodeWrapper(Rc::clone(rc))).collect();
            candidate_set = match candidate_set {
                Some(prev) => Some(prev.intersection(&set).cloned().collect()),
                None => Some(set),
            };
        } else {
            return vec![];
        }
    }
    candidate_set.map_or_else(Vec::new, |s| s.into_iter().map(|nw| nw.0).collect())
}

/// Recursively traverses the DOM and injects computed styles as inline "style" attributes.
fn inject_computed_styles(
    node: &Rc<RefCell<Node>>,
    styles: &HashMap<*const RefCell<Node>, ComputedStyle>,
) {
    let mut node_borrow = node.borrow_mut();
    match &mut *node_borrow {
        Node::DocumentRoot(root) => {
            for child in &root.children {
                inject_computed_styles(child, styles);
            }
        }
        Node::Element(elem) => {
            let ptr = Rc::as_ptr(node);
            if let Some(style) = styles.get(&ptr) {
                elem.layout = Some(style.clone());

                // let mut style_vec = Vec::new();
                // for (k, v) in &style.properties {
                //     style_vec.push(format!("{}: {}", k, v));
                // }
                // if !style_vec.is_empty() {
                //     if let Some(existing) = elem.attributes.get("style") {
                //         style_vec.insert(0, existing.replace(";", "").clone());
                //     }
                //     let mut inline_style = style_vec.join("; ");
                //     inline_style.push(';');
                //     elem.attributes.insert("style".to_string(), inline_style);
                // }

                // Convert the Taffy style to a CSS string.
                // (You’d need a function that converts a taffy::Style into a string.
                // Here we assume one exists: taffy_style_to_css_string.)
                // let css_from_taffy = taffy_style_to_css_string(&comp_style.taffy_style);
                // let mut inline_style = css_from_taffy;
                // // Append other properties.
                // for (prop, value) in &comp_style.other_properties {
                //     inline_style.push_str(&format!("{}: {}; ", prop, value));
                // }
                // elem.attributes.insert("style".to_string(), inline_style);
            }
            for child in &elem.children {
                inject_computed_styles(child, styles);
            }
        }
        Node::Text(_) => {}
    }
}

fn taffy_style_to_css_string(style: &Style) -> String {
    let mut css = String::new();

    // Display property.
    css.push_str(&format!(
        "display: {};",
        match style.display {
            Display::Block => "block",
            Display::Flex => "flex",
            Display::Grid => "grid",
            // Display::Inline => "inline",
            // Display::InlineBlock => "inline-block",
            Display::None => "none",
            // Extend for other variants as needed.
            _ => "initial",
        }
    ));

    // Width
    css.push_str(" width: ");
    let width_str = match style.size.width {
        Dimension::Length(val) => format!("{}px", val),
        Dimension::Auto => "auto".to_string(),
        // If you have percentages or other variants, handle them here.
        _ => "initial".to_string(),
    };
    css.push_str(&width_str);
    css.push(';');

    // Height
    css.push_str(" height: ");
    let height_str = match style.size.height {
        Dimension::Length(val) => format!("{}px", val),
        Dimension::Auto => "auto".to_string(),
        _ => "initial".to_string(),
    };
    css.push_str(&height_str);
    css.push(';');

    // Margin (if available; assuming margin is represented as a Rect<Dimension>)
    css.push_str(" margin: ");
    css.push_str(&format!(
        "{} {} {} {};",
        match style.margin.left {
            LengthPercentageAuto::Length(val) => format!("{}px", val),
            _ => "0".to_string(),
        },
        match style.margin.top {
            LengthPercentageAuto::Length(val) => format!("{}px", val),
            _ => "0".to_string(),
        },
        match style.margin.right {
            LengthPercentageAuto::Length(val) => format!("{}px", val),
            _ => "0".to_string(),
        },
        match style.margin.bottom {
            LengthPercentageAuto::Length(val) => format!("{}px", val),
            _ => "0".to_string(),
        }
    ));

    // Padding (similarly)
    css.push_str(" padding: ");
    css.push_str(&format!(
        "{} {} {} {};",
        match style.padding.left {
            LengthPercentage::Length(val) => format!("{}px", val),
            _ => "0".to_string(),
        },
        match style.padding.top {
            LengthPercentage::Length(val) => format!("{}px", val),
            _ => "0".to_string(),
        },
        match style.padding.right {
            LengthPercentage::Length(val) => format!("{}px", val),
            _ => "0".to_string(),
        },
        match style.padding.bottom {
            LengthPercentage::Length(val) => format!("{}px", val),
            _ => "0".to_string(),
        }
    ));

    // Font size (if you have it in your style, adjust accordingly)
    // For example, if your font-size is stored somewhere in other_properties:
    // if let Some(font_size) = style.other_properties.get("font-size") { ... }

    css
}
