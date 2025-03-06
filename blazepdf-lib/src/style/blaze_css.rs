use crate::dom::dom_tree::{Document, ElementNode, Node};
use crate::parser::dom_indices::DomIndices;
use crate::style::css_matcher;
use crate::style::owned_css::{OwnedDeclaration, OwnedRule, OwnedStylesheet};
use lightningcss::error::{Error as LcssError, ParserError};
use lightningcss::printer::PrinterOptions;
use lightningcss::rules::{style::StyleRule, CssRule};
use lightningcss::stylesheet::{ParserOptions, StyleSheet as LightningStyleSheet};
use lightningcss::traits::ToCss;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use std::hash::{Hash, Hasher};

/// A newtype wrapper for Rc<RefCell<Node>> that implements Hash and Eq
/// based on pointer identity.
#[derive(Clone)]
struct NodeWrapper(Rc<RefCell<Node>>);

impl PartialEq for NodeWrapper {
    fn eq(&self, other: &Self) -> bool {
        // Use Rc::ptr_eq to compare if they point to the same allocation.
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for NodeWrapper {}

impl Hash for NodeWrapper {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash the raw pointer (converted to usize).
        (Rc::as_ptr(&self.0) as usize).hash(state)
    }
}

/// Represents computed CSS properties for an element.
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

/// Top-level integration function.
/// Given a CSS snippet and a DOM document, it parses the CSS, builds global indexes,
/// and applies computed styles to matching elements in the DOM.
pub fn parse(css_snippet: &str, dom_document: &Document) {
    // First, parse and own the CSS.
    let owned_sheet = parse_and_own_css(css_snippet).expect("Failed to parse & own CSS");
    // Build the global DOM indices.
    let indices = DomIndices::build(dom_document);
    // Apply CSS rules using the indices.
    apply_css_with_indices(dom_document, &owned_sheet, &indices);
}

/// Parse a raw CSS string using LightningCSS and convert it to an OwnedStylesheet.
fn parse_and_own_css(css_text: &str) -> Result<OwnedStylesheet, ParserError<'_>> {
    let parser_opts = ParserOptions::default();
    let sheet = LightningStyleSheet::parse(css_text, parser_opts)
        .map_err(|e: LcssError<ParserError<'_>>| e.kind)?;
    let mut owned_rules = Vec::new();
    for rule in &sheet.rules.0 {
        match rule {
            CssRule::Style(style_rule) => {
                let owned = convert_style_rule(style_rule);
                owned_rules.push(owned);
            }
            CssRule::Media(media_rule) => {
                for inner_rule in &media_rule.rules.0 {
                    if let CssRule::Style(sr) = inner_rule {
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

/// Apply CSS rules to the DOM using global indices.
/// This function iterates over each rule and each selector in the rule,
/// uses the indices to quickly narrow candidate nodes,
/// and then runs full matching before merging declarations.
pub fn apply_css_with_indices(
    document: &Document,
    stylesheet: &OwnedStylesheet,
    indices: &DomIndices,
) {
    // A temporary map to collect computed styles for nodes.
    let mut computed_styles: HashMap<*const RefCell<Node>, ComputedStyle> = HashMap::new();

    for rule in &stylesheet.rules {
        for sel_str in &rule.selectors {
            // Parse the selector into a complex selector.
            let complex_sel = css_matcher::parse_selector(sel_str);
            // Use the key (the right-most compound) to narrow candidates via indices.
            let candidates = find_candidates_for_selector(&complex_sel, indices);
            for candidate in candidates {
                if css_matcher::matches_complex_selector(&candidate, &complex_sel) {
                    let key = Rc::as_ptr(&candidate);
                    let style = computed_styles
                        .entry(key)
                        .or_insert_with(ComputedStyle::new);
                    for decl in &rule.declarations {
                        style
                            .properties
                            .insert(decl.property.clone(), decl.value.clone());
                    }
                }
            }
        }
    }
    // Inject computed styles into the DOM.
    inject_computed_styles(&document.root, &computed_styles);
}

/// Find candidate nodes for a selector using global indices.
fn find_candidates_for_selector(
    complex: &css_matcher::ComplexSelector,
    indices: &DomIndices,
) -> Vec<Rc<RefCell<Node>>> {
    let key = &complex.key;
    let mut candidate_set: Option<std::collections::HashSet<NodeWrapper>> = None;
    // If id is specified, look it up.
    if let Some(ref id) = key.id {
        if let Some(node) = indices.id_map.get(id) {
            let mut set = std::collections::HashSet::new();
            set.insert(NodeWrapper(Rc::clone(node)));
            candidate_set = Some(set);
        } else {
            return vec![];
        }
    }
    // Intersect with candidates for each class.
    for class in &key.classes {
        if let Some(nodes) = indices.class_map.get(class) {
            let set: std::collections::HashSet<_> =
                nodes.iter().map(|rc| NodeWrapper(Rc::clone(rc))).collect();
            candidate_set = match candidate_set {
                Some(prev) => Some(prev.intersection(&set).cloned().collect()),
                None => Some(set),
            };
        } else {
            return vec![];
        }
    }
    // Intersect with candidates for tag.
    if let Some(ref tag) = key.tag {
        if let Some(nodes) = indices.tag_map.get(&tag.to_lowercase()) {
            let set: std::collections::HashSet<_> =
                nodes.iter().map(|rc| NodeWrapper(Rc::clone(rc))).collect();
            candidate_set = match candidate_set {
                Some(prev) => Some(prev.intersection(&set).cloned().collect()),
                None => Some(set),
            };
        } else {
            return vec![];
        }
    }
    // Convert the final HashSet<NodeWrapper> back to Vec<Rc<RefCell<Node>>>
    candidate_set.map_or_else(Vec::new, |s| s.into_iter().map(|nw| nw.0).collect())
}

/// Recursively traverse the DOM and inject computed styles (as inline "style" attribute).
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
                let mut style_vec = Vec::new();
                for (k, v) in &style.properties {
                    style_vec.push(format!("{}: {}", k, v));
                }
                if !style_vec.is_empty() {
                    if elem.attributes.contains_key("style") {
                        let existing = elem.attributes.get("style").unwrap();
                        style_vec.insert(0, existing.replace(";", "").clone());
                    }
                    let mut inline_style = style_vec.join("; ");
                    inline_style.push_str(";");
                    elem.attributes.insert("style".to_string(), inline_style);
                }
            }
            for child in &elem.children {
                inject_computed_styles(child, styles);
            }
        }
        Node::Text(_) => {}
    }
}
