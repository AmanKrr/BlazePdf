use crate::dom::dom_tree::{ElementNode, Node};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};

/// ------------------------------
/// 1. Extended Selector Parsing
/// ------------------------------

/// Supported attribute selector operators.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeOperator {
    /// [attr="value"]
    Exact,
    /// [attr~="value"]
    Includes,
    /// [attr^="value"]
    Prefix,
    /// [attr$="value"]
    Suffix,
    /// [attr*="value"]
    Substring,
}

/// Represents one attribute condition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttributeSelector {
    pub name: String,
    pub operator: Option<AttributeOperator>, // None means only existence check
    pub value: Option<String>,
}

/// A compound selector now includes an optional tag, id, classes, and a list of attribute selectors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundSelector {
    pub tag: Option<String>,
    pub id: Option<String>,
    pub classes: HashSet<String>,
    pub attributes: Vec<AttributeSelector>,
}

/// A complex selector composed of a key compound selector and a list of ancestor parts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ComplexSelector {
    pub key: CompoundSelector,
    /// Ancestors with their combinators, in right-to-left order.
    pub ancestors: Vec<(Combinator, CompoundSelector)>,
}

/// Supported combinators.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Combinator {
    /// Descendant combinator (a space).
    Descendant,
    /// Child combinator (`>`).
    Child,
    /// Adjacent sibling combinator (`+`).
    AdjacentSibling,
    /// General sibling combinator (`~`).
    GeneralSibling,
}

/// A helper that returns a parsed selector; if parsing fails, returns a fallback.
pub fn parse_selector(selector: &str) -> ComplexSelector {
    parse_complex_selector(selector).unwrap_or_else(|| ComplexSelector {
        key: parse_compound_selector(selector),
        ancestors: Vec::new(),
    })
}

/// Parse a compound selector string, e.g. "div.red#header[disabled][data-type~=\"main\"]"
pub fn parse_compound_selector(selector: &str) -> CompoundSelector {
    let mut tag = None;
    let mut id = None;
    let mut classes = HashSet::new();
    let mut attributes = Vec::new();
    let mut chars = selector.chars().peekable();
    let mut buffer = String::new();

    // If first char is alphabetic or '*' assume tag.
    if let Some(&ch) = chars.peek() {
        if ch.is_alphabetic() || ch == '*' {
            while let Some(&ch) = chars.peek() {
                if ch == '#' || ch == '.' || ch == '[' {
                    break;
                }
                buffer.push(ch);
                chars.next();
            }
            if !buffer.is_empty() && buffer != "*" {
                tag = Some(buffer.clone());
            }
            buffer.clear();
        }
    }

    while let Some(ch) = chars.next() {
        match ch {
            '#' => {
                while let Some(&ch) = chars.peek() {
                    if ch == '.' || ch == '#' || ch == '[' {
                        break;
                    }
                    buffer.push(ch);
                    chars.next();
                }
                if !buffer.is_empty() {
                    id = Some(buffer.clone());
                }
                buffer.clear();
            }
            '.' => {
                while let Some(&ch) = chars.peek() {
                    if ch == '.' || ch == '#' || ch == '[' {
                        break;
                    }
                    buffer.push(ch);
                    chars.next();
                }
                if !buffer.is_empty() {
                    classes.insert(buffer.clone());
                }
                buffer.clear();
            }
            '[' => {
                // Parse attribute selector until ']'
                let mut attr_name = String::new();
                let mut operator: Option<AttributeOperator> = None;
                let mut attr_value: Option<String> = None;

                // Skip whitespace.
                while let Some(&ch) = chars.peek() {
                    if ch.is_whitespace() {
                        chars.next();
                    } else {
                        break;
                    }
                }
                // Read attribute name.
                while let Some(&ch) = chars.peek() {
                    if ch == '=' || ch == ']' || ch.is_whitespace() {
                        break;
                    }
                    attr_name.push(ch);
                    chars.next();
                }
                // Skip whitespace.
                while let Some(&ch) = chars.peek() {
                    if ch.is_whitespace() {
                        chars.next();
                    } else {
                        break;
                    }
                }
                // Check if an operator is present.
                if let Some(&ch) = chars.peek() {
                    if ch == '=' || ch == '~' || ch == '^' || ch == '$' || ch == '*' {
                        // If next two characters form an operator like "~=".
                        let mut op_str = String::new();
                        op_str.push(ch);
                        chars.next();
                        if let Some(&next_ch) = chars.peek() {
                            if next_ch == '=' {
                                op_str.push(next_ch);
                                chars.next();
                            }
                        }
                        operator = match op_str.as_str() {
                            "=" => Some(AttributeOperator::Exact),
                            "~=" => Some(AttributeOperator::Includes),
                            "^=" => Some(AttributeOperator::Prefix),
                            "$=" => Some(AttributeOperator::Suffix),
                            "*=" => Some(AttributeOperator::Substring),
                            _ => None,
                        };
                        // Skip whitespace.
                        while let Some(&ch) = chars.peek() {
                            if ch.is_whitespace() {
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        // Now parse attribute value.
                        let quote = if let Some(&ch) = chars.peek() {
                            if ch == '"' || ch == '\'' {
                                Some(ch)
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        if let Some(q) = quote {
                            chars.next(); // Consume opening quote.
                            let mut value_buf = String::new();
                            while let Some(ch) = chars.next() {
                                if ch == q {
                                    break;
                                }
                                value_buf.push(ch);
                            }
                            attr_value = Some(value_buf);
                        } else {
                            let mut value_buf = String::new();
                            while let Some(&ch) = chars.peek() {
                                if ch.is_whitespace() || ch == ']' {
                                    break;
                                }
                                value_buf.push(ch);
                                chars.next();
                            }
                            attr_value = Some(value_buf);
                        }
                    }
                }
                // Skip until ']'
                while let Some(ch) = chars.next() {
                    if ch == ']' {
                        break;
                    }
                }
                if !attr_name.is_empty() {
                    attributes.push(AttributeSelector {
                        name: attr_name,
                        operator,
                        value: attr_value,
                    });
                }
            }
            _ => {}
        }
    }

    CompoundSelector {
        tag,
        id,
        classes,
        attributes,
    }
}

/// Parse a complex selector string (e.g. "div.red > p#header + span.foo") into a ComplexSelector.
/// Assumes tokens are separated by whitespace.
pub fn parse_complex_selector(selector: &str) -> Option<ComplexSelector> {
    let tokens: Vec<&str> = selector.split_whitespace().collect();
    if tokens.is_empty() {
        return None;
    }
    let mut iter = tokens.into_iter();
    let mut key = parse_compound_selector(iter.next().unwrap());
    let mut ancestors = Vec::new();

    while let Some(token) = iter.next() {
        let combinator = match token {
            ">" => Combinator::Child,
            "+" => Combinator::AdjacentSibling,
            "~" => Combinator::GeneralSibling,
            _ => Combinator::Descendant,
        };
        let compound_token = if token == ">" || token == "+" || token == "~" {
            iter.next().unwrap_or(token)
        } else {
            token
        };
        ancestors.push((combinator, key));
        key = parse_compound_selector(compound_token);
    }
    ancestors.reverse();
    Some(ComplexSelector { key, ancestors })
}

/// ------------------------------
/// 2. Specificity, Merging & Inheritance
/// ------------------------------

/// Compute specificity for a compound selector as (id_count, class+attribute_count, tag_count)
pub fn compute_specificity(compound: &CompoundSelector) -> (u32, u32, u32) {
    let id_count = if compound.id.is_some() { 1 } else { 0 };
    let class_count = compound.classes.len() as u32;
    // Count each attribute as a class-level selector.
    let attr_count = compound.attributes.len() as u32;
    let tag_count = if compound.tag.is_some() { 1 } else { 0 };
    (id_count, class_count + attr_count, tag_count)
}

/// Compute specificity for a complex selector by summing key and ancestors.
pub fn compute_complex_specificity(selector: &ComplexSelector) -> (u32, u32, u32) {
    let mut spec = compute_specificity(&selector.key);
    for &(_, ref comp) in &selector.ancestors {
        let anc_spec = compute_specificity(comp);
        spec.0 += anc_spec.0;
        spec.1 += anc_spec.1;
        spec.2 += anc_spec.2;
    }
    spec
}

/// Represents a CSS rule.
#[derive(Debug, Clone)]
pub struct CssRule {
    pub selector: ComplexSelector,
    /// Declarations: property -> value
    pub declarations: HashMap<String, String>,
    pub source_order: u32,
}

/// Expand shorthand properties into longhand properties.
/// For this example, we expand margin, padding, and a simple border.
/// For border, we assume the shorthand is of the form "1px solid black"
/// and we expand it to border-width, border-style, and border-color.
fn expand_shorthand_properties(declarations: &mut HashMap<String, String>) {
    if let Some(margin_val) = declarations.remove("margin") {
        declarations.insert("margin-top".into(), margin_val.clone());
        declarations.insert("margin-right".into(), margin_val.clone());
        declarations.insert("margin-bottom".into(), margin_val.clone());
        declarations.insert("margin-left".into(), margin_val);
    }
    if let Some(padding_val) = declarations.remove("padding") {
        declarations.insert("padding-top".into(), padding_val.clone());
        declarations.insert("padding-right".into(), padding_val.clone());
        declarations.insert("padding-bottom".into(), padding_val.clone());
        declarations.insert("padding-left".into(), padding_val);
    }
    if let Some(border_val) = declarations.remove("border") {
        // This is a simplified expansion: split by whitespace.
        let parts: Vec<&str> = border_val.split_whitespace().collect();
        if parts.len() >= 3 {
            declarations.insert("border-width".into(), parts[0].to_string());
            declarations.insert("border-style".into(), parts[1].to_string());
            declarations.insert("border-color".into(), parts[2].to_string());
        } else {
            // Fallback: assign the same value to all.
            declarations.insert("border-width".into(), border_val.clone());
            declarations.insert("border-style".into(), border_val.clone());
            declarations.insert("border-color".into(), border_val);
        }
    }
}

/// Compute the final computed style for an element by merging matched rules,
/// sorting by specificity and source order, expanding shorthands, and then applying inheritance.
/// `parent_style` is the computed style of the parent, if any.
pub fn compute_computed_style(
    matched_rules: Vec<CssRule>,
    parent_style: Option<&HashMap<String, String>>,
) -> HashMap<String, String> {
    let mut rules = matched_rules;
    rules.sort_by(|a, b| {
        let spec_a = compute_complex_specificity(&a.selector);
        let spec_b = compute_complex_specificity(&b.selector);
        let cmp_spec = spec_a.cmp(&spec_b);
        if cmp_spec == Ordering::Equal {
            a.source_order.cmp(&b.source_order)
        } else {
            cmp_spec
        }
    });
    let mut computed: HashMap<String, String> = HashMap::new();
    for mut rule in rules {
        expand_shorthand_properties(&mut rule.declarations);
        for (prop, value) in rule.declarations {
            computed.insert(prop, value);
        }
    }
    // Extend the inheritable properties list.
    let inheritable = [
        "color",
        "font-size",
        "font-family",
        "line-height",
        "font-weight",
        "text-align",
        "visibility",
        "cursor",
        "letter-spacing",
        "word-spacing",
        "direction",
    ];
    if let Some(parent) = parent_style {
        for prop in inheritable.iter() {
            if !computed.contains_key(*prop) {
                if let Some(val) = parent.get(*prop) {
                    computed.insert((*prop).to_string(), val.clone());
                }
            }
        }
    }
    computed
}

/// ------------------------------
/// 3. Selector Matching
/// ------------------------------

/// Returns true if the given ElementNode matches the CompoundSelector.
/// Checks tag, id, classes, and attribute conditions.
pub fn matches_compound(elem: &ElementNode, compound: &CompoundSelector) -> bool {
    if let Some(ref tag) = compound.tag {
        if !elem.tag.eq_ignore_ascii_case(tag) {
            return false;
        }
    }
    if let Some(ref id_val) = compound.id {
        if let Some(elem_id) = elem.attributes.get("id") {
            if elem_id != id_val {
                return false;
            }
        } else {
            return false;
        }
    }
    if !compound.classes.is_empty() {
        if let Some(class_attr) = elem.attributes.get("class") {
            let elem_classes: HashSet<String> = class_attr
                .split_whitespace()
                .map(|s| s.to_string())
                .collect();
            if !compound.classes.is_subset(&elem_classes) {
                return false;
            }
        } else {
            return false;
        }
    }
    // Check attribute selectors.
    for attr_sel in &compound.attributes {
        if let Some(actual_val) = elem.attributes.get(&attr_sel.name) {
            if let Some(expected) = &attr_sel.value {
                // Depending on the operator, perform different comparisons.
                match attr_sel.operator {
                    Some(AttributeOperator::Exact) => {
                        if actual_val != expected {
                            return false;
                        }
                    }
                    Some(AttributeOperator::Includes) => {
                        // Treat actual value as space-separated words.
                        let words: HashSet<String> = actual_val
                            .split_whitespace()
                            .map(|s| s.to_string())
                            .collect();
                        if !words.contains(expected) {
                            return false;
                        }
                    }
                    Some(AttributeOperator::Prefix) => {
                        if !actual_val.starts_with(expected) {
                            return false;
                        }
                    }
                    Some(AttributeOperator::Suffix) => {
                        if !actual_val.ends_with(expected) {
                            return false;
                        }
                    }
                    Some(AttributeOperator::Substring) => {
                        if !actual_val.contains(expected) {
                            return false;
                        }
                    }
                    None => {} // No operator means just existence; already confirmed.
                }
            }
            // If no expected value provided, existence is enough.
        } else {
            return false;
        }
    }
    true
}

/// Matches a ComplexSelector against a candidate Element (wrapped in Rc<RefCell<Node>>).
/// The matching proceeds right-to-left, using parent and sibling pointers.
pub fn matches_complex_selector(candidate: &Rc<RefCell<Node>>, complex: &ComplexSelector) -> bool {
    let current_elem = {
        let node = candidate.borrow();
        match &*node {
            crate::dom::dom_tree::Node::Element(elem) => elem.clone(),
            _ => return false,
        }
    };
    if !matches_compound(&current_elem, &complex.key) {
        return false;
    }
    let mut current_node = Rc::clone(candidate);
    for (combinator, compound) in &complex.ancestors {
        let found = match combinator {
            Combinator::Child => {
                if let Some(parent_weak) = get_parent(&current_node) {
                    if let Some(parent_rc) = parent_weak.upgrade() {
                        if let crate::dom::dom_tree::Node::Element(ref parent_elem) =
                            *parent_rc.borrow()
                        {
                            if matches_compound(parent_elem, compound) {
                                current_node = Rc::clone(&parent_rc);
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Combinator::Descendant => {
                let mut ancestor = get_parent(&current_node);
                let mut matched = false;
                while let Some(weak) = ancestor {
                    if let Some(ancestor_rc) = weak.upgrade() {
                        if let crate::dom::dom_tree::Node::Element(ref ancestor_elem) =
                            *ancestor_rc.borrow()
                        {
                            if matches_compound(ancestor_elem, compound) {
                                current_node = Rc::clone(&ancestor_rc);
                                matched = true;
                                break;
                            }
                        }
                        ancestor = get_parent(&ancestor_rc);
                    } else {
                        break;
                    }
                }
                matched
            }
            Combinator::AdjacentSibling => {
                if let Some(sibling_rc) = get_prev_sibling(&current_node) {
                    if let crate::dom::dom_tree::Node::Element(ref sibling_elem) =
                        *sibling_rc.borrow()
                    {
                        if matches_compound(sibling_elem, compound) {
                            current_node = Rc::clone(&sibling_rc);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Combinator::GeneralSibling => {
                let siblings = get_all_prev_siblings(&current_node);
                if let Some(sibling_rc) = siblings.into_iter().find(|s| {
                    if let crate::dom::dom_tree::Node::Element(ref sibling_elem) = *s.borrow() {
                        matches_compound(sibling_elem, compound)
                    } else {
                        false
                    }
                }) {
                    current_node = sibling_rc;
                    true
                } else {
                    false
                }
            }
        };
        if !found {
            return false;
        }
    }
    true
}

/// Helper: get parent pointer from a node.
fn get_parent(
    node: &Rc<RefCell<crate::dom::dom_tree::Node>>,
) -> Option<Weak<RefCell<crate::dom::dom_tree::Node>>> {
    if let crate::dom::dom_tree::Node::Element(ref elem) = *node.borrow() {
        elem.parent.clone()
    } else {
        None
    }
}

/// Helper: get immediate previous sibling from a node.
fn get_prev_sibling(
    node: &Rc<RefCell<crate::dom::dom_tree::Node>>,
) -> Option<Rc<RefCell<crate::dom::dom_tree::Node>>> {
    if let crate::dom::dom_tree::Node::Element(ref elem) = *node.borrow() {
        if let Some(weak) = &elem.prev_sibling {
            weak.upgrade()
        } else {
            None
        }
    } else {
        None
    }
}

/// Helper: get all previous siblings from a node.
fn get_all_prev_siblings(
    node: &Rc<RefCell<crate::dom::dom_tree::Node>>,
) -> Vec<Rc<RefCell<crate::dom::dom_tree::Node>>> {
    let mut siblings = Vec::new();
    let mut current = get_prev_sibling(node);
    while let Some(sib) = current {
        siblings.push(Rc::clone(&sib));
        current = get_prev_sibling(&sib);
    }
    siblings
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{HashMap, HashSet};

    // Helper to create a CompoundSelector easily.
    fn make_compound(tag: Option<&str>, id: Option<&str>, classes: &[&str]) -> CompoundSelector {
        let tag = tag.map(|t| t.to_string());
        let id = id.map(|i| i.to_string());
        let classes: HashSet<String> = classes.iter().map(|s| s.to_string()).collect();
        CompoundSelector {
            tag,
            id,
            classes,
            attributes: vec![],
        }
    }

    // Helper to create a ComplexSelector from a CompoundSelector.
    fn make_complex(compound: CompoundSelector) -> ComplexSelector {
        ComplexSelector {
            key: compound,
            ancestors: vec![],
        }
    }

    // Test that a rule with higher specificity (ID selector) wins.
    #[test]
    fn test_specificity_wins() {
        // Rule 1: "div" => specificity (0,0,1) with color blue.
        let comp1 = make_compound(Some("div"), None, &[]);
        let complex1 = make_complex(comp1);
        let mut decls1 = HashMap::new();
        decls1.insert("color".to_string(), "blue".to_string());
        let rule1 = CssRule {
            selector: complex1,
            declarations: decls1,
            source_order: 1,
        };

        // Rule 2: "#blue" => specificity (1,0,0) with color green.
        let comp2 = make_compound(None, Some("blue"), &[]);
        let complex2 = make_complex(comp2);
        let mut decls2 = HashMap::new();
        decls2.insert("color".to_string(), "green".to_string());
        let rule2 = CssRule {
            selector: complex2,
            declarations: decls2,
            source_order: 2,
        };

        // When merged, the higher specificity rule (#blue) should win.
        let computed = compute_computed_style(vec![rule1, rule2], None);
        assert_eq!(computed.get("color"), Some(&"green".to_string()));
    }

    // Test that when specificity is equal, source order wins.
    #[test]
    fn test_source_order() {
        // Both rules with the same specificity.
        let comp = make_compound(Some("p"), None, &[]);
        let complex = make_complex(comp);
        let mut decls_a = HashMap::new();
        decls_a.insert("font-size".to_string(), "12px".to_string());
        let rule_a = CssRule {
            selector: complex.clone(),
            declarations: decls_a,
            source_order: 1,
        };

        let mut decls_b = HashMap::new();
        decls_b.insert("font-size".to_string(), "14px".to_string());
        let rule_b = CssRule {
            selector: complex,
            declarations: decls_b,
            source_order: 2,
        };

        let computed = compute_computed_style(vec![rule_a, rule_b], None);
        // Since specificity is equal, the later (higher source_order) wins.
        assert_eq!(computed.get("font-size"), Some(&"14px".to_string()));
    }

    // Test inheritance: if a property is inheritable and not defined on the child, it should come from the parent.
    #[test]
    fn test_inheritance() {
        let child_rules: Vec<CssRule> = vec![]; // No rules for child.
        let parent_style: HashMap<String, String> = [("color".to_string(), "red".to_string())]
            .iter()
            .cloned()
            .collect();
        let computed = compute_computed_style(child_rules, Some(&parent_style));
        assert_eq!(computed.get("color"), Some(&"red".to_string()));
    }

    // Test shorthand expansion for margin.
    #[test]
    fn test_shorthand_margin_expansion() {
        let comp = make_compound(Some("div"), None, &[]);
        let complex = make_complex(comp);
        let mut decls = HashMap::new();
        decls.insert("margin".to_string(), "10px".to_string());
        let rule = CssRule {
            selector: complex,
            declarations: decls,
            source_order: 1,
        };
        let computed = compute_computed_style(vec![rule], None);
        assert_eq!(computed.get("margin-top"), Some(&"10px".to_string()));
        assert_eq!(computed.get("margin-right"), Some(&"10px".to_string()));
        assert_eq!(computed.get("margin-bottom"), Some(&"10px".to_string()));
        assert_eq!(computed.get("margin-left"), Some(&"10px".to_string()));
    }
}
