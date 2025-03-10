use crate::dom::dom_tree::{ElementNode, Node};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};
use taffy::prelude::*;

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

/// Pseudo‑classes that we support.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PseudoClass {
    FirstChild,
    LastChild,
    NthChild(i32),
    // In production you might add: nth-last-child, only-child, etc.
}

/// A compound selector now includes an optional tag, id, classes, and a list of attribute selectors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundSelector {
    pub tag: Option<String>,
    pub id: Option<String>,
    pub classes: HashSet<String>,
    pub attributes: Vec<AttributeSelector>,
    pub pseudo: Vec<PseudoClass>,
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
    let mut pseudo = Vec::new();
    let mut chars = selector.chars().peekable();
    let mut buffer = String::new();

    // If the first character is alphabetic or '*', assume a tag.
    if let Some(&ch) = chars.peek() {
        if ch.is_alphabetic() || ch == '*' {
            while let Some(&ch) = chars.peek() {
                if ch == '#' || ch == '.' || ch == '[' || ch == ':' {
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

    // Process the rest of the selector.
    while let Some(ch) = chars.next() {
        match ch {
            '#' => {
                while let Some(&ch) = chars.peek() {
                    if ch == '.' || ch == '#' || ch == '[' || ch == ':' {
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
                    if ch == '.' || ch == '#' || ch == '[' || ch == ':' {
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
                // Parse an attribute selector.
                let mut attr_name = String::new();
                let mut operator: Option<AttributeOperator> = None;
                let mut attr_value: Option<String> = None;

                // Skip any whitespace.
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
                // Check for an operator.
                if let Some(&ch) = chars.peek() {
                    if ch == '=' || ch == '~' || ch == '^' || ch == '$' || ch == '*' {
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
                        // Parse the attribute value.
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
                            chars.next(); // consume the quote
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
                // Consume until the closing ']'
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
            ':' => {
                // Parse a pseudo‑class.
                let mut pseudo_buf = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '-' {
                        pseudo_buf.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if pseudo_buf.eq_ignore_ascii_case("first-child") {
                    pseudo.push(PseudoClass::FirstChild);
                } else if pseudo_buf.eq_ignore_ascii_case("last-child") {
                    pseudo.push(PseudoClass::LastChild);
                } else if pseudo_buf.eq_ignore_ascii_case("nth-child") {
                    if let Some(&'(') = chars.peek() {
                        chars.next(); // consume '('
                        let mut num_buf = String::new();
                        while let Some(&c) = chars.peek() {
                            if c.is_digit(10) || c == '-' {
                                num_buf.push(c);
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        if let Some(&')') = chars.peek() {
                            chars.next(); // consume ')'
                        }
                        if let Ok(n) = num_buf.parse::<i32>() {
                            pseudo.push(PseudoClass::NthChild(n));
                        }
                    }
                }
            }
            _ => {
                // Skip any other characters (e.g. whitespace).
            }
        }
    }

    CompoundSelector {
        tag,
        id,
        classes,
        attributes,
        pseudo,
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
#[derive(Default, Clone, Debug)]
pub struct ComputedStyle {
    pub taffy_style: Style,
    pub other_properties: HashMap<String, String>,
}

#[allow(dead_code)]
impl ComputedStyle {
    pub fn new() -> Self {
        ComputedStyle {
            taffy_style: Style::default(),
            other_properties: HashMap::new(),
        }
    }
}

pub fn compute_computed_style(
    mut matched_rules: Vec<CssRule>,
    parent_style: Option<&ComputedStyle>,
) -> ComputedStyle {
    // Start with parent's style if available, otherwise use default.
    // let mut final_style = parent_style.cloned().unwrap_or_default();

    let mut final_style = ComputedStyle {
        taffy_style: parent_style
            .map(|p| p.taffy_style.clone())
            .unwrap_or_default(),
        other_properties: parent_style
            .map(|p| p.other_properties.clone())
            .unwrap_or_default(),
    };

    // Sort the matched rules by specificity then source order.
    matched_rules.sort_by(|a, b| {
        let spec_a = compute_complex_specificity(&a.selector);
        let spec_b = compute_complex_specificity(&b.selector);
        let cmp_spec = spec_a.cmp(&spec_b);
        if cmp_spec == Ordering::Equal {
            a.source_order.cmp(&b.source_order)
        } else {
            cmp_spec
        }
    });

    // We'll also keep track of typography properties that we don’t directly support in Taffy.
    let mut typography: HashMap<String, String> = HashMap::new();

    // Iterate over each rule.
    for rule in matched_rules {
        // Expand shorthand properties (for margin, padding, border, etc.).
        let mut decls = rule.declarations;
        expand_shorthand_properties(&mut decls);

        // For each property, update the final_style.
        for (prop, value) in decls {
            match prop.as_str() {
                // --- Display and Visibility ---
                "display" => {
                    final_style.taffy_style.display = match value.to_lowercase().as_str() {
                        "block" => Display::Block,
                        "flex" => Display::Flex,
                        "grid" => Display::Grid,
                        // "inline" => Display::Inline,
                        "none" => Display::None,
                        _ => final_style.taffy_style.display,
                    }
                }
                // --- Dimensions ---
                "width" => {
                    final_style.taffy_style.size.width = parse_dimension(&value);
                }
                "height" => {
                    final_style.taffy_style.size.height = parse_dimension(&value);
                }
                "min-width" => {
                    final_style.taffy_style.min_size.width = parse_dimension(&value);
                }
                "min-height" => {
                    final_style.taffy_style.min_size.height = parse_dimension(&value);
                }
                "max-width" => {
                    final_style.taffy_style.max_size.width = parse_dimension(&value);
                }
                "max-height" => {
                    final_style.taffy_style.max_size.height = parse_dimension(&value);
                }
                // --- Spacing ---
                "margin-top" => {
                    final_style.taffy_style.margin.top = parse_margin_position_dimension(&value);
                }
                "margin-right" => {
                    final_style.taffy_style.margin.right = parse_margin_position_dimension(&value);
                }
                "margin-bottom" => {
                    final_style.taffy_style.margin.bottom = parse_margin_position_dimension(&value);
                }
                "margin-left" => {
                    final_style.taffy_style.margin.left = parse_margin_position_dimension(&value);
                }
                "padding-top" => {
                    final_style.taffy_style.padding.top = parse_padding_dimension(&value);
                }
                "padding-right" => {
                    final_style.taffy_style.padding.right = parse_padding_dimension(&value);
                }
                "padding-bottom" => {
                    final_style.taffy_style.padding.bottom = parse_padding_dimension(&value);
                }
                "padding-left" => {
                    final_style.taffy_style.padding.left = parse_padding_dimension(&value);
                }
                // --- Positioning ---
                "position" => {
                    final_style.taffy_style.position = match value.to_lowercase().as_str() {
                        "absolute" => Position::Absolute,
                        "relative" => Position::Relative,
                        // "fixed" => Position::Fixed,
                        _ => final_style.taffy_style.position,
                    }
                }
                "top" => {
                    final_style.taffy_style.inset.top = parse_margin_position_dimension(&value);
                }
                "right" => {
                    final_style.taffy_style.inset.right = parse_margin_position_dimension(&value);
                }
                "bottom" => {
                    final_style.taffy_style.inset.bottom = parse_margin_position_dimension(&value);
                }
                "left" => {
                    final_style.taffy_style.inset.left = parse_margin_position_dimension(&value);
                }
                // --- Flexbox Properties ---
                "flex-direction" => {
                    final_style.taffy_style.flex_direction = match value.to_lowercase().as_str() {
                        "row" => FlexDirection::Row,
                        "row-reverse" => FlexDirection::RowReverse,
                        "column" => FlexDirection::Column,
                        "column-reverse" => FlexDirection::ColumnReverse,
                        _ => final_style.taffy_style.flex_direction,
                    }
                }
                "flex-wrap" => {
                    final_style.taffy_style.flex_wrap = match value.to_lowercase().as_str() {
                        "nowrap" => FlexWrap::NoWrap,
                        "wrap" => FlexWrap::Wrap,
                        "wrap-reverse" => FlexWrap::WrapReverse,
                        _ => final_style.taffy_style.flex_wrap,
                    }
                }
                "justify-content" => {
                    final_style.taffy_style.justify_content = match value.to_lowercase().as_str() {
                        "flex-start" => Some(JustifyContent::FlexStart),
                        "center" => Some(JustifyContent::Center),
                        "flex-end" => Some(JustifyContent::FlexEnd),
                        "space-between" => Some(JustifyContent::SpaceBetween),
                        "space-around" => Some(JustifyContent::SpaceAround),
                        _ => final_style.taffy_style.justify_content,
                    }
                }
                "align-items" => {
                    final_style.taffy_style.align_items = match value.to_lowercase().as_str() {
                        "flex-start" => Some(AlignItems::FlexStart),
                        "center" => Some(AlignItems::Center),
                        "flex-end" => Some(AlignItems::FlexEnd),
                        "stretch" => Some(AlignItems::Stretch),
                        _ => final_style.taffy_style.align_items,
                    }
                }
                "flex-grow" => {
                    if let Ok(val) = value.parse::<f32>() {
                        final_style.taffy_style.flex_grow = val;
                    }
                }
                "flex-shrink" => {
                    if let Ok(val) = value.parse::<f32>() {
                        final_style.taffy_style.flex_shrink = val;
                    }
                }
                "flex-basis" => {
                    final_style.taffy_style.flex_basis = parse_dimension(&value);
                }
                // --- Typography (if needed for text layout) ---
                "font-size" => {
                    // Convert font-size (assumed in px or percentage) to a Dimension.
                    // For now we only handle px and auto.
                    // You might also want to update a separate property for text rendering.
                    // Here we simply update our other_properties.
                    typography.insert("font-size".into(), value);
                }
                "font-family" => {
                    typography.insert("font-family".into(), value);
                }
                "font-weight" => {
                    typography.insert("font-weight".into(), value);
                }
                "line-height" => {
                    typography.insert("line-height".into(), value);
                }
                "letter-spacing" => {
                    typography.insert("letter-spacing".into(), value);
                }
                "word-spacing" => {
                    typography.insert("word-spacing".into(), value);
                }
                "text-align" => {
                    typography.insert("text-align".into(), value);
                }
                "white-space" => {
                    typography.insert("white-space".into(), value);
                }
                _ => {
                    // For any unhandled property, you could log or ignore it.
                }
            }
        }
    }

    // Finally, you might decide to merge typography properties into final_style if needed.
    // For this example, we simply store them as a string in an "other_properties" map.
    for (prop, val) in typography {
        final_style.other_properties.insert(prop, val);
    }

    final_style
}

/// Helper: Parse a CSS Padding dimension string.
fn parse_padding_dimension(value: &str) -> LengthPercentage {
    let value = value.trim();
    if value.ends_with("px") {
        if let Ok(val) = value.trim_end_matches("px").trim().parse::<f32>() {
            LengthPercentage::Length(val)
        } else {
            LengthPercentage::Length(0.0)
        }
    } else if value.ends_with('%') {
        if let Ok(val) = value.trim_end_matches('%').trim().parse::<f32>() {
            LengthPercentage::Percent(val)
        } else {
            LengthPercentage::Length(0.0)
        }
    } else {
        // Fallback: try parsing as pixels.
        if let Ok(val) = value.parse::<f32>() {
            LengthPercentage::Length(val)
        } else {
            LengthPercentage::Length(0.0)
        }
    }
}

/// Helper: Parse a CSS dimension string (like "100px", "50%") into a Taffy Dimension.
fn parse_margin_position_dimension(value: &str) -> LengthPercentageAuto {
    let value = value.trim();
    if value.eq_ignore_ascii_case("auto") {
        LengthPercentageAuto::Auto
    } else if value.ends_with("px") {
        if let Ok(val) = value.trim_end_matches("px").trim().parse::<f32>() {
            LengthPercentageAuto::Length(val)
        } else {
            LengthPercentageAuto::Auto
        }
    } else if value.ends_with('%') {
        if let Ok(val) = value.trim_end_matches('%').trim().parse::<f32>() {
            LengthPercentageAuto::Percent(val)
        } else {
            LengthPercentageAuto::Auto
        }
    } else {
        // Fallback: try parsing as pixels.
        if let Ok(val) = value.parse::<f32>() {
            LengthPercentageAuto::Length(val)
        } else {
            LengthPercentageAuto::Auto
        }
    }
}

/// Helper: Parse a CSS dimension string specifically for padding (no 'auto' allowed).
fn parse_dimension(value: &str) -> Dimension {
    let value = value.trim();
    if value.eq_ignore_ascii_case("auto") {
        Dimension::Auto
    } else if value.ends_with("px") {
        if let Ok(val) = value.trim_end_matches("px").trim().parse::<f32>() {
            Dimension::Length(val)
        } else {
            Dimension::Auto
        }
    } else if value.ends_with('%') {
        if let Ok(val) = value.trim_end_matches('%').trim().parse::<f32>() {
            Dimension::Percent(val / 100.0)
        } else {
            Dimension::Auto
        }
    } else {
        // Fallback: try parsing as pixels.
        if let Ok(val) = value.parse::<f32>() {
            Dimension::Length(val)
        } else {
            Dimension::Auto
        }
    }
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

/// Checks whether the candidate node satisfies all the pseudo‑classes in the slice.
fn matches_pseudo(candidate: &Rc<RefCell<Node>>, pseudo: &[PseudoClass]) -> bool {
    for pseudo_class in pseudo {
        match pseudo_class {
            PseudoClass::FirstChild => {
                if let Some(parent_weak) = get_parent(candidate) {
                    if let Some(parent_rc) = parent_weak.upgrade() {
                        let parent = parent_rc.borrow();
                        let children = match &*parent {
                            Node::DocumentRoot(root) => &root.children,
                            Node::Element(elem) => &elem.children,
                            _ => return false,
                        };
                        if let Some(first) = children
                            .iter()
                            .find(|child| matches!(*child.borrow(), Node::Element(_)))
                        {
                            if !Rc::ptr_eq(first, candidate) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            PseudoClass::LastChild => {
                if let Some(parent_weak) = get_parent(candidate) {
                    if let Some(parent_rc) = parent_weak.upgrade() {
                        let parent = parent_rc.borrow();
                        let children = match &*parent {
                            Node::DocumentRoot(root) => &root.children,
                            Node::Element(elem) => &elem.children,
                            _ => return false,
                        };
                        if let Some(last) = children
                            .iter()
                            .rev()
                            .find(|child| matches!(*child.borrow(), Node::Element(_)))
                        {
                            if !Rc::ptr_eq(last, candidate) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            PseudoClass::NthChild(n) => {
                if let Some(parent_weak) = get_parent(candidate) {
                    if let Some(parent_rc) = parent_weak.upgrade() {
                        let parent = parent_rc.borrow();
                        let children = match &*parent {
                            Node::DocumentRoot(root) => &root.children,
                            Node::Element(elem) => &elem.children,
                            _ => return false,
                        };
                        let mut index = 0;
                        let mut found = false;
                        for child in children {
                            if let Node::Element(_) = *child.borrow() {
                                index += 1;
                                if Rc::ptr_eq(child, candidate) {
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if !found || index != *n as usize {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
    }
    true
}

/// Matches a ComplexSelector against a candidate Element (wrapped in Rc<RefCell<Node>>).
/// The matching proceeds right-to-left, using parent and sibling pointers.
pub fn matches_complex_selector(candidate: &Rc<RefCell<Node>>, complex: &ComplexSelector) -> bool {
    // First, check the candidate against the key compound selector.
    let current_elem = {
        let node = candidate.borrow();
        match &*node {
            Node::Element(elem) => elem.clone(),
            _ => return false,
        }
    };
    if !matches_compound(&current_elem, &complex.key) {
        return false;
    }
    if !matches_pseudo(candidate, &complex.key.pseudo) {
        return false;
    }

    // Process ancestor selectors in right-to-left order.
    let mut current_node = Rc::clone(candidate);
    for (combinator, compound) in &complex.ancestors {
        let found = match combinator {
            Combinator::Child => {
                if let Some(parent_weak) = get_parent(&current_node) {
                    if let Some(parent_rc) = parent_weak.upgrade() {
                        if let Node::Element(ref parent_elem) = *parent_rc.borrow() {
                            if matches_compound(parent_elem, compound)
                                && matches_pseudo(&parent_rc, &compound.pseudo)
                            {
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
                        if let Node::Element(ref ancestor_elem) = *ancestor_rc.borrow() {
                            if matches_compound(ancestor_elem, compound)
                                && matches_pseudo(&ancestor_rc, &compound.pseudo)
                            {
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
                    if let Node::Element(ref sibling_elem) = *sibling_rc.borrow() {
                        if matches_compound(sibling_elem, compound)
                            && matches_pseudo(&sibling_rc, &compound.pseudo)
                        {
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
                    if let Node::Element(ref sibling_elem) = *s.borrow() {
                        matches_compound(sibling_elem, compound)
                            && matches_pseudo(s, &compound.pseudo)
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

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use std::collections::{HashMap, HashSet};

//     // Helper to create a CompoundSelector easily.
//     fn make_compound(tag: Option<&str>, id: Option<&str>, classes: &[&str]) -> CompoundSelector {
//         let tag = tag.map(|t| t.to_string());
//         let id = id.map(|i| i.to_string());
//         let classes: HashSet<String> = classes.iter().map(|s| s.to_string()).collect();
//         CompoundSelector {
//             tag,
//             id,
//             classes,
//             attributes: vec![],
//             pseudo: vec![],
//         }
//     }

//     // Helper to create a ComplexSelector from a CompoundSelector.
//     fn make_complex(compound: CompoundSelector) -> ComplexSelector {
//         ComplexSelector {
//             key: compound,
//             ancestors: vec![],
//         }
//     }

//     // Test that a rule with higher specificity (ID selector) wins.
//     #[test]
//     fn test_specificity_wins() {
//         // Rule 1: "div" => specificity (0,0,1) with color blue.
//         let comp1 = make_compound(Some("div"), None, &[]);
//         let complex1 = make_complex(comp1);
//         let mut decls1 = HashMap::new();
//         decls1.insert("color".to_string(), "blue".to_string());
//         let rule1 = CssRule {
//             selector: complex1,
//             declarations: decls1,
//             source_order: 1,
//         };

//         // Rule 2: "#blue" => specificity (1,0,0) with color green.
//         let comp2 = make_compound(None, Some("blue"), &[]);
//         let complex2 = make_complex(comp2);
//         let mut decls2 = HashMap::new();
//         decls2.insert("color".to_string(), "green".to_string());
//         let rule2 = CssRule {
//             selector: complex2,
//             declarations: decls2,
//             source_order: 2,
//         };

//         // When merged, the higher specificity rule (#blue) should win.
//         let computed = compute_computed_style(vec![rule1, rule2], None);
//         assert_eq!(computed.get("color"), Some(&"green".to_string()));
//     }

//     // Test that when specificity is equal, source order wins.
//     #[test]
//     fn test_source_order() {
//         // Both rules with the same specificity.
//         let comp = make_compound(Some("p"), None, &[]);
//         let complex = make_complex(comp);
//         let mut decls_a = HashMap::new();
//         decls_a.insert("font-size".to_string(), "12px".to_string());
//         let rule_a = CssRule {
//             selector: complex.clone(),
//             declarations: decls_a,
//             source_order: 1,
//         };

//         let mut decls_b = HashMap::new();
//         decls_b.insert("font-size".to_string(), "14px".to_string());
//         let rule_b = CssRule {
//             selector: complex,
//             declarations: decls_b,
//             source_order: 2,
//         };

//         let computed = compute_computed_style(vec![rule_a, rule_b], None);
//         // Since specificity is equal, the later (higher source_order) wins.
//         assert_eq!(computed.get("font-size"), Some(&"14px".to_string()));
//     }

//     // Test inheritance: if a property is inheritable and not defined on the child, it should come from the parent.
//     #[test]
//     fn test_inheritance() {
//         let child_rules: Vec<CssRule> = vec![]; // No rules for child.
//         let parent_style: HashMap<String, String> = [("color".to_string(), "red".to_string())]
//             .iter()
//             .cloned()
//             .collect();
//         let computed = compute_computed_style(child_rules, Some(&parent_style));
//         assert_eq!(computed.get("color"), Some(&"red".to_string()));
//     }

//     // Test shorthand expansion for margin.
//     #[test]
//     fn test_shorthand_margin_expansion() {
//         let comp = make_compound(Some("div"), None, &[]);
//         let complex = make_complex(comp);
//         let mut decls = HashMap::new();
//         decls.insert("margin".to_string(), "10px".to_string());
//         let rule = CssRule {
//             selector: complex,
//             declarations: decls,
//             source_order: 1,
//         };
//         let computed = compute_computed_style(vec![rule], None);
//         assert_eq!(computed.get("margin-top"), Some(&"10px".to_string()));
//         assert_eq!(computed.get("margin-right"), Some(&"10px".to_string()));
//         assert_eq!(computed.get("margin-bottom"), Some(&"10px".to_string()));
//         assert_eq!(computed.get("margin-left"), Some(&"10px".to_string()));
//     }
// }
