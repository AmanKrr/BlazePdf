use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};

use crate::dom::dom_tree::{ElementNode, Node};

/// The combinator between two compound selectors.
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

/// A simple compound selector that consists of an optional tag, an optional id, and a set of classes.
/// (For brevity, attribute selectors and pseudo-classes are not implemented.)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundSelector {
    pub tag: Option<String>,
    pub id: Option<String>,
    pub classes: HashSet<String>,
}

/// A complex selector composed of a key compound (the right‐most one)
/// and an ordered chain of ancestor parts with associated combinators (right-to-left).
#[derive(Debug, Clone)]
pub struct ComplexSelector {
    pub key: CompoundSelector,
    /// Ancestors, in order from immediate relation to the farthest.
    /// For example, for the selector:
    ///   "div.red > p#header + span.foo"
    /// the key is parsed from "span.foo" and ancestors is:
    ///   [(AdjacentSibling, p#header), (Child, div.red)]
    pub ancestors: Vec<(Combinator, CompoundSelector)>,
}

pub fn parse_selector(selector: &str) -> ComplexSelector {
    // parse_complex_selector returns an Option<ComplexSelector>.
    // If parsing fails (which normally it shouldn't for valid CSS),
    // you could fallback to a compound selector with no ancestors.
    parse_complex_selector(selector).unwrap_or_else(|| ComplexSelector {
        key: parse_compound_selector(selector),
        ancestors: Vec::new(),
    })
}

/// Parse a compound selector string (e.g. "div.red#header") into a CompoundSelector.
fn parse_compound_selector(selector: &str) -> CompoundSelector {
    let mut tag = None;
    let mut id = None;
    let mut classes = HashSet::new();

    let mut chars = selector.chars().peekable();
    let mut buffer = String::new();

    // If the first character is alphabetic or '*', assume a tag name.
    if let Some(&ch) = chars.peek() {
        if ch.is_alphabetic() || ch == '*' {
            while let Some(&ch) = chars.peek() {
                if ch == '#' || ch == '.' {
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

    // Process remaining characters for id and class selectors.
    while let Some(ch) = chars.next() {
        match ch {
            '#' => {
                while let Some(&ch) = chars.peek() {
                    if ch == '.' || ch == '#' {
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
                    if ch == '.' || ch == '#' {
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
            _ => {}
        }
    }

    CompoundSelector { tag, id, classes }
}

/// Parse a full complex selector string (with combinators) into a ComplexSelector.
/// For simplicity, this parser assumes that combinators and compound selectors are
/// separated by whitespace. For example:
///   "div.red > p#header + span.foo"
/// Tokens are expected to be either one of ">", "+", "~" or a compound selector.
fn parse_complex_selector(selector: &str) -> Option<ComplexSelector> {
    let tokens: Vec<&str> = selector.split_whitespace().collect();
    if tokens.is_empty() {
        return None;
    }

    let mut iter = tokens.into_iter();
    // The first token is the initial key.
    let mut key = parse_compound_selector(iter.next().unwrap());
    let mut ancestors: Vec<(Combinator, CompoundSelector)> = Vec::new();

    // Process tokens in pairs: (combinator, next compound)
    while let Some(token) = iter.next() {
        // token should be a combinator. If it isn’t one of ">", "+", or "~",
        // assume Descendant.
        let combinator = match token {
            ">" => Combinator::Child,
            "+" => Combinator::AdjacentSibling,
            "~" => Combinator::GeneralSibling,
            _ => Combinator::Descendant,
        };
        // Next token must be a compound selector.
        let compound_token = if token == ">" || token == "+" || token == "~" {
            if let Some(next) = iter.next() {
                next
            } else {
                return None;
            }
        } else {
            token
        };
        // The current key becomes an ancestor with the given combinator.
        ancestors.push((combinator, key));
        // Update key to be the new compound.
        key = parse_compound_selector(compound_token);
    }
    // Reverse ancestors so that they are in right-to-left order.
    ancestors.reverse();
    Some(ComplexSelector { key, ancestors })
}

/// Match a compound selector against an ElementNode.
fn matches_compound(elem: &ElementNode, compound: &CompoundSelector) -> bool {
    // Check tag if specified.
    if let Some(ref tag) = compound.tag {
        if !elem.tag.eq_ignore_ascii_case(tag) {
            return false;
        }
    }
    // Check id if specified.
    if let Some(ref id_val) = compound.id {
        if let Some(elem_id) = elem.attributes.get("id") {
            if elem_id != id_val {
                return false;
            }
        } else {
            return false;
        }
    }
    // Check that all classes in the compound are present.
    if !compound.classes.is_empty() {
        if let Some(class_attr) = elem.attributes.get("class") {
            let elem_classes: HashSet<_> = class_attr
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
    true
}

/// Attempt to match a ComplexSelector against a candidate node (given as Rc<RefCell<Node>>).
/// The matching proceeds right-to-left, using parent and sibling pointers.
pub fn matches_complex_selector(candidate: &Rc<RefCell<Node>>, complex: &ComplexSelector) -> bool {
    // The candidate must be an element.
    let current_elem = {
        let node = candidate.borrow();
        match &*node {
            Node::Element(elem) => elem.clone(),
            _ => return false,
        }
    };

    // First, check the key compound against the candidate.
    if !matches_compound(&current_elem, &complex.key) {
        return false;
    }

    // Starting from candidate, work right-to-left over the ancestor chain.
    let mut current_node = Rc::clone(candidate);
    for (combinator, compound) in &complex.ancestors {
        println!("current_node: {:#?}", combinator);
        let found = match combinator {
            Combinator::Child => {
                // Immediate parent must match.
                if let Some(parent_weak) = get_parent(&current_node) {
                    if let Some(parent_rc) = parent_weak.upgrade() {
                        if let Node::Element(ref parent_elem) = *parent_rc.borrow() {
                            if matches_compound(parent_elem, compound) {
                                // Update current_node to parent.
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
                // Some ancestor (parent, grandparent, etc.) must match.
                let mut ancestor = get_parent(&current_node);
                let mut matched = false;
                while let Some(weak) = ancestor {
                    if let Some(ancestor_rc) = weak.upgrade() {
                        if let Node::Element(ref ancestor_elem) = *ancestor_rc.borrow() {
                            if matches_compound(ancestor_elem, compound) {
                                // Update current_node to the matching ancestor.
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
                // The immediate previous sibling must match.
                if let Some(sibling_rc) = get_prev_sibling(&current_node) {
                    if let Node::Element(ref sibling_elem) = *sibling_rc.borrow() {
                        if matches_compound(sibling_elem, compound) {
                            // Update current_node to that sibling.
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
                // At least one previous sibling must match.
                let siblings = get_all_prev_siblings(&current_node);
                if let Some(sibling_rc) = siblings.into_iter().find(|s| {
                    if let Node::Element(ref sibling_elem) = *s.borrow() {
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

/// Helper: get the parent pointer (Weak) from a node, if it is an Element.
fn get_parent(node: &Rc<RefCell<Node>>) -> Option<Weak<RefCell<Node>>> {
    if let Node::Element(ref elem) = *node.borrow() {
        elem.parent.clone()
    } else {
        None
    }
}

/// Helper: get the immediate previous sibling (if any) from a node.
fn get_prev_sibling(node: &Rc<RefCell<Node>>) -> Option<Rc<RefCell<Node>>> {
    if let Node::Element(ref elem) = *node.borrow() {
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
fn get_all_prev_siblings(node: &Rc<RefCell<Node>>) -> Vec<Rc<RefCell<Node>>> {
    let mut siblings = Vec::new();
    let mut current = get_prev_sibling(node);
    while let Some(sib) = current {
        siblings.push(Rc::clone(&sib));
        current = get_prev_sibling(&sib);
    }
    siblings
}
