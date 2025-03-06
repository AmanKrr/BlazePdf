//! This module contains functions and types for parsing HTML into a custom DOM tree.
//!
//! It uses html5ever as the HTML parser and builds a DOM tree defined in the
//! `crate::dom::dom_tree` module.

use crate::dom::dom_tree;
use html5ever::tendril::{StrTendril, TendrilSink};
use html5ever::{
    interface::{ElemName, NodeOrText, QuirksMode, TreeSink},
    LocalName, Namespace, QualName,
};
use std::cell::RefCell;
use std::rc::Rc;

/// A list of void (self-closing) elements in HTML.
#[allow(dead_code)]
const VOID_ELEMENTS: &[&str] = &[
    "meta", "img", "br", "hr", "input", "link", "area", "base", "col", "embed", "param", "source",
    "track", "wbr",
];

/// Recursively prints the DOM tree structure starting at the given node.
///
/// # Arguments
///
/// * `node` - A reference to a DOM node to print.
/// * `indent` - The current indentation level (number of spaces) for formatting.
#[allow(dead_code)]
fn print_dom(node: &dom_tree::Node, indent: usize) {
    let indentation = " ".repeat(indent);
    match node {
        dom_tree::Node::DocumentRoot(root) => {
            for child in &root.children {
                print_dom(&child.borrow(), indent);
            }
        }
        dom_tree::Node::Element(elem) => {
            let is_void = VOID_ELEMENTS.contains(&elem.tag.as_str());

            print!("{}<{}", indentation, elem.tag);
            for (k, v) in &elem.attributes {
                print!(" {}=\"{}\"", k, v);
            }

            if is_void {
                println!("/>");
            } else {
                println!(">");
                for child in &elem.children {
                    print_dom(&child.borrow(), indent + 2);
                }
                println!("{}</{}>", indentation, elem.tag);
            }
        }
        dom_tree::Node::Text(text) => {
            if !text.trim().is_empty() {
                println!("{}{}", indentation, text.trim());
            }
        }
    }
}

/// Prints the entire Document including its DOCTYPE (if any) and the DOM tree.
///
/// # Arguments
///
/// * `document` - A reference to the Document to print.
#[allow(dead_code)]
pub fn print_document(document: &dom_tree::Document) {
    if let Some(doctype) = &*document.doctype.borrow() {
        println!("<!DOCTYPE {}>", doctype.name);
    }

    let root = document.root.borrow();
    print_dom(&root, 0);
}

/// Creates a DOM tree from the provided HTML content.
///
/// # Arguments
///
/// * `html_content` - A string slice containing the HTML to parse.
///
/// # Returns
///
/// A `dom_tree::Document` representing the parsed HTML.
pub fn create_dom_tree(html_content: &str) -> dom_tree::Document {
    let tree_sink = BlazePdfTreeSink::new();
    let document =
        html5ever::parse_document(tree_sink, Default::default()).one(html_content.to_string());
    document
}

/// A custom TreeSink for building the DOM tree used by the parser.
///
/// It holds the Document being built, a stack of open nodes, and the current quirks mode.
pub struct BlazePdfTreeSink {
    document: dom_tree::Document,
    stack: RefCell<Vec<Rc<RefCell<dom_tree::Node>>>>,
    quirks_mode: RefCell<QuirksMode>,
}

impl BlazePdfTreeSink {
    /// Creates a new `BlazePdfTreeSink` with an initial document and root node.
    pub fn new() -> Self {
        let root_element = dom_tree::new_document();
        let root_clone = root_element.root.clone();
        Self {
            document: root_element,
            stack: RefCell::new(vec![root_clone]),
            quirks_mode: RefCell::new(QuirksMode::NoQuirks),
        }
    }
}

/// A simple implementation of the `ElemName` trait for our elements.
#[derive(Debug)]
pub struct MyElemName {
    ns: Namespace,
    local: LocalName,
}

impl ElemName for MyElemName {
    /// Returns a reference to the local name of the element.
    fn local_name(&self) -> &LocalName {
        &self.local
    }

    /// Returns a reference to the namespace of the element.
    fn ns(&self) -> &Namespace {
        &self.ns
    }
}

impl TreeSink for BlazePdfTreeSink {
    type Handle = Rc<RefCell<dom_tree::Node>>;
    type Output = dom_tree::Document;
    type ElemName<'a>
        = MyElemName
    where
        Self: 'a;

    /// Finalizes and returns the constructed Document.
    fn finish(self) -> Self::Output {
        self.document
    }

    /// Called when a parsing error occurs.
    fn parse_error(&self, msg: std::borrow::Cow<'static, str>) {
        eprintln!("Parse error: {}", msg);
    }

    /// Returns the handle to the document's root node.
    fn get_document(&self) -> Self::Handle {
        self.document.root.clone()
    }

    /// Returns the element name (as `MyElemName`) for the given element handle.
    fn elem_name<'a>(&'a self, target: &'a Self::Handle) -> Self::ElemName<'a> {
        if let dom_tree::Node::Element(ref elem_rc) = *target.borrow() {
            return MyElemName {
                ns: elem_rc.qual_name.ns.clone(),
                local: elem_rc.qual_name.local.clone(),
            };
        } else {
            panic!("elem_name called on non-element node")
        }
    }

    /// Creates a new element node with the given name and attributes.
    fn create_element(
        &self,
        name: QualName,
        attrs: Vec<html5ever::Attribute>,
        _flags: html5ever::interface::ElementFlags,
    ) -> Self::Handle {
        let tag = name.local.to_string();
        // let attributes = attrs
        //     .into_iter()
        //     .map(|attr| (attr.name.local.to_string(), attr.value.to_string()))
        //     .collect::<Vec<_>>();
        let attributes = attrs
            .into_iter()
            .map(|attr| (attr.name.local.to_string(), attr.value.to_string()))
            .collect::<std::collections::HashMap<String, String>>();
        let element_node = dom_tree::ElementNode {
            tag: tag.clone(),
            qual_name: name.clone(),
            attributes,
            children: Vec::new(),
            parent: None,
            prev_sibling: None,
            next_sibling: None,
        };
        Rc::new(RefCell::new(dom_tree::Node::Element(element_node)))
    }

    /// Creates a comment node. For simplicity, returns an empty text node.
    fn create_comment(&self, _text: html5ever::tendril::StrTendril) -> Self::Handle {
        Rc::new(RefCell::new(dom_tree::Node::Text(String::new())))
    }

    /// Creates a processing instruction node by combining target and data into a text node.
    fn create_pi(&self, target: StrTendril, data: StrTendril) -> Self::Handle {
        let combined = format!("{} {}", target, data);
        Rc::new(RefCell::new(dom_tree::Node::Text(combined)))
    }

    /// Appends a child node or text to the given parent node.
    fn append(&self, parent: &Self::Handle, child: NodeOrText<Self::Handle>) {
        let child_node = match child {
            NodeOrText::AppendNode(node) => (node, true),
            NodeOrText::AppendText(text) => (
                Rc::new(RefCell::new(dom_tree::Node::Text(text.to_string()))),
                false,
            ),
        };

        // Update parent's children and sibling pointers.
        let mut parent_borrow = parent.borrow_mut();
        match &mut *parent_borrow {
            dom_tree::Node::DocumentRoot(root) => {
                // If the new child is an element, set its parent pointer.
                if let dom_tree::Node::Element(ref mut child_elem) = *child_node.0.borrow_mut() {
                    child_elem.parent = Some(Rc::downgrade(parent));
                }
                // Find the last element child in the document root.
                if let Some(prev_element) = root
                    .children
                    .iter()
                    .rev()
                    .find(|child| matches!(*child.borrow(), dom_tree::Node::Element(_)))
                {
                    // Update sibling pointers if both the previous node and the new one are elements.
                    if let (Some(child_elem), Some(prev_elem)) = (
                        match &mut *child_node.0.borrow_mut() {
                            dom_tree::Node::Element(ref mut elem) => Some(elem),
                            _ => None,
                        },
                        match &mut *prev_element.borrow_mut() {
                            dom_tree::Node::Element(ref mut elem) => Some(elem),
                            _ => None,
                        },
                    ) {
                        child_elem.prev_sibling = Some(Rc::downgrade(prev_element));
                        prev_elem.next_sibling = Some(child_node.0.clone());
                    }
                }
                // Finally, push the new child into the DocumentRoot children.
                root.children.push(child_node.0.clone());
            }
            dom_tree::Node::Element(ref mut element) => {
                // For parent element, set new child's parent pointer if it is an element.
                if let dom_tree::Node::Element(ref mut child_elem) = *child_node.0.borrow_mut() {
                    child_elem.parent = Some(Rc::downgrade(parent));
                }
                // Search for the last element among parent's children.
                if let Some(prev_element) = element
                    .children
                    .iter()
                    .rev()
                    .find(|child| matches!(*child.borrow(), dom_tree::Node::Element(_)))
                {
                    if let (Some(child_elem), Some(prev_elem)) = (
                        match &mut *child_node.0.borrow_mut() {
                            dom_tree::Node::Element(ref mut elem) => Some(elem),
                            _ => None,
                        },
                        match &mut *prev_element.borrow_mut() {
                            dom_tree::Node::Element(ref mut elem) => Some(elem),
                            _ => None,
                        },
                    ) {
                        child_elem.prev_sibling = Some(Rc::downgrade(prev_element));
                        prev_elem.next_sibling = Some(child_node.0.clone());
                    }
                }
                // Append the new child node into the parent's children vector.
                element.children.push(child_node.0.clone());
            }
            dom_tree::Node::Text(_) => {
                // Text nodes cannot have children; do nothing.
            }
        }
        let is_element = {
            let borrowed = child_node.0.borrow();
            matches!(*borrowed, dom_tree::Node::Element(_))
        };
        if is_element {
            self.stack.borrow_mut().push(child_node.0);
        }
    }

    /// Not used in this implementation.
    fn append_based_on_parent_node(
        &self,
        _element: &Self::Handle,
        _prev_element: &Self::Handle,
        _child: NodeOrText<Self::Handle>,
    ) {
    }

    /// Appends the DOCTYPE information to the Document.
    fn append_doctype_to_document(
        &self,
        name: StrTendril,
        public_id: StrTendril,
        system_id: StrTendril,
    ) {
        *self.document.doctype.borrow_mut() = Some(dom_tree::Doctype {
            name: name.to_string(),
            public_id: public_id.to_string(),
            system_id: system_id.to_string(),
        });
    }

    /// Marks that a script element has already started.
    fn mark_script_already_started(&self, _node: &Self::Handle) {}

    /// Pops the last node off the internal stack.
    fn pop(&self, _node: &Self::Handle) {
        self.stack.borrow_mut().pop();
    }

    /// Returns the contents of a template element.
    fn get_template_contents(&self, target: &Self::Handle) -> Self::Handle {
        target.clone()
    }

    /// Determines if two node handles refer to the same node.
    fn same_node(&self, x: &Self::Handle, y: &Self::Handle) -> bool {
        Rc::ptr_eq(x, y)
    }

    /// Sets the current quirks mode.
    fn set_quirks_mode(&self, mode: QuirksMode) {
        *self.quirks_mode.borrow_mut() = mode;
    }

    /// Appends a node before a sibling (not implemented).
    fn append_before_sibling(&self, _sibling: &Self::Handle, _child: NodeOrText<Self::Handle>) {}

    /// Adds attributes to the target node if they are missing.
    fn add_attrs_if_missing(&self, target: &Self::Handle, attrs: Vec<html5ever::Attribute>) {
        // Get mutable access to the node
        let mut target_node = target.borrow_mut();

        if let dom_tree::Node::Element(elem_node) = &mut *target_node {
            for attr in attrs {
                let key = attr.name.local.to_string();
                // Check if attribute already exists
                if !elem_node.attributes.iter().any(|(k, _)| k == &key) {
                    elem_node.attributes.insert(key, attr.value.to_string());
                }
            }
        }
    }

    /// Removes a node from its parent (not implemented).
    fn remove_from_parent(&self, _target: &Self::Handle) {}

    /// Reparents children from one node to another (not implemented).
    fn reparent_children(&self, _node: &Self::Handle, _new_parent: &Self::Handle) {}
}
