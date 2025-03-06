use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::dom::dom_tree::{Document, Node};

/// Global indexes for fast DOM lookup.
#[derive(Debug, Default)]
#[allow(dead_code)]
pub struct DomIndices {
    /// Maps an element's "id" attribute to the corresponding node.
    pub id_map: HashMap<String, Rc<RefCell<Node>>>,
    /// Maps a class name to all nodes that have that class.
    pub class_map: HashMap<String, Vec<Rc<RefCell<Node>>>>,
    /// Maps a lowercase tag name (e.g., "div") to all nodes with that tag.
    pub tag_map: HashMap<String, Vec<Rc<RefCell<Node>>>>,
}

#[allow(dead_code)]
impl DomIndices {
    /// Build the indices for the entire document.
    pub fn build(document: &Document) -> Self {
        let mut indices = DomIndices::default();
        Self::traverse(&document.root, &mut indices);
        indices
    }

    /// Recursively traverse the DOM tree and populate the indices.
    fn traverse(node: &Rc<RefCell<Node>>, indices: &mut DomIndices) {
        match &*node.borrow() {
            Node::DocumentRoot(root) => {
                for child in &root.children {
                    Self::traverse(child, indices);
                }
            }
            Node::Element(elem) => {
                // Index by tag (store tag names in lowercase for case-insensitive matching)
                indices
                    .tag_map
                    .entry(elem.tag.to_lowercase())
                    .or_default()
                    .push(Rc::clone(node));

                // Index by id if available.
                if let Some(id_value) = elem.attributes.get("id") {
                    indices.id_map.insert(id_value.clone(), Rc::clone(node));
                }
                // Index by each class (split the "class" attribute on whitespace)
                if let Some(class_attr) = elem.attributes.get("class") {
                    for class in class_attr.split_whitespace() {
                        indices
                            .class_map
                            .entry(class.to_string())
                            .or_default()
                            .push(Rc::clone(node));
                    }
                }
                // Recurse into children.
                for child in &elem.children {
                    Self::traverse(child, indices);
                }
            }
            Node::Text(_) => {
                // Text nodes are not indexed.
            }
        }
    }
}
