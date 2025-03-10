use html5ever::QualName;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::rc::Weak;

pub mod dom_tree {

    use crate::style::css_matcher::ComputedStyle;

    use super::*;

    #[derive(Debug, Clone)]
    #[allow(dead_code)]
    pub enum Node {
        DocumentRoot(DocumentRootNode),
        Element(ElementNode),
        Text(String),
    }

    #[derive(Debug, Clone)]
    pub struct DocumentRootNode {
        pub children: Vec<Rc<RefCell<Node>>>,
    }

    #[derive(Debug, Clone)]
    pub struct ElementNode {
        pub tag: String,
        pub qual_name: QualName,
        pub attributes: HashMap<String, String>,

        // The forward chain of children
        pub children: Vec<Rc<RefCell<Node>>>,

        // Extra links for quick upward / sideways traversal
        pub parent: Option<Weak<RefCell<Node>>>,
        pub prev_sibling: Option<Weak<RefCell<Node>>>,
        pub next_sibling: Option<Rc<RefCell<Node>>>,

        // Layout information
        pub layout: Option<ComputedStyle>,
    }

    #[derive(Debug)]
    pub struct Document {
        pub root: Rc<RefCell<Node>>,
        pub doctype: RefCell<Option<Doctype>>,
    }

    #[derive(Debug)]
    #[allow(dead_code)]
    pub struct Doctype {
        pub name: String,
        pub public_id: String,
        pub system_id: String,
    }

    impl DocumentRootNode {
        pub fn new() -> Self {
            DocumentRootNode {
                children: Vec::new(),
            }
        }
    }

    #[allow(dead_code)]
    impl ElementNode {
        pub fn new(tag: String, qual_name: QualName) -> Self {
            ElementNode {
                tag,
                qual_name,
                attributes: HashMap::new(),
                children: Vec::new(),
                parent: None,
                prev_sibling: None,
                next_sibling: None,
                layout: None,
            }
        }
    }

    pub fn new_document() -> Document {
        Document {
            root: Rc::new(RefCell::new(Node::DocumentRoot(DocumentRootNode::new()))),
            doctype: RefCell::new(None),
        }
    }
}
