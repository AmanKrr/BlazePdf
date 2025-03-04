use html5ever::QualName;
use std::cell::RefCell;
use std::rc::Rc;

pub mod dom_tree {
    use super::*;

    #[derive(Debug, Clone)]
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
        pub attributes: Vec<(String, String)>,
        pub children: Vec<Rc<RefCell<Node>>>,
    }

    #[derive(Debug)]
    pub struct Document {
        pub root: Rc<RefCell<Node>>,
        pub doctype: RefCell<Option<Doctype>>,
    }

    #[derive(Debug)]
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

    impl ElementNode {
        pub fn new(tag: String, qual_name: QualName) -> Self {
            ElementNode {
                tag,
                qual_name,
                attributes: Vec::new(),
                children: Vec::new(),
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
