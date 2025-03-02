use html5ever::namespace_url;
use html5ever::ns;
use html5ever::QualName;
use std::cell::RefCell;
use std::rc::Rc;

pub mod dom_tree {
    use super::*;

    #[derive(Debug)]
    pub enum Node {
        DocumentRoot,
        Element(Rc<RefCell<ElementNode>>),
        Text(String),
    }

    #[derive(Debug)]
    pub struct ElementNode {
        pub tag: String,
        pub qual_name: html5ever::QualName,
        pub attributes: Vec<(String, String)>,
        pub children: Vec<Rc<RefCell<Node>>>,
    }

    #[derive(Debug)]
    pub struct Document {
        pub root: Rc<RefCell<Node>>,
        pub doctype: RefCell<Option<Doctype>>, // Separate field for DOCTYPE
    }

    #[derive(Debug)]
    pub struct Doctype {
        pub name: String,
        pub public_id: String,
        pub system_id: String,
    }

    pub fn new() -> std::rc::Rc<std::cell::RefCell<dom_tree::Node>> {
        let root_elem: Rc<RefCell<Node>> = Rc::new(RefCell::new(Node::Element(Rc::new(
            RefCell::new(ElementNode {
                tag: "html".to_string(),
                qual_name: QualName::new(None, ns!(html), "html".into()),
                attributes: Vec::new(),
                children: Vec::new(),
            }),
        ))));
        root_elem
    }

    pub fn new_document() -> Document {
        let html_element = Rc::new(RefCell::new(Node::Element(Rc::new(RefCell::new(
            ElementNode {
                tag: "html".to_string(),
                qual_name: QualName::new(None, ns!(html), "html".into()),
                attributes: Vec::new(),
                children: Vec::new(),
            },
        )))));

        Document {
            root: html_element,
            doctype: RefCell::new(None), // No DOCTYPE by default
        }
    }
}
