use crate::dom::dom_tree;
use html5ever::namespace_url;
use html5ever::ns;
use html5ever::tendril::{StrTendril, TendrilSink};
use html5ever::{
    interface::{ElemName, NodeOrText, QuirksMode, TreeSink},
    LocalName, Namespace, QualName,
};
use std::cell::RefCell;
use std::rc::Rc;

/// Function to read and parse HTML content
pub fn read(html_content: &str) {
    println!("Invoking parse_html from html.rs");
    println!("Html content: {}", html_content);
    let a = create_dom_tree(html_content);
    print_document(&a);
}

const VOID_ELEMENTS: &[&str] = &[
    "meta", "img", "br", "hr", "input", "link", "area", "base", "col", "embed", "param", "source",
    "track", "wbr",
];

pub fn print_dom(node: &dom_tree::Node, indent: usize) {
    let indentation = " ".repeat(indent);
    match node {
        dom_tree::Node::DocumentRoot => {
            // DocumentRoot is just a marker, no output needed
        }
        dom_tree::Node::Text(text) => {
            if !text.trim().is_empty() {
                println!("{}{}", indentation, text.trim());
            }
        }
        dom_tree::Node::Element(elem_rc) => {
            let elem = elem_rc.borrow();
            let is_void = VOID_ELEMENTS.contains(&elem.tag.as_str());

            // Open tag with attributes
            print!("{}<{}", indentation, elem.tag);
            if !elem.attributes.is_empty() {
                for (key, value) in &elem.attributes {
                    print!(" {}=\"{}\"", key, value);
                }
            }

            if is_void {
                println!("/>");
            } else {
                println!(">");

                // Children with increased indentation
                for child in &elem.children {
                    print_dom(&child.borrow(), indent + 2);
                }

                // Closing tag
                println!("{}</{}>", indentation, elem.tag);
            }
        }
    }
}

/// Recursively prints a DOM Node with indentation.
// pub fn print_dom(node: &dom_tree::Node, indent: usize) {
//     let indentation = " ".repeat(indent);
//     match node {
//         dom_tree::Node::Text(text) => {
//             // Only print non-empty text.
//             if !text.trim().is_empty() {
//                 println!("{}{}", indentation, text.trim());
//             }
//         }
//         dom_tree::Node::Element(elem_rc) => {
//             let elem = elem_rc.borrow();
//             println!("{}<{}>", indentation, elem.tag);
//             if !elem.attributes.is_empty() {
//                 println!("{}", indentation);
//                 for (key, value) in &elem.attributes {
//                     println!("{}    {} = \"{}\"", indentation, key, value);
//                 }
//             }
//             // Print each child recursively.
//             for child in &elem.children {
//                 print_dom(&child.borrow(), indent + 2);
//             }
//             println!("{}</{}>", indentation, elem.tag);
//         }
//     }
// }

/// Prints the entire Document by printing its root node.
pub fn print_document(document: &dom_tree::Document) {
    if let Some(doctype) = &*document.doctype.borrow() {
        println!("<!DOCTYPE {}>", doctype.name);
    }
    // Since the document root is a Node inside an Rc/RefCell,
    // we borrow it and pass it to print_dom.
    let root = document.root.borrow();
    print_dom(&root, 0);
}

/// Function to create a DOM tree from HTML content
pub fn create_dom_tree(html_content: &str) -> dom_tree::Document {
    let tree_sink = BlazePdfTreeSink::new();
    let a = html5ever::parse_document(tree_sink, Default::default()).one(html_content.to_string());
    a
}

struct BlazePdfTreeSink {
    document: dom_tree::Document,
    stack: RefCell<Vec<Rc<RefCell<dom_tree::Node>>>>,
    quirks_mode: RefCell<QuirksMode>,
}

impl BlazePdfTreeSink {
    pub fn new() -> Self {
        let root = Rc::new(RefCell::new(dom_tree::Node::DocumentRoot));
        let root_element = dom_tree::new_document();
        Self {
            document: dom_tree::Document {
                root: root_element.root.clone(),
                doctype: RefCell::new(None),
            },
            stack: RefCell::new(vec![root_element.root]),
            quirks_mode: RefCell::new(QuirksMode::NoQuirks),
        }
    }
}

#[derive(Debug)]
pub struct MyElemName {
    // expanded: ExpandedName,
    ns: Namespace,
    local: LocalName,
}

impl ElemName for MyElemName {
    // fn expanded(&self) -> ExpandedName {
    //     ExpandedName {
    //         ns: self.ns(),
    //         local: self.local_name(),
    //     }
    // }

    fn local_name(&self) -> &LocalName {
        &self.local
    }

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

    fn finish(self) -> Self::Output {
        self.document
    }

    fn parse_error(&self, msg: std::borrow::Cow<'static, str>) {
        eprintln!("Parse error: {}", msg);
    }

    fn get_document(&self) -> Self::Handle {
        self.document.root.clone()
    }

    fn elem_name<'a>(&'a self, target: &'a Self::Handle) -> Self::ElemName<'a> {
        if let dom_tree::Node::Element(ref elem_rc) = *target.borrow() {
            let elem = elem_rc.borrow();
            return MyElemName {
                // expanded: elem.qual_name.expanded.clone(),
                ns: elem.qual_name.ns.clone(),
                local: elem.qual_name.local.clone(),
            };
        } else {
            panic!("elem_name called on non-element node")
        }
    }

    fn create_element(
        &self,
        name: QualName,
        attrs: Vec<html5ever::Attribute>,
        _flags: html5ever::interface::ElementFlags,
    ) -> Self::Handle {
        // if !dom_structrue_parser::is_valid_element(&name) {
        //     self.parse_error(format!("Invalid element: {:?}", name.local.to_string()).into());
        // }
        // for attr in &attrs {
        //     if !dom_structrue_parser::is_valid_attribute(&attr) {
        //         self.parse_error(
        //             format!(
        //                 "Invalid attribute: {}={}",
        //                 attr.name.local.to_string(),
        //                 attr.value.to_string()
        //             )
        //             .into(),
        //         );
        //     }
        // }
        let tag = name.local.to_string();
        let attributes = attrs
            .into_iter()
            .map(|attr| (attr.name.local.to_string(), attr.value.to_string()))
            .collect::<Vec<_>>();
        let element_node = dom_tree::ElementNode {
            tag: tag.clone(),
            qual_name: name.clone(),
            attributes,
            children: Vec::new(),
        };
        let node = Rc::new(RefCell::new(dom_tree::Node::Element(Rc::new(
            RefCell::new(element_node),
        ))));
        node
    }

    fn create_comment(&self, _text: html5ever::tendril::StrTendril) -> Self::Handle {
        Rc::new(RefCell::new(dom_tree::Node::Text(String::new())))
    }

    fn create_pi(&self, target: StrTendril, data: StrTendril) -> Self::Handle {
        let combined = format!("{} {}", target, data);
        Rc::new(RefCell::new(dom_tree::Node::Text(combined)))
    }

    // fn append(&self, parent: &Self::Handle, child: NodeOrText<Self::Handle>) {
    //     let child_node = match child {
    //         NodeOrText::AppendNode(node) => node,
    //         NodeOrText::AppendText(text) => {
    //             Rc::new(RefCell::new(dom_tree::Node::Text(text.to_string())))
    //         }
    //     };

    //     // Get the tag name if this is an element
    //     let is_html_element = match &*child_node.borrow() {
    //         dom_tree::Node::Element(elem_rc) => {
    //             let elem = elem_rc.borrow();
    //             elem.tag == "html" // Check if this is the <html> element
    //         }
    //         _ => false, // Not an element node
    //     };

    //     println!("is html element: {}", is_html_element);

    //     // Handle the <html> element specially
    //     if is_html_element {
    //         // Replace the DocumentRoot with the <html> element
    //         *parent.borrow_mut() =
    //             dom_tree::Node::Element(Rc::new(RefCell::new(dom_tree::ElementNode {
    //                 tag: "html".to_string(),
    //                 qual_name: QualName::new(None, ns!(html), "html".into()),
    //                 attributes: Vec::new(),
    //                 children: Vec::new(),
    //             })));
    //     } else {
    //         // Append non-<html> elements normally
    //         if let dom_tree::Node::Element(ref parent_elem_rc) = *parent.borrow() {
    //             parent_elem_rc
    //                 .borrow_mut()
    //                 .children
    //                 .push(child_node.clone());
    //         }
    //     }

    //     // Push element nodes to the stack (except <html>)
    //     if !is_html_element {
    //         let is_element = {
    //             let borrowed = child_node.borrow();
    //             matches!(*borrowed, dom_tree::Node::Element(_))
    //         };
    //         if is_element {
    //             self.stack.borrow_mut().push(child_node);
    //         }
    //     }
    // }

    // fn append_!(&self, parent: &Self::Handle, child: NodeOrText<Self::Handle>) {
    //     let child_node = match child {
    //         NodeOrText::AppendNode(node) => node,
    //         NodeOrText::AppendText(text) => {
    //             Rc::new(RefCell::new(dom_tree::Node::Text(text.to_string())))
    //         }
    //     };

    //     // Check if this is the <html> element
    //     let is_html_element = match &*child_node.borrow() {
    //         dom_tree::Node::Element(elem_rc) => {
    //             let elem = elem_rc.borrow();
    //             elem.tag == "html"
    //         }
    //         _ => false,
    //     };

    //     // Handle the <html> element
    //     if is_html_element {
    //         // Replace the DocumentRoot with the <html> element
    //         *parent.borrow_mut() = child_node.clone();
    //     } else {
    //         // Append non-<html> elements normally
    //         if let dom_tree::Node::Element(ref parent_elem_rc) = *parent.borrow() {
    //             parent_elem_rc
    //                 .borrow_mut()
    //                 .children
    //                 .push(child_node.clone());
    //         }
    //     }

    //     // Push element nodes to the stack (except <html>)
    //     if !is_html_element {
    //         let is_element = {
    //             let borrowed = child_node.borrow();
    //             matches!(*borrowed, dom_tree::Node::Element(_))
    //         };
    //         if is_element {
    //             self.stack.borrow_mut().push(child_node);
    //         }
    //     }
    // }

    fn append(&self, parent: &Self::Handle, child: NodeOrText<Self::Handle>) {
        let child_node = match child {
            NodeOrText::AppendNode(node) => node,
            NodeOrText::AppendText(text) => {
                Rc::new(RefCell::new(dom_tree::Node::Text(text.to_string())))
            }
        };
        if let dom_tree::Node::Element(ref parent_elem_rc) = *parent.borrow() {
            parent_elem_rc
                .borrow_mut()
                .children
                .push(child_node.clone());
        }
        let is_element = {
            let borrowed = child_node.borrow();
            matches!(*borrowed, dom_tree::Node::Element(_))
        };
        if is_element {
            self.stack.borrow_mut().push(child_node);
        }
    }

    fn append_based_on_parent_node(
        &self,
        _element: &Self::Handle,
        _prev_element: &Self::Handle,
        _child: NodeOrText<Self::Handle>,
    ) {
    }

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

    fn mark_script_already_started(&self, _node: &Self::Handle) {}

    fn pop(&self, _node: &Self::Handle) {
        self.stack.borrow_mut().pop();
    }

    fn get_template_contents(&self, target: &Self::Handle) -> Self::Handle {
        target.clone()
    }

    fn same_node(&self, x: &Self::Handle, y: &Self::Handle) -> bool {
        Rc::ptr_eq(x, y)
    }

    fn set_quirks_mode(&self, mode: QuirksMode) {
        *self.quirks_mode.borrow_mut() = mode;
    }

    fn append_before_sibling(&self, _sibling: &Self::Handle, _child: NodeOrText<Self::Handle>) {}

    fn add_attrs_if_missing(&self, target: &Self::Handle, attrs: Vec<html5ever::Attribute>) {
        if let dom_tree::Node::Element(ref elem_rc) = *target.borrow() {
            let mut elem = elem_rc.borrow_mut();
            for attr in attrs {
                let key = attr.name.local.to_string();
                if !elem.attributes.iter().any(|(k, _)| k == &key) {
                    elem.attributes.push((key, attr.value.to_string()));
                }
            }
        }
    }

    fn remove_from_parent(&self, _target: &Self::Handle) {}

    fn reparent_children(&self, _node: &Self::Handle, _new_parent: &Self::Handle) {}
}

mod dom_structrue_parser {
    use super::*;

    pub fn is_valid_attribute(attr: &html5ever::Attribute) -> bool {
        // Example: Validate against a list of known attributes for specific elements
        let valid_attributes = match attr.name.local.as_ref() {
            "href" => vec!["a"],
            "src" => vec!["img"],
            "type" => vec!["input", "button"],
            "alt" => vec!["img"],
            "class" => vec!["*"], // * means applicable to all elements
            "style" => vec!["*"], // Example: Style attribute can apply to any element
            _ => return true,     // Return true for unknown attributes by default
        };

        valid_attributes.contains(&attr.value.as_ref())
    }

    pub fn is_valid_element(name: &QualName) -> bool {
        // Example: Validate against a list of known HTML elements
        let valid_elements = vec![
            "html", "head", "body", "div", "span", "p", "h1", "h2", "h3", "h4", "h5", "h6", "a",
            "img", "table", "tr", "td", "th", "ul", "ol", "li", "form", "input", "button",
            "script", "meta", "title", // Add more as needed
        ];

        valid_elements.contains(&name.local.as_ref())
    }
}
