// use blazepdf_lib::dom::dom_tree;
// use blazepdf_lib::parser::blaze_html::BlazePdfTreeSink;
// use html5ever::tendril::TendrilSink;
// use std::cell::RefCell;
// use std::rc::Rc;

// #[cfg(test)]
// pub mod tests {
//     use super::*;
//     use pretty_assertions::assert_eq;
//     use std::time::Instant;

//     pub fn create_test_dom(html: &str) -> dom_tree::Document {
//         let sink = BlazePdfTreeSink::new();
//         html5ever::parse_document(sink, Default::default()).one(html.to_string())
//     }

//     fn collect_structure(node: &Rc<RefCell<dom_tree::Node>>) -> String {
//         let mut output = String::new();
//         traverse_node(&node, 0, &mut output);
//         output
//     }

//     fn traverse_node(node: &Rc<RefCell<dom_tree::Node>>, depth: usize, output: &mut String) {
//         let node_ref = node.borrow();
//         match &*node_ref {
//             dom_tree::Node::DocumentRoot(root_node) => {
//                 for child in &root_node.children {
//                     traverse_node(child, depth, output);
//                 }
//             }
//             dom_tree::Node::Element(elem_node) => {
//                 *output += &format!("{}<{}>\n", "  ".repeat(depth), elem_node.tag);
//                 for child in &elem_node.children {
//                     traverse_node(child, depth + 1, output);
//                 }
//             }
//             dom_tree::Node::Text(text) => {
//                 let trimmed = text.trim();
//                 if !trimmed.is_empty() {
//                     *output += &format!("{}{}\n", "  ".repeat(depth), trimmed);
//                 }
//             }
//         }
//     }

//     #[test]
//     fn test_basic_structure() {
//         let html = r#"
//             <!DOCTYPE html>
//             <html>
//                 <head>
//                     <title>Test</title>
//                 </head>
//                 <body>
//                     <h1>Hello</h1>
//                     <p>World</p>
//                 </body>
//             </html>
//         "#;

//         let document = create_test_dom(html);
//         let structure = collect_structure(&document.root);

//         let expected = r#"
// <html>
//   <head>
//     <title>
//       Test
//   <body>
//     <h1>
//       Hello
//     <p>
//       World
// "#;
//         assert_eq!(structure.trim(), expected.trim());
//     }

//     #[test]
//     fn test_void_elements() {
//         let html = r#"
//             <img src="test.jpg" alt="Test">
//             <br>
//             <input type="text">
//         "#;

//         let document = create_test_dom(html);
//         let structure = collect_structure(&document.root);

//         let expected = r#"
// <html>
//   <head>
//   <body>
//     <img>
//     <br>
//     <input>
// "#;
//         assert_eq!(structure.trim(), expected.trim());
//     }

//     #[test]
//     fn test_nested_elements() {
//         let html = r#"
//             <div class="container">
//                 <div class="row">
//                     <div class="col">1</div>
//                     <div class="col">2</div>
//                 </div>
//             </div>
//         "#;

//         let document = create_test_dom(html);
//         let structure = collect_structure(&document.root);

//         let expected = r#"
// <html>
//   <head>
//   <body>
//     <div>
//       <div>
//         <div>
//           1
//         <div>
//           2
// "#;
//         assert_eq!(structure.trim(), expected.trim());
//     }

//     #[test]
//     fn test_attributes() {
//         let html = r#"
//             <a href="https://example.com" target="_blank" data-test="123">
//                 Link
//             </a>
//         "#;

//         let document = create_test_dom(html);
//         let mut attributes = Vec::new();

//         // Traverse the document structure
//         if let dom_tree::Node::DocumentRoot(root) = &*document.root.borrow() {
//             // Get HTML element (first child of document root)
//             if let Some(html_node) = root.children.get(0) {
//                 let html_node_ref = html_node.borrow();
//                 if let dom_tree::Node::Element(html_elem) = &*html_node_ref {
//                     // Get body element (second child of html)
//                     if let Some(body_node) = html_elem.children.get(1) {
//                         let body_node_ref = body_node.borrow();
//                         if let dom_tree::Node::Element(body_elem) = &*body_node_ref {
//                             // Get first child of body (our <a> tag)
//                             if let Some(a_node) = body_elem.children.get(0) {
//                                 let a_node_ref = a_node.borrow();
//                                 if let dom_tree::Node::Element(a_elem) = &*a_node_ref {
//                                     attributes = a_elem.attributes.clone();
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//         }

//         assert_eq!(
//             attributes,
//             vec![
//                 ("href".to_string(), "https://example.com".to_string()),
//                 ("target".to_string(), "_blank".to_string()),
//                 ("data-test".to_string(), "123".to_string())
//             ]
//         );
//     }

//     #[test]
//     fn test_mixed_content() {
//         let html = r#"
//             <p>
//                 This is <strong>bold</strong> and <em>italic</em> text.
//                 <br>
//                 Next line.
//             </p>
//         "#;

//         let document = create_test_dom(html);
//         let structure = collect_structure(&document.root);

//         let expected = r#"
// <html>
//   <head>
//   <body>
//     <p>
//       This is
//       <strong>
//         bold
//       and
//       <em>
//         italic
//       text.
//       <br>
//       Next line.
// "#;
//         assert_eq!(structure.trim(), expected.trim());
//     }

//     #[test]
//     fn test_deeply_nested() {
//         let html = r#"
//             <div>
//                 <div>
//                     <div>
//                         <div>
//                             <p>Deep</p>
//                         </div>
//                     </div>
//                 </div>
//             </div>
//         "#;

//         let document = create_test_dom(html);
//         let structure = collect_structure(&document.root);

//         let expected = r#"
// <html>
//   <head>
//   <body>
//     <div>
//       <div>
//         <div>
//           <div>
//             <p>
//               Deep
// "#;
//         assert_eq!(structure.trim(), expected.trim());
//     }

//     #[test]
//     fn test_doctype() {
//         let html = r#"
//             <!DOCTYPE html>
//             <html>
//                 <head></head>
//             </html>
//         "#;

//         let document = create_test_dom(html);
//         assert_eq!(document.doctype.borrow().as_ref().unwrap().name, "html");
//     }

//     #[test]
//     fn test_performance() {
//         let mut big_html = String::new();
//         big_html.push_str("<div>");
//         for _ in 0..1000 {
//             big_html.push_str("<p>Test</p>");
//         }
//         big_html.push_str("</div>");

//         let start = Instant::now();
//         // let document = create_test_dom(&big_html);
//         let duration = start.elapsed();

//         println!("Parsing time: {:?}", duration);
//         assert!(duration < std::time::Duration::from_millis(100));
//     }

//     #[test]
//     fn test_malformed_html() {
//         let html = r#"
//             <div>
//                 <p>Unclosed
//                 <img>
//                 </div>
//         "#;

//         let document = create_test_dom(html);
//         let structure = collect_structure(&document.root);

//         let expected = r#"
// <html>
//   <head>
//   <body>
//     <div>
//       <p>
//         Unclosed
//         <img>
// "#;
//         assert_eq!(structure.trim(), expected.trim());
//     }

//     #[test]
//     fn test_table_autocorrection() {
//         let html = "<table><td>Cell</td></table>";
//         let document = create_test_dom(html);
//         // Should have auto-inserted <tbody> and <tr>

//         let structure = collect_structure(&document.root);

//         let expected = r#"
// <html>
//   <head>
//   <body>
//     <table>
//       <tbody>
//         <tr>
//           <td>
//             Cell
// "#;
//         assert_eq!(structure.trim(), expected.trim());
//     }

//     #[test]
//     fn test_form_containment() {
//         let html = "<form><div><input type='text'></div></form>";
//         // Verify input is properly contained within form
//         let document = create_test_dom(html);
//         // Should have auto-inserted <tbody> and <tr>

//         let structure = collect_structure(&document.root);

//         let expected = r#"
// <html>
//   <head>
//   <body>
//     <form>
//       <div>
//         <input>
// "#;
//         assert_eq!(structure.trim(), expected.trim());
//     }
// }
