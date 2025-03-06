use crate::parser::{blaze_html, dom_indices};
use crate::style::blaze_css;

pub mod blaze_pdf {
    use super::*;

    pub fn generate_pdf(html_content: &str, css_content: &str) {
        let dom_tree = blaze_html::create_dom_tree(html_content);
        blaze_css::parse(css_content, &dom_tree);
        let indices = dom_indices::DomIndices::build(&dom_tree);
        println!("Applying CSS...");
        println!("Document: {:#?}", &dom_tree);

        println!("Indices built: {:#?}", indices);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_owned_css_application() {
        let html_str = r#"<!DOCTYPE html>
<html>
<head>
<style>
.red { background: red; }
</style>
</head>
<body>
  <div class="red">Hello Red</div>
  <div id="blue">Hello Blue</div>
  <p>Unstyled paragraph</p>
</body>
</html>"#;

        // 1) Parse HTML -> DOM
        // let dom_document = create_dom_tree(html_str);

        // println!("Initial DOM: {:#?}", &dom_document);

        // 2) Suppose you have a CSS snippet
        let css_snippet = r#"
            .red { color: red; }
            #blue { color: blue; }  
            .red + #blue { font-size: 20px; }
            p { color: green; }
        "#;

        blaze_pdf::generate_pdf(html_str, css_snippet);

        // 5) Inspect the DOM
        // print_document(&dom_document);

        // from here, you could retrieve computed styles from each element if stored:
        // ...
        assert!(true, "Ran test without lifetime errors!");
    }
}
