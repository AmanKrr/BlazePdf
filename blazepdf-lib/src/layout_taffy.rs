use fontdue::Font;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use taffy::prelude::*; // Imports TaffyTree, NodeId, Layout, Size, Dimension, AvailableSpace, etc.
use taffy::style_helpers::zero;

use crate::dom::dom_tree::{Document, Node};

/// Builds a Taffy layout tree from your DOM and computes the layout.
/// `available_width` is the page width in points.
/// `font` is used for text measurement.
/// Returns the TaffyTree instance, the root NodeId, and the computed Layout.
pub fn build_taffy_tree(
    document: &Document,
    available_width: f32,
    font: &Font,
) -> (TaffyTree<String>, NodeId, Layout) {
    let mut tree = TaffyTree::new();
    let root_id = convert_dom_to_taffy(&mut tree, &document.root, available_width, font)
        .expect("Failed to convert DOM to Taffy layout");

    // Compute layout using available width and unbounded height.
    tree.compute_layout_with_measure(
        root_id,
        taffy::Size {
            width: AvailableSpace::Definite(available_width),
            height: AvailableSpace::MaxContent,
        },
        &|_known,
          avail: Size<AvailableSpace>,
          _node_id,
          ctx: Option<&mut String>,
          style: &Style|
         -> Size<f32> {
            let avail_width = match avail.width {
                AvailableSpace::Definite(w) => w,
                _ => available_width,
            };
            if let Some(text) = ctx {
                let font_size = match style.size.height {
                    Dimension::Length(val) => val,
                    _ => 16.0,
                };
                measure_text_wrapped(text.as_str(), font, font_size, avail_width)
            } else {
                zero()
            }
        },
    )
    .expect("Layout computation failed");

    let taffy_tree = tree.clone();
    let layout = taffy_tree.layout(root_id).unwrap();
    (tree, root_id, layout.clone())
}

/// Recursively converts a DOM node into a Taffy node in the TaffyTree.
/// For Element nodes, extracts inline style properties; for Text nodes, stores the text in the node context.
fn convert_dom_to_taffy(
    tree: &mut TaffyTree<String>,
    dom_node: &Rc<RefCell<Node>>,
    available_width: f32,
    font: &Font,
) -> Result<NodeId, String> {
    match &*dom_node.borrow() {
        Node::DocumentRoot(root) => {
            let mut children_ids = Vec::new();
            for child in &root.children {
                let child_id = convert_dom_to_taffy(tree, child, available_width, font)?;
                children_ids.push(child_id);
            }
            tree.new_with_children(Style::default(), &children_ids)
                .map_err(|e| format!("{:?}", e))
        }
        Node::Element(elem) => {
            let style = extract_style_from_attributes(&elem.attributes, available_width);
            let mut children_ids = Vec::new();
            for child in &elem.children {
                let child_id = convert_dom_to_taffy(tree, child, available_width, font)?;
                children_ids.push(child_id);
            }
            tree.new_with_children(style, &children_ids)
                .map_err(|e| format!("{:?}", e))
        }
        Node::Text(text) => {
            // For text nodes, create a leaf node and store the text as context.
            let style = Style {
                size: Size {
                    width: Dimension::Auto,
                    height: Dimension::Auto,
                },
                ..Default::default()
            };
            tree.new_leaf_with_context(style, text.clone())
                .map_err(|e| format!("{:?}", e))
        }
    }
}

/// Extracts a Taffy Style from an element’s attribute map by parsing its inline "style" attribute.
/// Currently supports "width" and "height" in pixels; extend as needed.
fn extract_style_from_attributes(
    attributes: &HashMap<String, String>,
    available_width: f32,
) -> Style {
    let mut style = Style::default();
    if let Some(style_str) = attributes.get("style") {
        for decl in style_str.split(';') {
            let decl = decl.trim();
            if decl.is_empty() {
                continue;
            }
            if decl.starts_with("width:") {
                let value = decl.trim_start_matches("width:").trim();
                if value.ends_with("px") {
                    if let Ok(val) = value.trim_end_matches("px").trim().parse::<f32>() {
                        style.size.width = Dimension::Length(val);
                    }
                }
            }
            if decl.starts_with("height:") {
                let value = decl.trim_start_matches("height:").trim();
                if value.ends_with("px") {
                    if let Ok(val) = value.trim_end_matches("px").trim().parse::<f32>() {
                        style.size.height = Dimension::Length(val);
                    }
                }
            }
            // Extend for other properties (e.g. margin, padding, flex, etc.) as needed.
        }
    } else {
        style.size.width = Dimension::Length(available_width);
        style.size.height = Dimension::Auto;
    }
    style
}

/// Measures text with basic word-wrapping based on the available width.
/// Splits the text into words, accumulates words onto lines until adding another word would exceed the available width,
/// then measures each line. Returns a Size<f32> where width is the maximum line width and height is the sum of line heights.
/// Improved text measurement function with basic word wrapping.
/// Splits the text into words and builds lines that do not exceed the available width.
/// Returns a Size<f32> where width is the maximum line width and height is the total height.
fn measure_text_wrapped(
    text: &str,
    font: &Font,
    font_size: f32,
    available_width: f32,
) -> Size<f32> {
    // Split the text into words.
    let words: Vec<&str> = text.split_whitespace().collect();
    if words.is_empty() {
        return Size {
            width: 0.0,
            height: 0.0,
        };
    }

    let mut lines: Vec<String> = Vec::new();
    let mut current_line = String::new();

    for word in words {
        // If current_line is empty, candidate is the word itself.
        // Otherwise, candidate is current_line + " " + word.
        let candidate = if current_line.is_empty() {
            word.to_string()
        } else {
            format!("{} {}", current_line, word)
        };

        // Measure the candidate line.
        let candidate_size = measure_text_real(&candidate, font, font_size);
        if candidate_size.width <= available_width {
            // If it fits, update current_line.
            current_line = candidate;
        } else {
            // If current_line is empty, even a single word exceeds available width.
            // In that case, we still accept the word as a line.
            if current_line.is_empty() {
                current_line = word.to_string();
            }
            // Push the current line into our lines and start a new line.
            lines.push(current_line);
            current_line = String::new();
            // Start the new line with the word.
            current_line.push_str(word);
        }
    }
    // Push any remaining text as the final line.
    if !current_line.is_empty() {
        lines.push(current_line);
    }

    // Now, measure each line to determine the maximum width and total height.
    let mut max_line_width = 0.0;
    let mut total_height = 0.0;
    for line in lines {
        let line_size = measure_text_real(&line, font, font_size);
        if line_size.width > max_line_width {
            max_line_width = line_size.width;
        }
        total_height += line_size.height;
    }

    Size {
        width: max_line_width,
        height: total_height,
    }
}

/// Measures a single line of text by summing character advances.
/// This function iterates over each character in the text and sums the advance widths,
/// taking the maximum height among characters.
/// Returns a Size<f32> where width is the total advance and height is the maximum character height.
fn measure_text_real(text: &str, font: &Font, font_size: f32) -> Size<f32> {
    let mut total_width = 0.0;
    let mut max_height = 0.0;
    for c in text.chars() {
        let (metrics, _) = font.rasterize(c, font_size);
        total_width += metrics.advance_width as f32;
        if (metrics.height as f32) > max_height {
            max_height = metrics.height as f32;
        }
    }
    Size {
        width: total_width,
        height: max_height,
    }
}
