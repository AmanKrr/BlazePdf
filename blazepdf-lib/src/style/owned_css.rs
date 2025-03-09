// src/style/owned_css.rs (for clarity, we split out an “owned” module)
use std::fmt;

// A fully-owned CSS stylesheet: a list of style rules only (skipping @media, @font-face, etc. for brevity).
#[derive(Debug)]
pub struct OwnedStylesheet {
    pub rules: Vec<OwnedRule>,
}

#[derive(Debug, Clone)]
pub struct OwnedRule {
    /// e.g. "div", ".red", "#header"
    pub selectors: Vec<String>,
    /// Each declaration is property => value, e.g. "color" => "red".
    pub declarations: Vec<OwnedDeclaration>,
}

#[derive(Debug, Clone)]
pub struct OwnedDeclaration {
    pub property: String,
    pub value: String,
}

impl fmt::Display for OwnedRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Selectors: {:?}", self.selectors)?;
        for decl in &self.declarations {
            writeln!(f, "  {}: {}", decl.property, decl.value)?;
        }
        Ok(())
    }
}
