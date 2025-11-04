use regex::Regex;
use serde::Deserialize;
use std::path::Path;
use swc_core::ecma::ast::IdentName;
use swc_core::plugin::metadata::TransformPluginMetadataContextKind as Ctx;

use swc_core::{
    common::{DUMMY_SP},
    ecma::{
        ast::*,
        visit::{VisitMut, VisitMutWith},
    },
    plugin::{
        plugin_transform,
        proxies::{TransformPluginProgramMetadata},
    },
};
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Options {
    #[serde(default = "default_custom_property")]
    custom_property: String,
    #[serde(default = "default_custom_separator")]
    custom_separator: String,
    #[serde(default = "default_slash_char")]
    slash_char: String,
    #[serde(default = "default_dir_level")]
    dir_level: i32,
    #[serde(default)]
    add_module_class_names: bool,
    #[serde(default)]
    prefix: String,
    #[serde(default)]
    ignore_tree_depth: bool,
    #[serde(default)]
    ignore_node_names: bool,
    #[serde(default)]
    first_child_only: bool,
    #[serde(default)]
    omit_file_name: bool,
    /// JS `RegExp` string, e.g. "src/pages/.*\\.tsx$"
    #[serde(default)]
    r#match: Option<String>,
}

fn default_custom_property() -> String { "data-id".into() }
fn default_custom_separator() -> String { "_".into() }
fn default_slash_char() -> String { std::path::MAIN_SEPARATOR.to_string() }
fn default_dir_level() -> i32 { 1 }

struct State {
    opts: Options,
    /// Absolute filename from metadata (if available)
    filename: String,
    /// Rspack/SWC “root” (best-effort). If we can’t get it, we treat it as empty.
    root: String,

    // Derived
    file_identifier: String,
    match_regex: Option<Regex>,

    // Traversal bookkeeping (mirrors the Babel logic)
    previous_node_name: String,
    index: u32,

    // Current JSX parent stack (to compute firstChildOnly rules)
    parent_stack: Vec<String>,
}

impl State {
    fn new(opts: Options, metadata: &TransformPluginProgramMetadata) -> Self {
        // filename
        let filename = metadata
            .get_context(&Ctx::Filename)   // Option<String>
            .map(|s| s)                    // Option<String>
            .unwrap_or_default();          // String


        // best-effort project root (not always present)
       let root = metadata
            .get_context(&Ctx::Cwd)
            .map(|s| s)
            .unwrap_or_default();

        // Build path parts
        let slash = if opts.slash_char.is_empty() {
            std::path::MAIN_SEPARATOR.to_string()
        } else {
            opts.slash_char.clone()
        };

        let file_identifier = compute_file_identifier(
            &filename,
            &root,
            &slash,
            opts.dir_level,
            &opts.custom_separator,
            opts.omit_file_name,
        );

        let match_regex = opts.r#match.as_ref().and_then(|s| {
            let trimmed = s.trim();
            let body = if trimmed.starts_with('/') && trimmed.ends_with('/') && trimmed.len() >= 2 {
                &trimmed[1..trimmed.len()-1]
            } else { trimmed };
            Regex::new(body).ok()
        });

        Self {
            opts,
            filename : filename.to_string(),
            root,
            file_identifier,
            match_regex,
            previous_node_name: String::new(),
            index: 0,
            parent_stack: Vec::new(),
        }
    }

    fn should_apply_on_file(&self) -> bool {
        match &self.match_regex {
            Some(re) => re.is_match(&self.filename),
            None => true,
        }
    }

    fn filename_regex_capture(&self) -> Option<String> {
        self.match_regex
            .as_ref()
            .and_then(|re| re.find(&self.filename))
            .map(|m| m.as_str().to_string())
    }
}

fn compute_file_identifier(
    filename: &str,
    root: &str,
    slash: &str,
    sep: i32,
    custom_sep: &str,
    omit_file_name: bool,
) -> String {
    if filename.is_empty() {
        return "".into();
    }

    let p = Path::new(filename);

    // Split by user slash char (not necessarily OS separator)
    let splits: Vec<&str> = if filename.contains(slash) {
        filename.split(slash).collect()
    } else {
        filename.split(std::path::MAIN_SEPARATOR).collect()
    };

    let root_len = root.len();
    let relative = if filename.len() >= root_len && &filename[..root_len] == root {
        filename[root_len..].to_string()
    } else {
        // best effort: try to strip common prefix
        match pathdiff::diff_paths(p, root) {
            Some(rp) => rp.to_string_lossy().to_string(),
            None => filename.to_string(),
        }
    };
    let relative_splits: Vec<&str> = relative.split(slash).collect();

    // dir names selection behavior (mirror Babel)
    let dir_names: Vec<&str> = if sep >= 0 {
        let end = splits.len().saturating_sub(1);
        let start = end.saturating_sub(sep as usize);
        splits.get(start..end).unwrap_or(&[]).to_vec()
    } else {
        // negative: strip first x layers from relative path
        let start = (-sep) as usize;
        let end = relative_splits.len().saturating_sub(1);
        relative_splits.get(start..end).unwrap_or(&[]).to_vec()
    };

    let file_stem = Path::new(splits.last().unwrap_or(&""))
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("");

    if omit_file_name {
        dir_names.join(custom_sep)
    } else if dir_names.is_empty() {
        file_stem.to_string()
    } else {
        format!("{}{}{}", dir_names.join(custom_sep), custom_sep, file_stem)
    }
}

#[plugin_transform]
pub fn transform(mut program: Program, metadata: TransformPluginProgramMetadata) -> Program {
    // Read options JSON from metadata (SWC passes it as a string)
    let opts: Options = metadata
        .get_transform_plugin_config()
        .and_then(|raw| serde_json::from_str::<Options>(&raw).ok())
        .unwrap_or_else(|| Options {
            custom_property: default_custom_property(),
            custom_separator: default_custom_separator(),
            slash_char: default_slash_char(),
            dir_level: default_dir_level(),
            add_module_class_names: false,
            prefix: String::new(),
            ignore_tree_depth: false,
            ignore_node_names: false,
            first_child_only: false,
            omit_file_name: false,
            r#match: None,
        });

    let mut state = State::new(opts, &metadata);

    // If the path is clearly junk, just return
        eprintln!(
        "[SWC] file='{}' file_identifier='{}' match_regex='{:?}' should_apply={}",
        state.filename,
        state.file_identifier,
        state.match_regex,
        state.should_apply_on_file()
    );
    if state.filename.is_empty() { return program; }

    program.visit_mut_with(&mut ReactGenerateProperty { st: &mut state });
    program
}

struct ReactGenerateProperty<'a> {
    st: &'a mut State,
}

impl<'a> VisitMut for ReactGenerateProperty<'a> {
    fn visit_mut_jsx_element(&mut self, n: &mut JSXElement) {
        // Track parent stack so we can test "firstChildOnly"
        let parent_name = self
            .st
            .parent_stack
            .last()
            .cloned()
            .unwrap_or_else(|| "".into());

        // Current element name
        let node_name = jsx_element_name(&n.opening.name);

        // Push current as parent for children
        self.st.parent_stack.push(node_name.clone());

        // Recurse first so we can control “first child only” logic (but we actually need behavior
        // similar to your Babel traverse: decide here for opening element before children)
        // The Babel version visits opening element immediately; we’ll mimic that now.

        // Determine if attribute already exists
        let mut data_prop_exists = false;
        for attr in &n.opening.attrs {
            if let JSXAttrOrSpread::JSXAttr(a) = attr {
                if let JSXAttrName::Ident(id) = &a.name {
                    if id.sym.as_ref() == self.st.opts.custom_property {
                        data_prop_exists = true;
                        break;
                    }
                }
            }
        }

        // Compute className aggregation from CSS modules if requested
        let class_name = if self.st.opts.add_module_class_names {
            collect_css_module_class_names_from(&n.opening, &self.st.opts.custom_separator)
        } else {
            String::new()
        };

        // Should we act on this file at all (match regex)?
        let ok_file_match = self.st.should_apply_on_file();

        // “first child only” rule
        let match_first_child_rule = if self.st.opts.first_child_only {
            let prev = self.st.previous_node_name.clone();
            if prev.is_empty() {
                true
            } else if starts_with_uppercase(&prev) && prev == parent_name {
                true
            } else {
                false
            }
        } else {
            true
        };

        let filtering_ok = ok_file_match && match_first_child_rule;
        eprintln!(
        "[SWC] JSX <{}> filtering_ok={} data_exists={} parent={} previous={} idx={} class='{}'",
        node_name,
        filtering_ok,
        data_prop_exists,
        parent_name,
        self.st.previous_node_name,
        self.st.index,
        class_name
    );
        // Not Fragment, has name, attribute not present yet
        if filtering_ok && !data_prop_exists && !node_name.is_empty() && node_name != "Fragment" {
            let regex_prefix = self.st.filename_regex_capture();
            let name = generate_name(
                &GenerateParams {
                    path: self.st.file_identifier.clone(),
                    custom_separator: self.st.opts.custom_separator.clone(),
                    node_name: node_name.clone(),
                    previous_node_name: self.st.previous_node_name.clone(),
                    index: self.st.index,
                    class_name: class_name.clone(),
                    regex_capture: regex_prefix,
                },
                &self.st.opts,
            );

            // push attribute
            eprintln!(
            "[SWC] ✅ Adding {}='{}' to <{}>",
            self.st.opts.custom_property,
            name,
            node_name
        );
            n.opening.attrs.push(JSXAttrOrSpread::JSXAttr(JSXAttr {
                span: DUMMY_SP,
                name: JSXAttrName::Ident(IdentName {
                    sym: self.st.opts.custom_property.clone().into(),
                    span: DUMMY_SP,
                }),
                value: Some(JSXAttrValue::Lit(Lit::Str(Str {
                    span: DUMMY_SP,
                    value: name.into(),
                    raw: None,
                }))),
            }));

            // update previous/index (mirrors Babel)
            if self.st.previous_node_name == node_name {
                self.st.index += 1;
            } else {
                self.st.index = 0;
            }
            self.st.previous_node_name = node_name.clone();
        }

        // Now visit children
        n.children.visit_mut_with(self);

        // Pop parent
        self.st.parent_stack.pop();
    }
}

fn jsx_element_name(name: &JSXElementName) -> String {
    match name {
        JSXElementName::Ident(i) => i.sym.to_string(),
        JSXElementName::JSXMemberExpr(m) => member_expr_to_string(m),
        JSXElementName::JSXNamespacedName(n) => {
            format!("{}:{}", n.ns.sym, n.name.sym)
        }
    }
}

fn member_expr_to_string(m: &JSXMemberExpr) -> String {
    fn obj_to_string(obj: &JSXObject) -> String {
        match obj {
            JSXObject::Ident(i) => i.sym.to_string(),
            JSXObject::JSXMemberExpr(m) => member_expr_to_string(m),
        }
    }
    format!("{}.{}", obj_to_string(&m.obj), m.prop.sym)
}

fn collect_css_module_class_names_from(
    opening: &JSXOpeningElement,
    sep: &str,
) -> String {
    // We’re trying to capture patterns like: className={s.foo} or className={styles.bar}
    // (MemberExpr -> property ident)
    let mut classes: Vec<String> = Vec::new();

    for a in &opening.attrs {
        if let JSXAttrOrSpread::JSXAttr(attr) = a {
            if let JSXAttrName::Ident(id) = &attr.name {
                if id.sym.as_ref() == "className" {
                    if let Some(v) = &attr.value {
                        if let Some(prop) = extract_member_expr_property_ident(v) {
                            classes.push(prop);
                        }
                    }
                }
            }
        }
    }

    if classes.is_empty() {
        String::new()
    } else {
        classes.join(sep)
    }
}

fn extract_member_expr_property_ident(v: &JSXAttrValue) -> Option<String> {
    match v {
        JSXAttrValue::JSXExprContainer(c) => match &c.expr {
            JSXExpr::Expr(e) => match &**e {
                Expr::Member(MemberExpr { prop, .. }) => match prop {
                    MemberProp::Ident(i) => Some(i.sym.to_string()),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

struct GenerateParams {
    path: String,
    custom_separator: String,
    node_name: String,
    previous_node_name: String,
    index: u32,
    class_name: String,
    regex_capture: Option<String>,
}

fn generate_name(p: &GenerateParams, o: &Options) -> String {
    let mut parts: Vec<String> = Vec::new();

    if !o.prefix.is_empty() {
        parts.push(o.prefix.clone());
    }
    if let Some(rx) = &p.regex_capture {
        parts.push(rx.clone());
    }
    if !p.path.is_empty() {
        parts.push(p.path.clone());
    }

    if !o.ignore_node_names && !p.node_name.is_empty() {
        parts.push(p.node_name.clone());
    }

    if !o.ignore_tree_depth && p.previous_node_name == p.node_name {
        parts.push(p.index.to_string());
    }

    if o.add_module_class_names && !p.class_name.is_empty() {
        parts.push(p.class_name.clone());
    }

    parts
        .into_iter()
        .collect::<Vec<_>>()
        .join(&o.custom_separator)
}

fn starts_with_uppercase(s: &str) -> bool {
    s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false)
}
