use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use proc_macro2::TokenStream;

use crate::type_spec;
use crate::type_spec::TypeSpec;
use crate::attribute;

#[derive(Deserialize, Debug)]
struct Meta {
    id: Option<String>,
    endian: Endian,
}

#[derive(Deserialize, Debug)]
enum Endian {
    #[serde(rename = "be")]
    Big,
    #[serde(rename = "le")]
    Little,
    #[serde(rename = "net")]
    Network,
}

#[derive(Deserialize, Debug)]
enum Repeat {
    #[serde(rename = "expr")]
    Expr,
    #[serde(rename = "until")]
    Until,
    #[serde(rename = "eos")]
    Eos,
}

#[derive(Deserialize, Debug)]
struct Root {
    meta: Meta,
    #[serde(flatten)]
    root: TypeDef,
}

#[derive(Deserialize, Default, Debug)]
#[serde(default)]
struct TypeDef {
    types: HashMap<String, TypeDef>,
    seq: Vec<Attribute>,
    instances: HashMap<String, Instance>,
}

#[derive(Deserialize, Debug)]
struct Attribute {
    id: Option<String>,
    #[serde(rename = "type")] 
    typ: Option<String>,

    #[serde(skip)]
    doc: (),
    #[serde(rename="doc-ref", skip)]
    doc_ref: (),

    repeat: Option<Repeat>,
    #[serde(rename = "repeat-expr")] 
    repeat_expr: Option<String>,
    #[serde(rename = "repeat-until")] 
    repeat_until: Option<String>,
    
    #[serde(rename = "if")] 
    cond: Option<String>,
    #[serde(default)] 
    contents: Vec<u8>,

    size: Option<usize>,
    #[serde(rename = "size-eos")] 
    size_eos: Option<bool>,
    process: Option<String>,
}

#[derive(Deserialize, Debug)]
struct Instance {
    pos: Option<usize>,
    value: Option<String>,
    #[serde(flatten)]
    attr: Attribute,
}

impl Into<type_spec::Meta> for Meta {
    fn into(self) -> type_spec::Meta {
        type_spec::Meta {
            endian: self.endian.into(),
        }
    }
}

impl Into<type_spec::Endian> for Endian {
    fn into(self) -> type_spec::Endian {
        match self {
            Endian::Big => type_spec::Endian::Big,
            Endian::Network => type_spec::Endian::Big,
            Endian::Little => type_spec::Endian::Little,
        }
    }
}

pub fn parse(scope: &[String], input: &str) -> (type_spec::Meta, Rc<RefCell<TypeSpec>>) {
    let obj: Root = serde_yaml::from_str(input).unwrap();
    let id = obj.meta.id.clone().unwrap_or_else(|| "root".to_owned());

    let scope = scope.iter().map(|s| syn::parse_str(&s[..]).unwrap()).collect();

    (obj.meta.into(), parse_type(id, scope, obj.root))
}

fn parse_attribute(id: Option<String>, a: Attribute) -> attribute::Attribute {
    let repeat = match a.repeat {
        None => attribute::Repeat::NoRepeat,
        Some(Repeat::Eos) => attribute::Repeat::Eos,
        Some(Repeat::Expr) => {
            let expr = a.repeat_expr.unwrap_or_else(|| panic!("repeat-expr missing"));
            let expr = syn::parse_str(&expr).unwrap();
            attribute::Repeat::Expr(expr)
        },
        Some(Repeat::Until) => {
            let expr = a.repeat_expr.unwrap_or_else(|| panic!("repeat-until missing"));
            let expr = syn::parse_str(&expr).unwrap();
            attribute::Repeat::Until(expr)
        },
    };

    let typ = a.typ.unwrap_or_else(|| "u8".to_owned());
    let cond = a.cond.map(|s| syn::parse_str(&s[..]).unwrap());

    attribute::Attribute::new(a.id.or(id).unwrap(), typ, repeat, cond, a.contents)
}

fn parse_type(id: String, scope: Vec<TokenStream>, typ: TypeDef) -> Rc<RefCell<TypeSpec>> {
    let types = typ.types.into_iter().map(|(id, t): (String, TypeDef)| {
        let mut new_scope = scope.clone();
        new_scope.push(quote!(__subtypes));
        (id.clone(), parse_type(id, new_scope, t))
    }).collect();

    let seq = typ.seq.into_iter().map(|a| parse_attribute(None, a)).collect();

    let instances = typ.instances.into_iter().map(|(id, inst): (String, Instance)| {
        let attr = parse_attribute(Some(id.clone()), inst.attr);
        let value = inst.value.map(|s| syn::parse_str(&s[..]).unwrap());
        let pos = inst.pos.unwrap_or(0);
        (id, attribute::Instance::from_attr(attr, pos, value))
    }).collect();

    TypeSpec::new(scope,
        id,
        types,
        seq,
        instances)
}