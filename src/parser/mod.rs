use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use proc_macro2::TokenStream;

use crate::types;
use crate::type_spec::TypeSpec;
use crate::attribute;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "kebab-case")] 
struct Meta {
    id: Option<String>,
    endian: Endian,
    encoding: Option<String>,

    #[serde(skip)]
    title: (),
    #[serde(skip)]
    file_extension: (),
    #[serde(skip)]
    license: (),
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
#[serde(rename_all = "lowercase")] 
enum Repeat {
    Expr,
    Until,
    Eos,
}

#[derive(Deserialize, Debug)]
struct Root {
    meta: Meta,
    #[serde(flatten)]
    root: TypeDef,
    #[serde(default)]
    context: TypeDef,
}

#[derive(Deserialize, Default, Debug)]
#[serde(default)]
struct TypeDef {
    types: HashMap<String, TypeDef>,
    seq: Vec<Attribute>,
    instances: HashMap<String, Instance>,
    enums: HashMap<String, HashMap<String, String>>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "kebab-case")] 
struct Attribute {
    id: Option<String>,
    #[serde(rename = "type")] 
    typ: Option<String>,

    #[serde(skip)]
    doc: (),
    #[serde(skip)]
    doc_ref: (),

    repeat: Option<Repeat>,
    repeat_expr: Option<String>,
    repeat_until: Option<String>,
    
    #[serde(rename = "if")] 
    cond: Option<String>,
    #[serde(default)] 
    contents: Vec<u8>,

    size: Option<String>,
    #[serde(default)] 
    size_eos: bool,
    process: Option<String>,

    encoding: Option<String>,

    terminator: Option<u8>,
    consume: Option<bool>,
    #[serde(default)] 
    include: bool,
    eos_error: Option<bool>,
}

#[derive(Deserialize, Debug)]
struct Instance {
    pos: Option<usize>,
    value: Option<String>,
    #[serde(flatten)]
    attr: Attribute,
}

impl Into<types::Meta> for Meta {
    fn into(self) -> types::Meta {
        let enc = self.encoding.unwrap_or_else(|| "utf-8".to_string());
        let enc = if encoding_rs::Encoding::for_label_no_replacement(enc.as_bytes()).is_some() {
            types::Encoding::Fixed(enc.as_bytes().to_vec())
        } else {
            let enc = syn::parse_str(&enc).unwrap();
            types::Encoding::Runtime(enc)
        };

        types::Meta {
            endian: self.endian.into(),
            encoding: enc,
        }
    }
}

impl Into<types::Endian> for Endian {
    fn into(self) -> types::Endian {
        match self {
            Endian::Big => types::Endian::Big,
            Endian::Network => types::Endian::Big,
            Endian::Little => types::Endian::Little,
        }
    }
}

pub fn parse(scope: &[String], input: &str)
    -> (types::Meta, Rc<RefCell<TypeSpec>>, Rc<RefCell<TypeSpec>>)
{
    let obj: Root = serde_yaml::from_str(input).unwrap();
    let id = obj.meta.id.clone().unwrap_or_else(|| "root".to_owned());

    let scope: Vec<_> = scope.iter().map(|s| syn::parse_str(&s[..]).unwrap()).collect();

    let ctx = parse_type("Context".to_string(), scope.clone(), obj.context);
    let root = parse_type(id, scope, obj.root);

    (obj.meta.into(), ctx, root)
}

fn parse_attribute(id: Option<String>, a: Attribute) -> attribute::Attribute {
    let typ = a.typ.unwrap_or_else(|| "u8".to_owned());
    let cond = a.cond.map(|s| syn::parse_str(&s[..]).unwrap());

    let str_props = match &typ[..] {
        "str"
        | "strz"
        | "u8" if a.size.is_some() || a.size_eos => {
            let length = if &typ[..] == "strz" {
                attribute::Length::Terminator(0)
            } else {
                if let Some(term) = a.terminator {
                    attribute::Length::Terminator(term)
                } else if let Some(size) = a.size {
                    let size = syn::parse_str(&size[..]).unwrap();
                    attribute::Length::Size(size)
                } else {
                    assert!(a.size_eos);
                    attribute::Length::Eos
                }
            };

            Some(attribute::SizeProperties {
                length,
                consume: a.consume.unwrap_or(true),
                include: a.include,
                eos_error: a.eos_error.unwrap_or(true),
            })
        },
        _ => None
    };

    /*
     * We do not want to deal with 'strz' later (which just defines the
     * terminator), so we overwrite 'strz' with 'str'!
     */
    let typ = if typ == "strz" {
        "str".to_string()
    } else {
        typ
    };

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
    
    let enc = a.encoding.map(|enc| {
        if encoding_rs::Encoding::for_label_no_replacement(enc.as_bytes()).is_some() {
            types::Encoding::Fixed(enc.as_bytes().to_vec())
        } else {
            let enc = syn::parse_str(&enc).unwrap();
            types::Encoding::Runtime(enc)
        }
    });

    attribute::Attribute::new(a.id.or(id).unwrap(), typ, repeat, cond, a.contents, enc, str_props)
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
        instances,
        typ.enums)
}

