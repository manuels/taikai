#![feature(proc_macro_hygiene)]
#![feature(try_blocks)]
#![feature(stmt_expr_attributes)]

#[macro_use] extern crate taikai;

#[macro_use] extern crate nom;
extern crate byteorder;
extern crate tuple_utils;

#[macro_use] extern crate log;

/*
mod test_simple {
    test_simple!();

    #[test]
    fn test_simple() {
        let ctx = Context {};
        let (rest, obj) = Root::read(&[0x01, 0xab, 0x99], &ctx).unwrap();
        assert_eq!(obj, Root {
            i: 0x01,
            baz: __subtypes::Bar {i: 0xab},
            j: 0x99,
        });
        assert_eq!(rest.len(), 0);
    }
}

mod test_resolve {
    test_resolve!();

    #[test]
    fn test_resolve() {
        let ctx = Context {};
        let (rest, obj) = Root::read(&[0x01, 0x02, 0x03], &ctx).unwrap();
        assert_eq!(obj, Root {
            foo: __subtypes::Header {i: 0x01},
            bar: __subtypes::Body1 {
                foo: __subtypes::Header {i: 0x02}
            },
            baz: __subtypes::Body2 {
                foo: __subtypes::__subtypes::Header {j: 0x03}
            },
        });
        assert_eq!(rest.len(), 0);
    }
}

mod test_compound {
    test_compound!();

    #[test]
    fn test_compound() {
        let ctx = Context {};
        let data = [
            0x01, 0x02,
            0x03, 0x04,
            0x05, 0x06, 0x07,
            0x08,
            0x61, 0x62, 0x63, // abc
            0x61, 0x62, 0x63, // abc
        ];
        let (rest, obj) = Root::read(&data, &ctx).unwrap();
        assert_eq!(obj, Root {
            i: 0x0102,
            j: 0x0403,
            baz: Some(__subtypes::Bar {
                i: vec![5, 6, 7],
            }),
            k: None,
            l: Some(0x08),
            bytes: Some(vec![(),()])
        });
        assert_eq!(rest.len(), 0);
    }
}

mod test_macro {
    taikai_from_str!("
meta:
  endian: be
  id: top_level
seq:
  - id: foo
    type: header
    # resolves to /top_level/header ──┐
  - id: bar     #                     │
    type: body1 #                     │
  - id: baz     #                     │
    type: body2 #                     │
types:          #                     │
  header: #     <─────────────────────┘ <─┐
    seq:             #                    |
      - id: i        #                    |
        type: u8     #                    |
  body1:             #                    │
    seq:             #                    │
      - id: foo      #                    │
        type: super.header #              │
        # resolves to /top_level/header ──┘
      - id: bar      #                    │
        type: root.header  #              │
        # resolves to /top_level/header ──┘
  body2:
    seq:
      - id: foo
        type: header
        # resolves to /top_level/second_level/header ──┐
    types: #                                           │
      header: #     <──────────────────────────────────┘
        seq:
          - id: j
            type: u16le
");

    #[test]
    fn test_cpio() {
        let ctx = Context {};

        let bytes = &[0x01, 0x02, 0x03, 0x04, 0x05];

        let (rest, obj) = TopLevel::read(bytes, &ctx).unwrap();
        assert_eq!(obj, TopLevel {
            foo: __subtypes::Header {i: 0x01},
            bar: __subtypes::Body1 {
                foo: __subtypes::Header {i: 0x02},
                bar: __subtypes::Header {i: 0x03},
            },
            baz: __subtypes::Body2 {
                foo: __subtypes::__subtypes::Header {j: 0x0504}
            },
        });
        assert_eq!(rest.len(), 0);

        let mut out = vec![];
        obj.write(&mut out, &ctx).unwrap();

        assert_eq!(&out[..], bytes);
    }
}
*/
mod test_cpio {
    taikai_from_file!("/tmp/cpio.tsy");
}
