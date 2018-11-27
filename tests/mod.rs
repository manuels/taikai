#![feature(proc_macro_hygiene)]

#[macro_use]
extern crate taikai;

extern crate tuple_utils;
#[macro_use]
extern crate nom;

mod test_simple {
    test_simple!();

    #[test]
    fn test_simple() {
        let meta = Meta {
            endian: Endian::Big,
        };
        let ctx = Context {};
        let (rest, obj) = Root::read(&[0x01, 0xab, 0x99], &meta, &ctx).unwrap();
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
        let meta = Meta {
            endian: Endian::Big,
        };
        let ctx = Context {};
        let (rest, obj) = Root::read(&[0x01, 0x02, 0x03], &meta, &ctx).unwrap();
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
        let meta = Meta {
            endian: Endian::Big,
        };

        let ctx = Context {};
        let data = [
            0x01, 0x02,
            0x03, 0x04,
            0x05, 0x06, 0x07,
            0x08,
        ];
        let (rest, obj) = Root::read(&data, &meta, &ctx).unwrap();
        assert_eq!(obj, Root {
            i: 0x0102,
            j: 0x0403,
            baz: Some(__subtypes::Bar {
                i: vec![5, 6, 7],
            }),
            k: None,
            l: Some(0x08),
        });
        assert_eq!(rest.len(), 0);
    }
}
