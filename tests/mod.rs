#[macro_use]
extern crate taikai;

extern crate tuple_utils;

mod test_simple {
    test_simple!(foo);

    #[test]
    fn test_simple() {
        let meta = Meta {
            endian: Endian::Big,
        };
        let ctx = Context {};
        let (rest, obj) = Foo::read(&[0x01, 0xab, 0x99], &meta, &ctx).unwrap();
        assert_eq!(obj, Foo {
            i: 0x01,
            baz: __subtypes::Bar {i: 0xab},
            j: 0x99,
        });
        assert_eq!(rest.len(), 0);
    }
}

mod test_resolve {
    test_resolve!(foo);

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
