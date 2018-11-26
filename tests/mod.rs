#[macro_use]
extern crate taikai;

extern crate tuple_utils;

mod test_simple {
    test_simple!(foo);

    #[test]
    fn test_simple() {
        let meta = Meta {};
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
