#[macro_use]
extern crate taikai;

mod test_simple {
    test_simple!(foo);

    #[test]
    fn test_simple() {
        let meta = Meta {};
        let ctx = Context {};
        let obj = Foo::read(&[0xab], &meta, &ctx);
        assert_eq!(obj.unwrap(), Foo {
            baz: __subtypes::Bar {i: 0xab}
        })
    }
}
