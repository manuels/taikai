#taikai

[kaitai](https://www.kaitai.io)-like serialization (and deserialization)

## Runtime Requirements

 - nom
 - byteorder
 - encoding_rs
 - log

## Example

    mod test_macro {
        taikai_from_str!(crate::test_macro, "
    meta:
    endian: be
    id: top_level
    seq:
    - id: foo
        type: header
        # resolves to /top_level/header ──┐
    - id: bar       #                     │
        type: body1 #                     │
    - id: baz       #                     │
        type: body2 #                     │
    types:          #                     │
    header:   #     <─────────────────────┘ <─┐
        seq:             #                    |
        - id: i          #                    |
            type: u8     #                    |
    body1:               #                    │
        seq:             #                    │
        - id: foo        #                    │
            type: super.header #              │
            # resolves to /top_level/header ──┘
        - id: bar        #                    │
            type: root.header  #              │
            # resolves to /top_level/header ──┘
    body2:
        seq:
        - id: foo
            type: header
            # resolves to /top_level/second_level/header ──┐
        types: #                                           │
        header:   #     <──────────────────────────────────┘
            seq:
            - id: j
                type: u16le
    ");

        #[test]
        fn test_levels() {
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

## TODO
Lots of stuff

 - make enums work
 - switch-on types
 - instances
 - ...
