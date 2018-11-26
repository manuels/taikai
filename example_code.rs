/*
meta:
  id: gif
  file-extension: gif
  endian: le
seq:
  - id: header
    type: header
  - id: logical_screen
    type: logical_screen
types:
  header:
    seq:
      - id: magic
        contents: 'GIF'
      - id: version
        size: 3
  logical_screen:
    seq:
      - id: image_width
        type: u2
      - id: image_height
        type: u2
      - id: flags
        type: u1
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
*/

mod taikai {
    struct Gif {
        header:         crate::taikai::subtypes::Header,
        logical_screen: crate::taikai::subtypes::LogicalScreen,
    }

    mod precursors {
        struct Gif {}

        impl Gif {
            fn new() -> Gif {
                Gif {}
            }

            fn read(self, _parent: &(), _root: &()) -> IoResult<Gif__header> {
                // TODO: _root
                let header = crate::taikai::subtypes::Header::read(&self, _root, _parent)?;
                Ok(Gif__header {
                    header
                })
            }
        }

        struct Gif__header {
            header: crate::taikai::subtypes::Header,
        }

        impl Gif__header {
            fn read(self, _parent: &(), _root: &()) -> IoResult<Gif__logical_screen> {
                // TODO: _root
                let logical_screen = crate::taikai::subtypes::LogicalScreen::read(&self, _root, _parent)?;
                Ok(Gif__logical_screen {
                    header: self.header,
                    logical_screen,
                })
            }
        }

        struct Gif__logical_screen {
            header:         crate::taikai::subtypes::Header,
            logical_screen: crate::taikai::subtypes::LogicalScreen,
        }

        impl Gif__logical_screen {
            fn read(self, _parent: &(), _root: &()) -> IoResult<crate::taikai::Gif> {
                Ok(crate::taikai::Gif {
                    header: self.header,
                    logical_screen: self.logical_screen,
                })
            }
        }
    }

    mod subtypes {
        struct Header {
            version: [u8; 3],
        }

        struct LogicalScreen {
            image_width:        u16,
            image_height:       u16,
            flags:              u8,
            bg_color_index:     u8,
            pixel_aspect_ratio: u8,
        }
    }

    impl Gif {
        fn read(_parent: &(), _root: &()) -> IoResult<Gif> {
            let obj = create::taikai::precursors::Gif::new(_parent, _root)?;
            let obj = obj.read(_parent, _root)?; // read header
            let obj = obj.read(_parent, _root)?; // read logical_screen
            Ok(obj)
        }
    }
}

