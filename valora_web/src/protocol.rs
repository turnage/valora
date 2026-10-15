//! Messages between the valora server and its page.

use serde::{Deserialize, Serialize};

/// The path of the websocket the page connects to.
pub const SOCKET_PATH: &str = "/socket";

/// A command from the page to the server, sent as JSON text.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Command {
    /// Rebuild the painting with a new random seed.
    NewSeed,
    /// Rebuild the painting with the current seed.
    Restart,
    /// Stop or resume advancing frames.
    SetPaused(bool),
    /// Rebuild the painting in the given world.
    Rebuild(World),
}

/// The world in which a painting takes place; mirrors `valora::World`.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct World {
    pub seed: u64,
    /// The width of the world in world coordinates.
    pub width: f32,
    /// The height of the world in world coordinates.
    pub height: f32,
    /// The size of a world unit in output pixels.
    pub scale: f32,
    /// The number of frames in the painting, if it ends.
    pub frames: Option<u64>,
    /// The number of frames to render per second.
    pub framerate: u32,
}

/// Metadata for a rendered frame.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct FrameHeader {
    /// The width of the frame in pixels.
    pub width: u32,
    /// The height of the frame in pixels.
    pub height: u32,
    pub frame: u64,
    pub render_micros: u64,
    /// Whether the server has stopped advancing frames.
    pub paused: bool,
    /// The largest width or height in pixels the server can render.
    pub max_dimension: u32,
    pub world: World,
}

/// Encodes a frame as a binary message: the length of the header as a little
/// endian u32, the header as JSON, and then sRGB encoded RGBA8 pixels in
/// row-major order from the top left.
pub fn encode_frame(header: &FrameHeader, rgba: &[u8]) -> Vec<u8> {
    let json = serde_json::to_vec(header).expect("serializing a frame header");
    let mut message = Vec::with_capacity(4 + json.len() + rgba.len());
    message.extend((json.len() as u32).to_le_bytes());
    message.extend(json);
    message.extend(rgba);
    message
}

/// Decodes a message from `encode_frame`.
pub fn decode_frame(message: &[u8]) -> Option<(FrameHeader, &[u8])> {
    let (len, rest) = message.split_at_checked(4)?;
    let len = u32::from_le_bytes(len.try_into().ok()?) as usize;
    let (json, rgba) = rest.split_at_checked(len)?;
    let header: FrameHeader = serde_json::from_slice(json).ok()?;

    let expected_len = header.width as usize * header.height as usize * 4;
    (rgba.len() == expected_len).then_some((header, rgba))
}

#[cfg(test)]
pub(crate) fn test_header() -> FrameHeader {
    FrameHeader {
        width: 1024,
        height: 1300,
        frame: 3,
        render_micros: 1234,
        paused: false,
        max_dimension: 8192,
        world: World {
            seed: 42,
            width: 512.,
            height: 650.,
            scale: 2.,
            frames: None,
            framerate: 24,
        },
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn frame_round_trip() {
        let header = FrameHeader {
            width: 2,
            height: 1,
            world: World {
                seed: u64::MAX,
                scale: 0.1,
                frames: Some(100),
                ..test_header().world
            },
            ..test_header()
        };
        let rgba = [1, 2, 3, 4, 5, 6, 7, 8];
        let message = encode_frame(&header, &rgba);
        assert_eq!(decode_frame(&message), Some((header, &rgba[..])));
    }

    #[test]
    fn truncated_frame() {
        let header = FrameHeader {
            width: 2,
            height: 2,
            ..test_header()
        };
        let message = encode_frame(&header, &[0; 12]);
        assert_eq!(decode_frame(&message), None);
        assert_eq!(decode_frame(&message[..10]), None);
        assert_eq!(decode_frame(&message[..3]), None);
    }

    #[test]
    fn command_json() {
        for command in [
            Command::SetPaused(true),
            Command::Rebuild(World {
                frames: Some(7),
                ..test_header().world
            }),
        ] {
            let json = serde_json::to_string(&command).unwrap();
            assert_eq!(serde_json::from_str::<Command>(&json).unwrap(), command);
        }
    }
}
