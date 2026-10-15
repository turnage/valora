//! The fields for rebuilding the painting in a new world.

use crate::protocol::{Command, FrameHeader, World};

/// A text field for one setting of the world.
pub struct Field<'a> {
    pub label: &'static str,
    pub text: &'a mut String,
    /// The most characters the field accepts.
    pub max_chars: usize,
    /// Shown when the field is empty.
    pub hint: &'static str,
}

/// Text fields holding the settings of a world. Fields accept digits, and the
/// scale also accepts a decimal point.
#[derive(Debug, Default)]
pub struct Settings {
    pub seed: String,
    pub width: String,
    pub height: String,
    pub scale: String,
    pub frames: String,
    pub framerate: String,
    /// Whether the fields hold edits which have not been applied.
    edited: bool,
    /// The world last applied, until a frame from it arrives.
    pending: Option<World>,
}

impl Settings {
    /// The fields, in the order they should be shown.
    pub fn fields(&mut self) -> [Field<'_>; 6] {
        [
            Field {
                label: "Seed",
                text: &mut self.seed,
                max_chars: 20,
                hint: "",
            },
            Field {
                label: "Width",
                text: &mut self.width,
                max_chars: 6,
                hint: "",
            },
            Field {
                label: "Height",
                text: &mut self.height,
                max_chars: 6,
                hint: "",
            },
            Field {
                label: "Scale",
                text: &mut self.scale,
                max_chars: 10,
                hint: "",
            },
            Field {
                label: "Frames",
                text: &mut self.frames,
                max_chars: 20,
                hint: "unlimited",
            },
            Field {
                label: "Frame rate",
                text: &mut self.framerate,
                max_chars: 4,
                hint: "",
            },
        ]
    }

    /// Records an edit to the fields, dropping characters they do not accept.
    pub fn edited(&mut self) {
        for field in [
            &mut self.seed,
            &mut self.width,
            &mut self.height,
            &mut self.frames,
            &mut self.framerate,
        ] {
            field.retain(|c| c.is_ascii_digit());
        }
        let mut has_point = false;
        self.scale.retain(|c| {
            c.is_ascii_digit() || (c == '.' && !std::mem::replace(&mut has_point, true))
        });
        self.edited = true;
    }

    pub fn is_edited(&self) -> bool {
        self.edited
    }

    /// Shows the world of a frame, unless the fields hold edits or a world
    /// whose frames have not arrived yet.
    pub fn sync(&mut self, header: &FrameHeader) {
        if let Some(pending) = self.pending {
            if header.world != pending {
                return;
            }
            self.pending = None;
        }
        if self.edited {
            return;
        }

        let world = header.world;
        self.seed = world.seed.to_string();
        self.width = (world.width.round() as u32).to_string();
        self.height = (world.height.round() as u32).to_string();
        self.scale = world.scale.to_string();
        self.frames = world.frames.map_or(String::new(), |f| f.to_string());
        self.framerate = world.framerate.to_string();
    }

    /// Discards edits, showing the world of the given frame.
    pub fn revert(&mut self, header: &FrameHeader) {
        self.edited = false;
        self.pending = None;
        self.sync(header);
    }

    /// The world the fields describe, or why their values are invalid.
    /// `header` describes the current frame.
    pub fn world(&self, header: &FrameHeader) -> Result<World, String> {
        let seed = match self.seed.as_str() {
            "" => return Err(String::from("Enter a seed.")),
            seed => seed
                .parse::<u64>()
                .map_err(|_| format!("The seed may be at most {}.", u64::MAX))?,
        };

        let scale = match self.scale.as_str() {
            "" => return Err(String::from("Enter a scale.")),
            scale => match scale.parse::<f32>() {
                Ok(scale) if scale.is_finite() && scale > 0. => scale,
                Ok(_) => return Err(String::from("The scale must be greater than 0.")),
                Err(_) => return Err(String::from("The scale must be a number, like 1.5.")),
            },
        };

        // Mirror the server's limits on the output size.
        let output = |world: u32| (world as f32 * scale) as u32;
        let mut largest = ((header.max_dimension as f64 + 1.) / scale as f64).ceil() as u32;
        while largest > 0 && output(largest) > header.max_dimension {
            largest -= 1;
        }
        let dimension = |name: &str, text: &str| match text.parse::<u32>() {
            Ok(world) if output(world) >= 1 && world <= largest => Ok(world),
            Ok(world) if output(world) >= 1 => Err(format!(
                "At scale {}, the {} may be at most {}.",
                scale, name, largest
            )),
            _ if text.is_empty() => Err(format!("Enter a {}.", name)),
            _ => Err(format!(
                "The {} is too small to render at scale {}.",
                name, scale
            )),
        };
        let width = dimension("width", &self.width)?;
        let height = dimension("height", &self.height)?;

        let frames = match self.frames.as_str() {
            "" => None,
            frames => Some(
                frames
                    .parse::<u64>()
                    .map_err(|_| format!("The frame count may be at most {}.", u64::MAX))?,
            ),
        };

        let framerate = match self.framerate.parse::<u32>() {
            Ok(framerate) if framerate >= 1 => framerate,
            _ if self.framerate.is_empty() => return Err(String::from("Enter a frame rate.")),
            _ => return Err(String::from("The frame rate must be at least 1.")),
        };

        Ok(World {
            seed,
            width: width as f32,
            height: height as f32,
            scale,
            frames,
            framerate,
        })
    }

    /// Returns the command to send to apply the fields, if they are valid.
    pub fn apply(&mut self, header: &FrameHeader) -> Option<Command> {
        let world = self.world(header).ok()?;
        self.pending = Some(world);
        self.edited = false;
        Some(Command::Rebuild(world))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::protocol::test_header;

    fn settings(values: [&str; 6]) -> Settings {
        let [seed, width, height, scale, frames, framerate] = values.map(String::from);
        Settings {
            seed,
            width,
            height,
            scale,
            frames,
            framerate,
            ..Default::default()
        }
    }

    fn values(settings: &Settings) -> [&str; 6] {
        [
            &settings.seed,
            &settings.width,
            &settings.height,
            &settings.scale,
            &settings.frames,
            &settings.framerate,
        ]
        .map(String::as_str)
    }

    fn error(values: [&str; 6]) -> String {
        settings(values).world(&test_header()).unwrap_err()
    }

    #[test]
    fn syncs_from_frames() {
        let mut settings = Settings::default();
        settings.sync(&test_header());
        assert_eq!(values(&settings), ["42", "512", "650", "2", "", "24"]);

        settings.sync(&FrameHeader {
            world: World {
                scale: 0.1,
                frames: Some(30),
                ..test_header().world
            },
            ..test_header()
        });
        assert_eq!(values(&settings), ["42", "512", "650", "0.1", "30", "24"]);
    }

    #[test]
    fn edits_keep_accepted_characters() {
        let mut settings = settings(["4a2", "5-12", "6 5.0", "1.2.5x", "3e0", "2,4"]);
        settings.edited();
        assert_eq!(values(&settings), ["42", "512", "650", "1.25", "30", "24"]);
    }

    #[test]
    fn edits_are_not_overwritten() {
        let mut settings = Settings::default();
        settings.sync(&test_header());
        settings.scale = String::from("3");
        settings.edited();
        settings.sync(&test_header());
        assert_eq!(settings.scale, "3");

        settings.revert(&test_header());
        assert_eq!(settings.scale, "2");
    }

    #[test]
    fn applied_world_waits_for_its_frame() {
        let mut settings = settings(["7", "100", "200", "1.5", "60", "30"]);
        settings.edited();
        let world = World {
            seed: 7,
            width: 100.,
            height: 200.,
            scale: 1.5,
            frames: Some(60),
            framerate: 30,
        };
        assert_eq!(
            settings.apply(&test_header()),
            Some(Command::Rebuild(world))
        );
        assert!(!settings.is_edited());

        // Frames from before the rebuild do not replace the applied values.
        settings.sync(&test_header());
        assert_eq!(settings.seed, "7");

        let rebuilt = FrameHeader {
            world,
            ..test_header()
        };
        settings.sync(&rebuilt);
        settings.sync(&FrameHeader {
            world: World { seed: 8, ..world },
            ..rebuilt
        });
        assert_eq!(settings.seed, "8");
    }

    #[test]
    fn unlimited_frames() {
        let world = settings(["1", "1", "1", "1", "", "24"])
            .world(&test_header())
            .unwrap();
        assert_eq!(world.frames, None);
    }

    #[test]
    fn invalid_values() {
        assert_eq!(error(["", "1", "1", "1", "", "24"]), "Enter a seed.");
        assert_eq!(
            error(["18446744073709551616", "1", "1", "1", "", "24"]),
            format!("The seed may be at most {}.", u64::MAX)
        );
        assert_eq!(error(["1", "1", "1", "", "", "24"]), "Enter a scale.");
        assert_eq!(
            error(["1", "1", "1", ".", "", "24"]),
            "The scale must be a number, like 1.5."
        );
        assert_eq!(
            error(["1", "1", "1", "0.0", "", "24"]),
            "The scale must be greater than 0."
        );
        assert_eq!(
            error(["1", "0", "1", "1", "", "24"]),
            "The width is too small to render at scale 1."
        );
        assert_eq!(error(["1", "1", "", "1", "", "24"]), "Enter a height.");
        assert_eq!(
            error(["1", "4097", "1", "2", "", "24"]),
            "At scale 2, the width may be at most 4096."
        );
        assert_eq!(
            error(["1", "1", "1", "1", "18446744073709551616", "24"]),
            format!("The frame count may be at most {}.", u64::MAX)
        );
        assert_eq!(error(["1", "1", "1", "1", "", ""]), "Enter a frame rate.");
        assert_eq!(
            error(["1", "1", "1", "1", "", "0"]),
            "The frame rate must be at least 1."
        );
    }

    #[test]
    fn size_limits_follow_the_new_scale() {
        // The current frame is at scale 2, but the fields ask for scale 0.5.
        assert!(settings(["1", "16385", "2", "0.5", "", "24"])
            .world(&test_header())
            .is_ok());
        assert_eq!(
            error(["1", "16386", "2", "0.5", "", "24"]),
            "At scale 0.5, the width may be at most 16385."
        );
        assert_eq!(
            error(["1", "1", "2", "0.5", "", "24"]),
            "The width is too small to render at scale 0.5."
        );
    }
}
