//! The egui app.

use crate::{
    preview::shrink_to_fit,
    protocol::{decode_frame, Command, FrameHeader, SOCKET_PATH},
    settings::Settings,
};
use eframe::egui;
use ewebsock::{WsEvent, WsMessage, WsReceiver, WsSender};
use std::time::Duration;

const RECONNECT_INTERVAL: Duration = Duration::from_secs(1);

enum Connection {
    Connecting(WsSender, WsReceiver),
    Open(WsSender, WsReceiver),
    /// Disconnected since the given time, in seconds of egui input time.
    Closed {
        since: f64,
        reason: String,
    },
}

/// Shows frames from the valora server around controls for the painting.
pub struct App {
    connection: Connection,
    texture: Option<egui::TextureHandle>,
    header: Option<FrameHeader>,
    settings: Settings,
}

impl App {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        Self {
            connection: connect(&cc.egui_ctx, 0.),
            texture: None,
            header: None,
            settings: Settings::default(),
        }
    }

    fn send(&mut self, command: Command) {
        if let Connection::Open(sender, _) = &mut self.connection {
            let json = serde_json::to_string(&command).expect("serializing a command");
            sender.send(WsMessage::Text(json));
        }
    }

    /// Handles websocket events, and reconnects if the connection closed a
    /// while ago.
    fn poll(&mut self, ctx: &egui::Context) {
        let now = ctx.input(|i| i.time);
        loop {
            let receiver = match &self.connection {
                Connection::Connecting(_, receiver) | Connection::Open(_, receiver) => receiver,
                Connection::Closed { since, .. } => {
                    if now - since >= RECONNECT_INTERVAL.as_secs_f64() {
                        self.connection = connect(ctx, now);
                        continue;
                    }
                    ctx.request_repaint_after(RECONNECT_INTERVAL);
                    return;
                }
            };
            let Some(event) = receiver.try_recv() else {
                return;
            };

            match event {
                WsEvent::Opened => {
                    let previous = std::mem::replace(
                        &mut self.connection,
                        Connection::Closed {
                            since: now,
                            reason: String::new(),
                        },
                    );
                    if let Connection::Connecting(sender, receiver) = previous {
                        self.connection = Connection::Open(sender, receiver);
                    }
                }
                WsEvent::Message(WsMessage::Binary(message)) => self.show_frame(ctx, &message),
                WsEvent::Message(_) => {}
                WsEvent::Error(reason) => {
                    self.connection = Connection::Closed { since: now, reason }
                }
                WsEvent::Closed => {
                    self.connection = Connection::Closed {
                        since: now,
                        reason: String::from("closed by the server"),
                    }
                }
            }
        }
    }

    fn show_frame(&mut self, ctx: &egui::Context, message: &[u8]) {
        let Some((header, rgba)) = decode_frame(message) else {
            log::warn!("Received a malformed frame of {} bytes", message.len());
            return;
        };

        let (width, height) = (header.width as usize, header.height as usize);
        let max_side = ctx.input(|i| i.max_texture_side);
        let image = match shrink_to_fit(width, height, rgba, max_side) {
            Some((width, height, rgba)) => {
                egui::ColorImage::from_rgba_unmultiplied([width, height], &rgba)
            }
            None => egui::ColorImage::from_rgba_unmultiplied([width, height], rgba),
        };
        match &mut self.texture {
            Some(texture) => texture.set(image, egui::TextureOptions::LINEAR),
            None => {
                self.texture = Some(ctx.load_texture("frame", image, egui::TextureOptions::LINEAR))
            }
        }
        self.settings.sync(&header);
        self.header = Some(header);
    }

    fn controls(&mut self, ui: &mut egui::Ui) {
        ui.heading("valora");
        ui.separator();

        match &self.connection {
            Connection::Connecting(..) => ui.label("Connecting…"),
            Connection::Open(..) => ui.label("Connected"),
            Connection::Closed { reason, .. } => ui.label(format!("Disconnected: {}", reason)),
        };

        let connected = matches!(self.connection, Connection::Open(..));
        let Some(header) = self.header else {
            return;
        };

        ui.separator();
        self.settings_form(ui, &header, connected);

        ui.separator();
        egui::Grid::new("frame_info").num_columns(2).show(ui, |ui| {
            ui.label("Frame");
            ui.monospace(header.frame.to_string());
            ui.end_row();
            ui.label("Output");
            ui.monospace(format!("{} × {} px", header.width, header.height));
            ui.end_row();
            ui.label("Render");
            ui.monospace(format!("{:.1} ms", header.render_micros as f64 / 1000.));
            ui.end_row();
        });

        ui.separator();
        ui.add_enabled_ui(connected, |ui| {
            if ui.button("New seed").clicked() {
                self.send(Command::NewSeed);
            }
            if ui.button("Restart").clicked() {
                self.send(Command::Restart);
            }
            // The server reports whether it is paused with each frame, and
            // sends the current frame again when that changes.
            let label = match header.paused {
                true => "Resume",
                false => "Pause",
            };
            if ui
                .add(egui::Button::new(label).selected(header.paused))
                .clicked()
            {
                self.send(Command::SetPaused(!header.paused));
            }
        });
    }
}

impl App {
    /// Fields for the settings of the world, which rebuild the painting when
    /// applied.
    fn settings_form(&mut self, ui: &mut egui::Ui, header: &FrameHeader, connected: bool) {
        let mut submitted = false;
        egui::Grid::new("settings").num_columns(2).show(ui, |ui| {
            let mut edited = false;
            for field in self.settings.fields() {
                ui.label(field.label);
                let response = ui.add(
                    egui::TextEdit::singleline(field.text)
                        .char_limit(field.max_chars)
                        .hint_text(field.hint)
                        .desired_width(160.)
                        .font(egui::TextStyle::Monospace),
                );
                edited |= response.changed();
                submitted |= response.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter));
                ui.end_row();
            }
            if edited {
                self.settings.edited();
            }
        });

        let world = self.settings.world(header);
        if let (true, Err(reason)) = (self.settings.is_edited(), &world) {
            ui.colored_label(ui.visuals().error_fg_color, reason);
        }

        let can_apply = connected && self.settings.is_edited() && world.is_ok();
        ui.horizontal(|ui| {
            let apply = ui.add_enabled(can_apply, egui::Button::new("Apply"));
            if apply.clicked() || (submitted && can_apply) {
                if let Some(command) = self.settings.apply(header) {
                    self.send(command);
                }
            }
            let revert = ui.add_enabled(self.settings.is_edited(), egui::Button::new("Revert"));
            if revert.clicked() {
                self.settings.revert(header);
            }
        });
    }
}

impl eframe::App for App {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        self.poll(ui.ctx());

        egui::Panel::left("controls")
            .resizable(false)
            .show(ui, |ui| self.controls(ui));

        egui::CentralPanel::default_margins().show(ui, |ui| match &self.texture {
            Some(texture) => {
                // Fit the frame in the panel, at no more than one frame pixel per
                // physical pixel.
                let frame_size = match self.header {
                    Some(header) => egui::vec2(header.width as f32, header.height as f32),
                    None => texture.size_vec2(),
                };
                let available = ui.available_size();
                let scale = (available.x / frame_size.x)
                    .min(available.y / frame_size.y)
                    .min(1. / ui.ctx().pixels_per_point());
                ui.centered_and_justified(|ui| {
                    ui.add(
                        egui::Image::new(egui::load::SizedTexture::from_handle(texture))
                            .fit_to_exact_size(frame_size * scale),
                    );
                });
            }
            None => {
                ui.centered_and_justified(|ui| ui.label("Waiting for the first frame…"));
            }
        });
    }
}

fn connect(ctx: &egui::Context, now: f64) -> Connection {
    let location = web_sys::window().expect("a window").location();
    let scheme = match location.protocol().as_deref() {
        Ok("https:") => "wss",
        _ => "ws",
    };
    let host = location.host().expect("a host");
    let url = format!("{}://{}{}", scheme, host, SOCKET_PATH);

    let ctx = ctx.clone();
    match ewebsock::connect_with_wakeup(url, ewebsock::Options::default(), move || {
        ctx.request_repaint()
    }) {
        Ok((sender, receiver)) => Connection::Connecting(sender, receiver),
        Err(reason) => Connection::Closed { since: now, reason },
    }
}
