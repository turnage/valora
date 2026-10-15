//! A web server for the page which shows the composition.

use crate::Result;
use anyhow::Context;
use axum::{
    body::Bytes,
    extract::{
        ws::{Message, WebSocket},
        State, WebSocketUpgrade,
    },
    http::header::CONTENT_TYPE,
    response::{Html, IntoResponse},
    routing::get,
    Router,
};
use std::{
    net::SocketAddr,
    sync::mpsc::{self, Receiver, Sender},
};
use tokio::sync::watch;
use valora_web::{
    assets,
    protocol::{encode_frame, Command, FrameHeader, SOCKET_PATH},
};

/// Serves the page, sends it each published frame, and receives its commands.
pub(crate) struct Server {
    frames: watch::Sender<Option<Bytes>>,
    commands: Receiver<Command>,
}

#[derive(Clone)]
struct Session {
    frames: watch::Receiver<Option<Bytes>>,
    commands: Sender<Command>,
}

impl Server {
    /// Starts serving on a background thread.
    pub fn start(address: SocketAddr) -> Result<Self> {
        let (frames, frames_receiver) = watch::channel(None);
        let (commands_sender, commands) = mpsc::channel();

        let runtime = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()?;
        let listener = runtime
            .block_on(tokio::net::TcpListener::bind(address))
            .with_context(|| format!("listening on {}", address))?;
        println!("Serving valora at http://{}", listener.local_addr()?);

        let router = Router::new()
            .route("/", get(|| async { Html(assets::INDEX_HTML) }))
            .route(
                "/valora_web.js",
                get(|| async { ([(CONTENT_TYPE, "text/javascript")], assets::JS) }),
            )
            .route(
                "/valora_web_bg.wasm",
                get(|| async { ([(CONTENT_TYPE, "application/wasm")], assets::WASM) }),
            )
            .route(SOCKET_PATH, get(connect))
            .with_state(Session {
                frames: frames_receiver,
                commands: commands_sender,
            });
        std::thread::spawn(move || {
            if let Err(e) = runtime.block_on(async { axum::serve(listener, router).await }) {
                eprintln!("valora server stopped: {:?}", e);
            }
        });

        Ok(Self { frames, commands })
    }

    /// Sends a frame to every open page, replacing any frame they have not
    /// received yet.
    pub fn publish(&self, header: &FrameHeader, rgba: &[u8]) {
        self.frames
            .send_replace(Some(Bytes::from(encode_frame(header, rgba))));
    }

    /// The next command from a page, if one has arrived.
    pub fn try_command(&self) -> Option<Command> {
        self.commands.try_recv().ok()
    }

    /// Waits for the next command from a page.
    pub fn wait_command(&self) -> Command {
        // The server thread holds a sender for as long as the process runs.
        self.commands.recv().expect("the server to be running")
    }
}

async fn connect(socket: WebSocketUpgrade, State(session): State<Session>) -> impl IntoResponse {
    socket.on_upgrade(move |socket| run_session(socket, session))
}

async fn run_session(mut socket: WebSocket, mut session: Session) {
    // Send the latest frame right away.
    session.frames.mark_changed();
    loop {
        tokio::select! {
            changed = session.frames.changed() => {
                if changed.is_err() {
                    return;
                }
                let frame = session.frames.borrow_and_update().clone();
                if let Some(frame) = frame {
                    if socket.send(Message::Binary(frame)).await.is_err() {
                        return;
                    }
                }
            }
            message = socket.recv() => match message {
                Some(Ok(Message::Text(text))) => match serde_json::from_str(text.as_str()) {
                    Ok(command) => {
                        let _ = session.commands.send(command);
                    }
                    Err(e) => eprintln!("Ignoring malformed command {:?}: {}", text.as_str(), e),
                },
                Some(Ok(Message::Close(_))) | Some(Err(_)) | None => return,
                Some(Ok(_)) => {}
            },
        }
    }
}
