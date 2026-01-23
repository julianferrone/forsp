use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum MessageType {
    Error,
    Output,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Message {
    pub typ: MessageType,
    pub msg: String,
}

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let typ = match self.typ {
            MessageType::Error => "ERR: ",
            MessageType::Output => "",
        };
        write!(f, "{}{}", typ, self.msg)
    }
}

impl Message {
    pub fn print(message: &Message) {
        let display = message.to_string();
        match message.typ {
            MessageType::Error => eprintln!("{display}"),
            MessageType::Output => println!("{display}")
        };
    }
   
    pub fn new(typ: MessageType, msg: impl Into<String>) -> Message {
        Message {
            typ: typ,
            msg: msg.into()
        }
    }

    pub fn msg(msg: impl Into<String>) -> Message {
        Message::new(MessageType::Output, msg)
    }

    pub fn msg_error(msg: impl Into<String>) -> Message {
        Message::new(MessageType::Error, msg)
    }
}
