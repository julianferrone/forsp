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
