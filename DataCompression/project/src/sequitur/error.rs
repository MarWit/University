use thiserror::Error;

#[derive(Error, Debug)]
pub enum SequiturError {
    #[error("Problem occured during (de)serialization: {0}")]
    SerializationError(#[from] bincode::Error),
    #[error("Invalid encoded token stream. Could not decode grammar.")]
    InvalidEncodedTokenStreamError,
    #[error("Unknown error")]
    Unknown,
}
