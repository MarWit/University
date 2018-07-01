mod cargo;
mod shop;
mod delivery;
mod hub;
mod location;
mod recipient;

pub use self::cargo::*;
pub use self::shop::*;
pub use self::delivery::*;
pub use self::hub::*;
pub use self::location::*;
pub use self::recipient::*;

pub type Id = i32;
pub type Scalar = f64;

