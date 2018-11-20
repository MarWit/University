use super::{Location, LocationSort};

#[derive(Serialize, Clone, Queryable, Debug)]
pub struct Recipient {
    pub full_name:  String,
    pub location:   Location
}

pub enum RecipientSort {
    ByFullName,
    ByLocation(LocationSort)
}
