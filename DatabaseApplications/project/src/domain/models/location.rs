#[derive(Serialize, Clone, Queryable, Debug)]
pub struct Location {
    pub city:     String,
    pub zipcode:  String,
    pub address1: String,
    pub address2: Option<String>
}

pub enum LocationSort {
    ByCity,
    ByZipcode,
    ByAddress1,
    ByAddress2
}
