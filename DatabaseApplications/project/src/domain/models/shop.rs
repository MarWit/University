use super::{Id, Location, LocationSort};
use schema::*;

#[derive(Serialize, Identifiable, Queryable, Clone, Debug)]
#[table_name="shop"]
pub struct Shop {
    pub id:         Id,
    pub name:       String,
    pub location:   Location
}

#[derive(Insertable, Clone)]
#[table_name="shop"]
pub struct NewShop {
    pub name:     String,
    pub city:     String,
    pub zipcode:  String,
    pub address1: String,
    pub address2: Option<String>
}

pub enum ShopSort {
    ById,
    ByName,
    ByLocation(LocationSort)
}

impl ::utils::Sortable for Shop {
    type Order = ShopSort;
}

create_mapping!(Shop; NewShop => {
    name        <- name,
    city        <- location.city,
    zipcode     <- location.zipcode,
    address1    <- location.address1,
    address2    <- location.address2
});
