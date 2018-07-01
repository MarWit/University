use super::{Id, Recipient, RecipientSort, Delivery, Shop};
use schema::*;

#[derive(Identifiable, Serialize, Queryable, Associations, Clone, Debug)]
#[belongs_to(Shop)]
#[belongs_to(Delivery)]
#[table_name="cargo"]
pub struct Cargo {
    pub id: Id,
    pub shop_id: Id,
    pub delivery_id: Id,
    pub recipient: Recipient
}

#[derive(Insertable, Clone)]
#[table_name="cargo"]
pub struct NewCargo {
    pub shop_id: Id,
    pub delivery_id: Id,
    pub full_name:  String,
    pub city:     String,
    pub zipcode:  String,
    pub address1: String,
    pub address2: Option<String>
}

// XXX: Create derive which can do this..
pub enum CargoSort {
    ById,
    ByShopId,
    ByDeliveryId,
    ByRecipient(RecipientSort)
}

impl ::utils::Sortable for Cargo {
    type Order = CargoSort;
}

create_mapping!(Cargo; NewCargo => {
    shop_id     <- shop_id,
    delivery_id <- delivery_id,
    full_name   <- recipient.full_name,
    city        <- recipient.location.city,
    zipcode     <- recipient.location.zipcode,
    address1    <- recipient.location.address1,
    address2    <- recipient.location.address2
});
