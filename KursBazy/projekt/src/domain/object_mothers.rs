use domain::models::*;

pub struct CargoMother;
impl CargoMother {
    pub fn new_cargo( id: Id ) -> Cargo {
        Cargo {
            id: id,
            shop_id: 1,
            recipient: Recipient {
                full_name: "Jan Nowak".into(),
                location: Location {
                    city: "Wroclaw".into(),
                    zipcode: "54-321".into(),
                    address1: "ul. Jakastam 1".into(),
                    address2: None
                }
            },
            delivery_id: id
        }
    }

    pub fn new_cargo_with_recipient( id: Id, receipient: Recipient ) -> Cargo {
        Cargo {
            id: id,
            shop_id: 1,
            recipient: receipient,
            delivery_id: id
        }

    }
}

pub struct ShopMother;
impl ShopMother {
    pub fn new_shop( id: Id, name: String ) -> Shop {
        Shop {
            id: id,
            name: name,
            location: Location {
                city: "Wroclaw".into(),
                zipcode: "54-321".into(),
                address1: format!( "al. Sklepowa {}", id ),
                address2: None
            }
        }
    }
}
