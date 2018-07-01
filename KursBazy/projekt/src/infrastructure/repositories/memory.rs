use domain::repositories::*;
use domain::models::*;
use errors::*;

pub struct CargoMemory {
    list: Vec<Cargo>
}

impl CargoMemory {
    pub fn new() -> Self {
        let receipient = Recipient {
            full_name: "Jan Nowak".into(),
            location: Location {
                city: "Wroclaw".into(),
                zipcode: "54-321".into(),
                address1: "Jakastam 1".into(),
                address2: None
            }
        };

        CargoMemory {
            list: vec![
                Cargo {
                    id:             1,
                    shop_id:        1,
                    recipient:      receipient.clone(),
                    delivery_id:    1
                },
                Cargo {
                    id:             2,
                    shop_id:        1,
                    recipient:      receipient.clone(),
                    delivery_id: 2
                },
                Cargo {
                    id:             3,
                    shop_id:        1,
                    recipient:      receipient.clone(),
                    delivery_id:    3
                }
            ]
        }
    }
}

impl CommonRepository for CargoMemory {
    type Item = Cargo;

    fn insert( &mut self, item: Self::Item ) -> Result<()> {
        self.list.push( item );
        Ok( () )
    }

    fn delete( &mut self, id: Id ) -> Result<()> {
        self.list.retain( |c| c.id != id );
        Ok( () )
    }

    fn find( &self, id: Id ) -> Result<Self::Item> {
        self.list.iter().cloned().find( |c| c.id == id )
            .chain_err( || "entry does not exist" )
    }

    fn find_all( &self ) -> Result<Vec<Self::Item>> {
        Ok( self.list.clone() )
    }
}

pub struct ShopMemory {
    list: Vec<Shop>
}

impl ShopMemory {
    pub fn new() -> Self {
        ShopMemory {
            list: vec![
                Shop {
                    id: 1,
                    name: "Sum shop".into(),
                    location: Location {
                        city: "Wroclaw".into(),
                        zipcode: "54-321".into(),
                        address1: "Jakastam 13".into(),
                        address2: None
                    }
                }
            ]
        }
    }
}

impl CommonRepository for ShopMemory {
    type Item = Shop;

    fn insert( &mut self, item: Self::Item ) -> Result<()> {
        self.list.push( item );
        Ok( () )
    }

    fn delete( &mut self, id: Id ) -> Result<()> {
        self.list.retain( |c| c.id != id );
        Ok( () )
    }

    fn find( &self, id: Id ) -> Result<Self::Item> {
        self.list.iter().cloned().find( |c| c.id == id )
            .chain_err( || "entry does not exist" )
    }

    fn find_all( &self ) -> Result<Vec<Self::Item>> {
        Ok( self.list.clone() )
    }
}
