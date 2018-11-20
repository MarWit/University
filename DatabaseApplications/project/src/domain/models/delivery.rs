use super::{Id, Scalar};
use schema::*;

use chrono::NaiveDate;

#[derive(Identifiable, Queryable, Clone, Debug)]
#[table_name="delivery"]
pub struct Delivery {
    pub id:                 Id,
    pub status:             DeliveryStatus,
    pub features:           String,
    pub last_location:      Id,
    pub last_update:        NaiveDate,
    pub package:            Package
}

#[derive(Insertable, Clone)]
#[table_name="delivery"]
pub struct NewDelivery {
    pub status:             DeliveryStatus,
    pub features:           String,
    pub last_location:      Id,
    pub last_update:        NaiveDate,
    pub size_x:             Option<Scalar>,
    pub size_y:             Option<Scalar>,
    pub size_z:             Option<Scalar>,
    pub weight:             Option<Scalar>
}

pub enum DeliverySort {
    ById,
    ByStatus,
    ByFeatures,
    ByLastLocation,
    ByLastUpdate,
    ByPackage(PackageSort)
}

impl ::utils::Sortable for Delivery {
    type Order = DeliverySort;
}

impl ::utils::FromQueryable for Delivery {
    type To = NewDelivery;

    fn from_queryable( self ) -> Self::To {
        Self::To {
            status:         self.status,
            features:       self.features,
            last_location:  self.last_location,
            last_update:    self.last_update,
            size_x:         self.package.size.0,
            size_y:         self.package.size.1,
            size_z:         self.package.size.2,
            weight:         self.package.weight
        }
    }
}


new_table!(DeliveryFeature; NewDeliveryFeature; "delivery_feature" => {
    name:          String,
    description:   String,
    cost:          Scalar
});

create_mapping!(DeliveryFeature; NewDeliveryFeature => {
    name        <- name,
    description <- description,
    cost        <- cost
});

#[derive(Clone, Debug, DbEnum)]
pub enum DeliveryStatus {
    UNKNOWN,
    BOOKED,
    READY,
    ONBOARD,
    DELIVERED
}

#[derive(Queryable, Copy, Clone, Debug)]
pub struct Package {
    size:       (Option<Scalar>, Option<Scalar>, Option<Scalar>),
    weight:     Option<Scalar>
}

pub enum PackageSort {
    BySize,
    ByWeight
}
