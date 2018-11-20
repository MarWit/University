table! {
    cargo (id) {
        id -> Int4,
        shop_id -> Int4,
        delivery_id -> Int4,
        full_name -> Varchar,
        city -> Varchar,
        zipcode -> Bpchar,
        address1 -> Varchar,
        address2 -> Nullable<Varchar>,
    }
}

table! {
    use diesel::sql_types::*;
    use domain::models::DeliveryStatusMapping;

    delivery (id) {
        id -> Int4,
        status -> DeliveryStatusMapping,
        features -> Text,
        last_location -> Int4,
        last_update -> Date,
        size_x -> Nullable<Float8>,
        size_y -> Nullable<Float8>,
        size_z -> Nullable<Float8>,
        weight -> Nullable<Float8>,
    }
}

table! {
    delivery_feature (id) {
        id -> Int4,
        name -> Varchar,
        description -> Text,
        cost -> Float8,
    }
}

table! {
    shop (id) {
        id -> Int4,
        name -> Varchar,
        city -> Varchar,
        zipcode -> Bpchar,
        address1 -> Varchar,
        address2 -> Nullable<Varchar>,
    }
}

joinable!(cargo -> delivery (delivery_id));
joinable!(cargo -> shop (shop_id));

allow_tables_to_appear_in_same_query!(
    cargo,
    delivery,
    delivery_feature,
    shop,
);
