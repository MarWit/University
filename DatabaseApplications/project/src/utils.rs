macro_rules! new_table {
    ( $name:ident; $new_name:ident; $table:expr => {
            $( $attr:ident : $type:ty ),+
    }) => {
        #[derive(Identifiable, Queryable, Clone, Debug)]
        #[table_name=$table]
        pub struct $name {
            pub id: usize,
            $( pub $attr : $type ),*
        }

        #[derive(Insertable)]
        #[table_name=$table]
        pub struct $new_name {
            $( pub $attr : $type ),*
        }
    }
}

pub trait FromQueryable {
    type To;
    fn from_queryable( self ) -> Self::To;
}

pub trait Sortable {
    type Order;
}

macro_rules! create_mapping {
    ( $from:ty; $to:ty => { $($new:ident <- $($old:ident).+),+ } ) => {
        impl ::utils::FromQueryable for $from {
            type To = $to;

            fn from_queryable( self ) -> Self::To {
                Self::To {
                    $( $new: self.$($old).+ ),+
                }
            }
        }
    }
}
