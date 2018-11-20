use std::marker::PhantomData;

use domain::repositories::*;
use domain::models::*;
use utils::FromQueryable;
use pagination::*;
use errors::*;

use diesel::prelude::*;

pub struct DatabaseRepository<'a, R> {
    database: &'a PgConnection,
    _phantom: PhantomData<R>
}

impl<'a, R> DatabaseRepository<'a, R> {
    pub fn new( connection: &'a PgConnection ) -> Self {
        DatabaseRepository {
            database: connection,
            _phantom: PhantomData
        }
    }
}

impl<'a> CommonRepository for DatabaseRepository<'a, Cargo> {
    type Item = Cargo;

    fn insert( &mut self, item: Self::Item ) -> Result<()> {
        use schema::cargo::dsl::*;

        ::diesel::insert_into( cargo )
            .values( & item.from_queryable() )
            .execute( self.database )?;

        Ok( () )
    }

    fn delete( &mut self, item_id: Id ) -> Result<()> {
        use schema::cargo::dsl::*;

        ::diesel::delete( cargo )
            .filter( id.eq( item_id ) )
            .execute( self.database )?;

        Ok( () )
    }

    fn find( &self, item_id: Id ) -> Result<Self::Item> {
        use schema::cargo::dsl::*;

        cargo.filter( id.eq( item_id ) )
             .select( (id, shop_id, delivery_id, (full_name, (city, zipcode, address1, address2))) )
             .first::<Self::Item>( self.database )
             .map_err( |e| e.into() )
    }

    fn find_all( &self ) -> Result<Vec<Self::Item>> {
        use schema::cargo::dsl::*;

        cargo.select( (id, shop_id, delivery_id, (full_name, (city, zipcode, address1, address2))) )
             .load::<Self::Item>( self.database )
             .map_err( |e| e.into() )
    }
}

impl<'a> PaginableRepository for DatabaseRepository<'a, Cargo> {
    type Item = Cargo;

    fn get_page( &self, page: i64, page_size: i64, sort_by: CargoSort ) -> Result<(Vec<Self::Item>, i64)> {
        use self::CargoSort::*;
        use schema::cargo::dsl::*;

        let query = match sort_by {
            ById         => cargo.order( id ).into_boxed(),
            ByShopId     => cargo.order( shop_id ).into_boxed(),
            ByDeliveryId => cargo.order( delivery_id ).into_boxed(),
            _            => unimplemented!()
        };

        query.select( (id, shop_id, delivery_id, (full_name, (city, zipcode, address1, address2))) )
            .paginate( page )
            .per_page( page_size )
            .load_and_count_pages::<Self::Item>( self.database )
            .map_err( |e| e.into() )
    }
}

impl<'a> CommonRepository for DatabaseRepository<'a, Shop> {
    type Item = Shop;

    fn insert( &mut self, item: Self::Item ) -> Result<()> {
        use schema::shop::dsl::*;

        ::diesel::insert_into( shop )
            .values( & item.from_queryable() )
            .execute( self.database )?;

        Ok( () )
    }

    fn delete( &mut self, item_id: Id ) -> Result<()> {
        use schema::shop::dsl::*;

        ::diesel::delete( shop )
            .filter( id.eq( item_id ) )
            .execute( self.database )?;

        Ok( () )
    }

    fn find( &self, item_id: Id ) -> Result<Self::Item> {
        use schema::shop::dsl::*;

        shop.filter( id.eq( item_id ) )
            .select( (id, name, (city, zipcode, address1, address2)) )
            .first::<Self::Item>( self.database )
            .map_err( |e| e.into() )
    }

    fn find_all( &self ) -> Result<Vec<Self::Item>> {
        use schema::shop::dsl::*;

        shop.select( (id, name, (city, zipcode, address1, address2)) )
            .load::<Self::Item>( self.database )
            .map_err( |e| e.into() )
    }
}

impl<'a> PaginableRepository for DatabaseRepository<'a, Shop> {
    type Item = Shop;

    fn get_page( &self, page: i64, page_size: i64, sort_by: ShopSort ) -> Result<(Vec<Self::Item>, i64)> {
        use self::ShopSort::*;
        use schema::shop::dsl::*;

        let query = match sort_by {
            ById   => shop.order( id ).into_boxed(),
            ByName => shop.order( name ).into_boxed(),
            _      => unimplemented!()
        };

        query.select( (id, name, (city, zipcode, address1, address2)) )
            .paginate( page )
            .per_page( page_size )
            .load_and_count_pages::<Self::Item>( self.database )
            .map_err( |e| e.into() )
    }
}

impl<'a> CommonRepository for DatabaseRepository<'a, Delivery> {
    type Item = Delivery;

    fn insert( &mut self, item: Self::Item ) -> Result<()> {
        use schema::delivery::dsl::*;

        ::diesel::insert_into( delivery )
            .values( & item.from_queryable() )
            .execute( self.database )?;

        Ok( () )
    }

    fn delete( &mut self, item_id: Id ) -> Result<()> {
        use schema::delivery::dsl::*;

        ::diesel::delete( delivery )
            .filter( id.eq( item_id ) )
            .execute( self.database )?;

        Ok( () )
    }

    fn find( &self, item_id: Id ) -> Result<Self::Item> {
        use schema::delivery::dsl::*;

        delivery.filter( id.eq( item_id ) )
            .select( (id, status, features, last_location, last_update, ((size_x, size_y, size_z), weight)) )
            .first::<Self::Item>( self.database )
            .map_err( |e| e.into() )
    }

    fn find_all( &self ) -> Result<Vec<Self::Item>> {
        use schema::delivery::dsl::*;

        delivery.select( (id, status, features, last_location, last_update, ((size_x, size_y, size_z), weight)) )
            .load::<Self::Item>( self.database )
            .map_err( |e| e.into() )
    }
}
