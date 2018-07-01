use domain::repositories::*;
use domain::models::*;
use infrastructure::repositories::*;

use errors::*;

pub struct CargoService<R> {
    repository: R
}

impl<R> CargoService<R>
    where R: CommonRepository<Item=Cargo> + PaginableRepository<Item=Cargo>
{
    pub fn new( repository: R ) -> Self {
        CargoService {
            repository: repository
        }
    }

    pub fn insert( &mut self, item: Cargo ) -> Result<()> {
        self.repository.insert( item )
    }

    pub fn delete( &mut self, id: Id ) -> Result<()> {
        self.repository.delete( id )
    }

    pub fn find( &self, id: Id ) -> Result<Cargo> {
        self.repository.find( id )
    }

    pub fn find_all( &self ) -> Result<Vec<Cargo>> {
        self.repository.find_all( )
    }

    pub fn get_recipient( &self, id: Id ) -> Result<Recipient> {
        self.repository.find( id ).map( |c| c.recipient )
    }

    pub fn get_page( &self, page: i64, page_size: i64, sort_by: CargoSort ) -> Result<Vec<Cargo>> {
        self.repository.get_page( page, page_size, sort_by ).map( |c| c.0 )
    }
}

pub struct ShopService<R> {
    repository: R
}

impl<R> ShopService<R>
    where R: CommonRepository<Item=Shop> + PaginableRepository<Item=Shop>
{
    pub fn new( repository: R ) -> Self {
        ShopService {
            repository: repository
        }
    }

    pub fn insert( &mut self, item: Shop ) -> Result<()> {
        self.repository.insert( item )
    }

    pub fn delete( &mut self, id: Id ) -> Result<()> {
        self.repository.delete( id )
    }

    pub fn find( &self, id: Id ) -> Result<Shop> {
        self.repository.find( id )
    }

    pub fn find_all( &self ) -> Result<Vec<Shop>> {
        self.repository.find_all( )
    }

    pub fn get_page( &self, page: i64, page_size: i64, sort_by: ShopSort ) -> Result<Vec<Shop>> {
        self.repository.get_page( page, page_size, sort_by ).map( |c| c.0 )
    }
}

pub struct DeliveryService<R> {
    repository: R
}

impl<R> DeliveryService<R>
    where R: CommonRepository<Item=Delivery>
{
    pub fn new( repository: R ) -> Self {
        DeliveryService {
            repository: repository
        }
    }

    pub fn insert( &mut self, item: Delivery ) -> Result<()> {
        self.repository.insert( item )
    }

    pub fn delete( &mut self, id: Id ) -> Result<()> {
        self.repository.delete( id )
    }

    pub fn find( &self, id: Id ) -> Result<Delivery> {
        self.repository.find( id )
    }

    pub fn find_all( &self ) -> Result<Vec<Delivery>> {
        self.repository.find_all( )
    }
}
