use super::models::Id;
use errors::*;

pub trait CommonRepository {
    type Item;

    fn insert( &mut self, item: Self::Item ) -> Result<()>;
    fn delete( &mut self, id: Id ) -> Result<()>;
    fn find( &self, id: Id ) -> Result<Self::Item>;
    fn find_all( &self ) -> Result<Vec<Self::Item>>;
    // fn update
}

pub trait PaginableRepository {
    type Item: ::utils::Sortable;

    fn get_page( &self, page: i64, page_size: i64, sort_by: <Self::Item as ::utils::Sortable>::Order ) -> Result<(Vec<Self::Item>, i64)>;
}
