use super::services::*;
use infrastructure::repositories::memory::*;
use domain::object_mothers::*;

#[test]
fn cargoservice_find_all_objects() {
    let service = CargoService::new( CargoMemory::new() );
    let objects = service.find_all();

    assert!( objects.len() == 3 );
}

#[test]
fn cargoservice_add_new_object() {
    let mut service = CargoService::new( CargoMemory::new() );
    let object = CargoMother::new_cargo( 21 );

    service.insert( object );
    let objects = service.find_all();

    assert!( objects.len() == 4 );
}

#[test]
fn cargoservice_add_and_remove() {
    let mut service = CargoService::new( CargoMemory::new() );
    let object = CargoMother::new_cargo( 21 );

    service.insert( object );
    let objects = service.find_all();

    assert!( objects.len() == 4 );

    service.delete( 21 );
    let objects = service.find_all();

    assert!( objects.len() == 3 );
}

#[test]
fn shopservice_find_all_objects() {
    let service = ShopService::new( ShopMemory::new() );
    let objects = service.find_all();

    assert!( objects.len() == 1 );
}
#[test]
fn shopservice_add_new_object() {
    let mut service = ShopService::new( ShopMemory::new() );
    let object = ShopMother::new_shop( 21, "Sum test shop".into() );

    service.insert( object );
    let objects = service.find_all();

    assert!( objects.iter().any( |s| s.id == 21 ) );
}
