#![feature(box_syntax)]
#![feature(plugin)]
#![plugin(rocket_codegen)]

#[macro_use] extern crate diesel;
#[macro_use] extern crate diesel_derive_enum;
#[macro_use] extern crate error_chain;
#[macro_use] extern crate serde_derive;
extern crate serde;
extern crate r2d2;
extern crate r2d2_diesel;
extern crate rocket;
extern crate rocket_contrib;
extern crate chrono;

mod errors;
#[macro_use] mod utils;
mod pagination; // TODO(marwit): Move to utils
mod schema;
mod application;
mod infrastructure;
mod domain;

use std::collections::BTreeMap;

use diesel::prelude::*;
use r2d2_diesel::ConnectionManager;
use error_chain::ChainedError;
use errors::*;

use rocket::response::Redirect;
use rocket_contrib::Template;

const BASE_URL: &'static str = "http://10.8.0.2:8000";

struct DbConn(pub r2d2::PooledConnection<r2d2_diesel::ConnectionManager<PgConnection>>);
impl<'a, 'r> rocket::request::FromRequest<'a, 'r> for DbConn {
    type Error = ();

    fn from_request( request: &'a rocket::request::Request<'r> ) -> rocket::request::Outcome<Self, Self::Error> {
        let pool = request.guard::<rocket::State<r2d2::Pool<r2d2_diesel::ConnectionManager<PgConnection>>>>()?;
        match pool.get() {
            Ok( conn ) => rocket::Outcome::Success( DbConn( conn ) ),
            Err( _ ) => rocket::Outcome::Failure( ( rocket::http::Status::ServiceUnavailable, () ) )
        }
    }
}

impl std::ops::Deref for DbConn {
    type Target = PgConnection;

    fn deref( & self ) -> & Self::Target {
        & self.0
    }
}

struct User;
impl<'a, 'r> rocket::request::FromRequest<'a, 'r> for User {
    type Error = ();

    fn from_request( request: &'a rocket::request::Request<'r> ) -> rocket::request::Outcome<Self, Self::Error> {
        if request.cookies().get_private( "auth" ).is_some() {
            rocket::Outcome::Success( Self {} )
        } else {
            rocket::Outcome::Forward( () )
        }
    }
}

#[derive(Serialize)]
struct TemplateContext<T: serde::ser::Serialize> {
    title: String,
    url: &'static str,
    data: Option<T>
}

#[post( "/login/auth" )]
fn do_login( mut cookies: rocket::http::Cookies ) -> Redirect {
    let cookie = rocket::http::Cookie::new( "auth", "yes" );
    cookies.add_private( cookie );

    Redirect::to( "/" )
}

#[get( "/login" )]
fn login( ) -> Template {
    let ctx = TemplateContext {
        title: "Login".into(),
        url: BASE_URL,
        data: None::<i32>
    };

    Template::render( "login", &ctx )
}

#[get( "/", rank = 2 )]
fn index( ) -> Redirect {
    Redirect::to( "/login" )
}

#[get( "/", rank = 1 )]
fn index_user( u: User ) -> Redirect {
    Redirect::to( "/cargos" )
}


#[get( "/cargos" )]
fn cargos( u: User, db: DbConn ) -> Template {
    use application::services::CargoService;
    use domain::models::*;
    use infrastructure::repositories::diesel::DatabaseRepository;

    let service = CargoService::new( DatabaseRepository::<Cargo>::new( & db ) );
    let cargos = service.find_all().unwrap_or_else( |_| vec![] );

    let ctx = TemplateContext {
        title: "Dashboard :: Cargos".into(),
        url: BASE_URL,
        data: Some( cargos )
    };

    Template::render( "cargos", &ctx )
}

#[get( "/cargo/<id>" )]
fn cargo( u: User, id: i32, db: DbConn ) -> Option<Template> {
    use application::services::CargoService;
    use domain::models::*;
    use infrastructure::repositories::diesel::DatabaseRepository;

    let service = CargoService::new( DatabaseRepository::<Cargo>::new( & db ) );
    let cargo = match service.find( id ) {
        Ok( cargo ) => cargo,
        _ => return None
    };


    let ctx = TemplateContext {
        title: format!( "Dashboard :: Cargo #{}", id ),
        url: BASE_URL,
        data: Some( cargo )
    };

    Some( Template::render( "cargo", &ctx ) )
}

// #[get( "/delivery/<id>" )]
// fn delivery( u: User, id: i32, db: DbConn ) -> Option<Template> {
//     use application::services::DeliveryService;
//     use domain::models::*;
//     use infrastructure::repositories::diesel::DatabaseRepository;

//     let service = DeliveryService::new( DatabaseRepository::<Delivery>::new( & db ) );
//     let delivery = match service.find( id ) {
//         Ok( delivery ) => delivery,
//         _ => return None
//     };

//     let ctx = TemplateContext {
//         title: format!( "Dashboard :: Delivery #{}", id ),
//         url: BASE_URL,
//         data: Some( delivery )
//     };

//     Some( Template::render( "delivery", &ctx ) )
// }

#[get( "/shop/<id>" )]
fn shop( u: User, id: i32, db: DbConn ) -> Option<Template> {
    use application::services::ShopService;
    use domain::models::*;
    use infrastructure::repositories::diesel::DatabaseRepository;

    let service = ShopService::new( DatabaseRepository::<Shop>::new( & db ) );
    let shop = match service.find( id ) {
        Ok( shop ) => shop,
        _ => return None
    };

    let ctx = TemplateContext {
        title: format!( "Dashboard :: Shop #{}", id ),
        url: BASE_URL,
        data: Some( shop )
    };

    Some( Template::render( "shop", &ctx ) )
}

#[get( "/static/<path..>" )]
fn files( path: std::path::PathBuf ) -> Option<rocket::response::NamedFile> {
    rocket::response::NamedFile::open( std::path::Path::new( "static/" ).join( path ) ).ok()
}

fn app() -> Result<()> {
    let database_url = std::env::var( "DATABASE_URL" )
                            .chain_err( || "Variable DATABASE_URL is not defined!" )?;

    let manager = ConnectionManager::<PgConnection>::new( database_url );
    let pool = r2d2::Pool::builder().build( manager )
                            .chain_err( || "Failed to create connection pool!" )?;

    rocket::ignite()
        .mount( "/", routes![ index, index_user, do_login, login, cargos, cargo, shop, files ] )
        .manage( pool )
        .attach( Template::fairing() )
        .launch();

    Ok( () )
}

fn main() {
    if let Err( e ) = app() {
        println!( "{}", e.display_chain() );
    }
}
