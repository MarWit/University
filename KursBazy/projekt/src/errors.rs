error_chain! {
    foreign_links {
        DieselError(::diesel::result::Error);
    }
}
