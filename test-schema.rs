table! {
    use diesel::sql_types::*;

    test (test_id) {
        test_id -> Uuid,
        some_string -> Text,
        some_bool -> Bool,
        some_int -> Int4,
        some_float -> Float4,
    }
}
