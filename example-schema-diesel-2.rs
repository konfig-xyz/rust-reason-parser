// @generated automatically by Diesel CLI.

diesel::table! {
    use diesel::sql_types::*;

    hide_me (hide_me_id) {
        hide_me_id -> Uuid,
    }
}

diesel::table! {
    use diesel::sql_types::*;

    qualified_shown (test_id) {
        qualified_field -> Text,
    }
}

diesel::table! {
    use diesel::sql_types::*;

    qualified_hide (test_id) {
        qualified_field -> Text,
        another_qualified_field -> Text,
    }
}

diesel::table! {
    use diesel::sql_types::*;

    test (test_id) {
        test_id -> Uuid,
        hidden_id -> Uuid,
        some_string -> Text,
        some_bool -> Bool,
        some_int -> Int4,
        some_float -> Float4,
        some_array -> Array<Text>,
        some_option -> Nullable<Text>,
    }
}

diesel::joinable!(foo -> bar (bar_id));

diesel::allow_tables_to_appear_in_same_query!(
    bar,
);
