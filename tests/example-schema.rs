// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "role"))]
    pub struct Role;
}

diesel::table! {
    use diesel::sql_types::*;

    companies (id) {
        id -> Uuid,
        company_name -> Text,
        created_at -> Timestamp,
        updated_at -> Timestamp,
        archived_at -> Nullable<Timestamp>,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::Role;

    members (id) {
        id -> Uuid,
        user_id -> Uuid,
        company_id -> Uuid,
        role -> Role,
        created_at -> Timestamp,
        updated_at -> Timestamp,
        archived_at -> Nullable<Timestamp>,
    }
}

diesel::table! {
    use diesel::sql_types::*;

    users (id) {
        id -> Uuid,
        email -> Text,
        password -> Text,
        blocked -> Bool,
        is_super_user -> Bool,
        verified -> Bool,
        created_at -> Timestamp,
        updated_at -> Timestamp,
        archived_at -> Nullable<Timestamp>,
    }
}

diesel::joinable!(members -> companies (company_id));
diesel::joinable!(members -> users (user_id));

diesel::allow_tables_to_appear_in_same_query!(companies, members, users,);

