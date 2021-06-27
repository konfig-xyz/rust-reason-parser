# Rust Diesel -> ReasonML 

## Before First Run
Pre-requisites: 
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

## Usage
Pass in a filename and config file to the `stack run` command and get Reason 
parsed output. See `example-config.yaml` for an example config file.

```
stack run {example-config.yaml} {example-schema.rs}
```

#### Input
`example-config.yaml`
```yaml
types:
  aliases:
    - uuid->string
  base:
    - Uuid->uuid
    - Text->string
    - Bool->bool
    - Int4->int
    - Float4->float
  nested:
    - Array->array
    - Nullable->option
annotations:
  alias-ppx:
    - decco
  type-ppx:
    - decco
    - bs.deriving jsConverter
hiding:
  tables: 
    - hide_me 
  keys: 
    - hidden_id
  qualified:
    qualified_hide:
    - qualified_field
    - another_qualified_field
```

`example-schema.rs`
```rust
table! {
    use diesel::sql_types::*;

    hide_me (hide_me_id) {
        hide_me_id -> Uuid,
    }
}

table! {
    use diesel::sql_types::*;

    qualified_shown (test_id) {
        qualified_field -> Text,
    }
}

table! {
    use diesel::sql_types::*;

    qualified_hide (test_id) {
        qualified_field -> Text,
        another_qualified_field -> Text,
    }
}

table! {
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
```
#### Output
```reason
[@decco]
type uuid = string;

// module HideMe = { };

module QualifiedShown = {
  [@decco]
  [@bs.deriving jsConverter]
  type t = {
    qualifiedField: string,
  };
};

module QualifiedHide = {
  [@decco]
  [@bs.deriving jsConverter]
  type t = {
    // qualifiedField: string,
    // anotherQualifiedField: string,
  };
};

module Test = {
  [@decco]
  [@bs.deriving jsConverter]
  type t = {
    testId: uuid,
    // hiddenId: uuid,
    someString: string,
    someBool: bool,
    someInt: int,
    someFloat: float,
    someArray: array(string),
    someOption: option(string),
  };
};
```

## Development

- Build
    ```bash
    stack build
    ```
- Build & Run
    ```bash
    stack run {config.yaml} {filename.rs} 
    ```
