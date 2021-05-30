# Rust Diesel -> ReasonML 

## Todo
- [ ] - Add config with 'ignore' to ignore record keys for a certain type / module
- [ ] - Add config with custom type mappers (for for instance 'uuid')
- [ ] - Add config with custom ppx's to add

## Before First Run
Pre-requisites: 
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

## Usage
Pass in a filename to the `stack run` command and get Reason parsed output...

#### Input
See also `test-schema.rs`
```rust
table! {
    use diesel::sql_types::*;

    test (test_id) {
        test_id -> Uuid,
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
module Test {
  type t = {
    testId: string,
    someString: string,
    someBool: bool,
    someInt: int,
    someFloat: float,
    someArray: array(string),
    someOption: option(string)
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
    stack run {filename.rs} 
    ```
