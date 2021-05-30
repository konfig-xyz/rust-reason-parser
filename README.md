# Rust Diesel -> ReasonML 

## Todo
- [ ] - Add config with 'ignore' to ignore record keys for a certain type / module

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
    }
}
```
#### Output
```reason
module test {
  type t = {
    test_id: string,
    some_string: string,
    some_bool: bool,
    some_int: int,
    some_float: float
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
