# Rust Diesel -> ReasonML 

## Todo
- [x] - Add config with 'ignore' to ignore record keys for a certain type / module
- [ ] - Update config to include specified ignores (ie. x.y)

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
type: uuid: string;

module Test {
  type t = {
    testId: uuid,
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
    stack run {config.yaml} {filename.rs} 
    ```
