# Rust Diesel -> ReasonML 

## Rationale
For more simple CRUD apps, there is often a near 1-1 mapping of types from DB,
to API, to FE. This packages tries to take advantage of this fact, and create
this mapping. Specifically from a Diesel / Rust `schema.rs` file, to a single
file, that outputs ReasonML types in the conventional `Module.t` notation.
It does so with reasonable flexibilty, allowing for:
- Specifiying alias types
- Specifiying mappings based on types and fieldnames (`table.field`)
- Specifiying mappings for nested types
- Add annotations (ppx's) for aliases and for types
- Hide:
  - Modules (tables)
  - Fieldnames
  - Qualified types (`table.field`)


## Usage
### Using Docker
We provide a docker image that's synced with latest master, to make the process
of diffing / generating these files easier. Given a project with the following
folder structure:

```
xyz
- app
-- src
---- schema.rs
- config.yaml
```

One should be able to run the following command:
```
docker run -v $(pwd):/mnt/xyz konfigxyz/rust-reason-parser /mnt/xyz/config.yaml /mnt/xyz/app/src/schema.rs
```
It works by mounting the entire project into the container, and subsequently
running the parser on with the provided input.

### Building from scratch

Pre-requisites: 
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

Pass in a filename and config file to the `stack run` command and get Reason 
parsed output. See `example-config.yaml` for an example config file.

```
stack run {example-config.yaml} {example-schema.rs}
```

### Input
`example-config.yaml`
```yaml
language: reason
types:
  aliases:
    - uuid->string
  containerized:
    - arrayT->array
    - listT->list
    - optionT->option
  base:
    - Uuid->uuid
    - Text->string
    - Bool->bool
    - Int4->int
    - Float4->float
  nested:
    - Array->array
    - Nullable->option
  qualified:
    - test.some_string->someRandomTypeName
annotations:
  key-ppx: "@decco.key(\"{}\")"
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
    @decco.key("qualified_field") qualifiedField: string,
  };

  [@decco]
  type arrayT = array(t);

  [@decco]
  type listT = list(t);

  [@decco]
  type optionT = option(t);
};

module QualifiedHide = {
  [@decco]
  [@bs.deriving jsConverter]
  type t = {
    // @decco.key("qualified_field") qualifiedField: string,
    // @decco.key("another_qualified_field") anotherQualifiedField: string,
  };

  [@decco]
  type arrayT = array(t);

  [@decco]
  type listT = list(t);

  [@decco]
  type optionT = option(t);
};

module Test = {
  [@decco]
  [@bs.deriving jsConverter]
  type t = {
    @decco.key("test_id") testId: uuid,
    // @decco.key("hidden_id") hiddenId: uuid,
    @decco.key("some_string") someString: someRandomTypeName,
    @decco.key("some_bool") someBool: bool,
    @decco.key("some_int") someInt: int,
    @decco.key("some_float") someFloat: float,
    @decco.key("some_array") someArray: array(string),
    @decco.key("some_option") someOption: option(string),
  };

  [@decco]
  type arrayT = array(t);

  [@decco]
  type listT = list(t);

  [@decco]
  type optionT = option(t);
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

## Configuration Keys (-vv)

#### Language
Either `reason` or `rescript`.

#### Types.Aliases 

To keep the output self-contained, we allow for the specification of alias
types, that get printed at the top of the output.

#### Types.Containerized 

We found quite often we would need array types next to the regular ones, and
those would need to be annotated with our PPX's. This is messy and clutters
this approach where this file is / stays auto-generated.

#### Types.Base 

These are the base mappings. Based on the `value` of the type in the
`schema.rs`, we map the `value` of the type over. The name of the type get's
passed as-is, only converted to camel-case.

#### Types.Nested 

Whenever we encounter something like `Nullable<foo>` we need to know what to
map it too. These mappings can be specified here. Note that **they recurse**.
So given the configuration above, and the input type
`Nullable<Array<Nullable<Int4>>`, we would generate
`option(array(option(int)))`. 

#### Types.Nested 

There may be cases where you don't want to switch based on the type's `value`,
but rather on its `name`. For instance, when you want to save an convert a
`string` to a `variant` type only relevant to the FE. This is where you would
do that.

#### Annotations.(Alias-PPX | Type-PPX | Containerized-PPX) 

PPX annotations can be used to annotate types so that they automatically get
some extra nice-ties. Such as using
[decco](https://github.com/reasonml-labs/decco) for automatic JSON conversion,
or [bs-pancake](https://github.com/rolandpeelen/bs-pancake) to automatically
generate lenses for each record entry. There are respectively intended for
either aliases (which are printed at the top), or for the types themselves.
Some PPX's, like [decco](https://github.com/reasonml-labs/decco) require a sort
of bottom-up approach, where every type in a record is also annotated itself.
Hence the `alias-ppx` field. The `containerized-ppx` is the latest addition for
more flexibility.

There is one additional annotation that has some special syntax. I've found that when using things like Decco, or Spice, while we want to use camelCased things locally, it could be that the database tables are named with something that is more used in your backend language (snake_case, PascalCase, whichever - I think the PG default is snake_case). You can use this key to still parse into camelCase, by annotating the original key with: `"@decco.key(\"{}\")"`. Note a few things:
- The entire string is escaped, because yaml doesn't allow `@`.
- The content within `{}` will be replaced with the original type-name.
- You can enter anything there - so `"@spice.key(\"{}\")"` will also work.
- Default Yaml escaping will apply

#### Hiding.Tables 

If the API you're building has some tables that are not to be exposed to the
FE, here's where you would specify them. They'll be commented out in the
output. Given that Reason will try to convert as-little as possible, the
comments will automatically dissapear. However, for the more full-stack
oriented, it might be nice to keep it in there, hence commented as opposed to
deleted.

#### Hiding.Keys 

Sometimes one doesn't want to hide a full `table`, but instead a `key` that
occurs on a bunch of tables. For instance a `userId` or `companyId`.

#### Hiding.Qualified 

This is the more specific variant to `tables` / `keys`. It allows for the full
specification of hiding (`user.password`) for instance.

**NOTE** - There is a difference in qualified notation between `types` and
`hiding`. Reasoning here is that hiding multiple elements from a type is more
common than having multiple `convert-by-typename` elements.


## TODO
- [ ] - Build 'the' definitive mapping as a good default
- [ ] - Build this in CI
- [ ] - Tests...
- [ ] - Homebrew / ... ?
