stack run tests/example-config-reason.yaml tests/example-schema.rs > tests/example-config-reason-snapshot.txt
stack run tests/example-config-rescript.yaml tests/example-schema.rs > tests/example-config-rescript-snapshot.txt

git diff --exit-code tests
