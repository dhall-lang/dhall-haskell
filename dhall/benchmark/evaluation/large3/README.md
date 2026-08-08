This example is taken from git@github.com:sourcegraph/deploy-sourcegraph-dhall-archived.git

The command `dhall --file pipeline.dhall` takes about 30 seconds. The text size of the normal form is about 193 MB.

Timed via `stack bench evaluation --ba '--pattern large3'` (resolve / typecheck / evaluation).
