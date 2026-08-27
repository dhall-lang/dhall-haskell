This example is taken from github:uwedeportivo/ds-dhall

Run `dhall --file generate-example.dhall`. This is a medium customization tree
over a large generated record/schema. 

Warning: earlier version of `dhall` (1.42.3 and earlier) will OOM on this benchmark.

See `../README.md` for phase-benchmark interpretation (prep warms disk caches;
timed resolve uses the semantic cache off).

Subdirectory `base/` holds the large generated record/schema used by the
customization combinators.
