-- The purpose of this test is to verify that `dhall freeze --cached` is
-- idempotent and doesn't attempt to resolve the `missing` import
  missing
    sha256:797a7b7d0711361d75477c48d4442351120f1493a0844ea58180c3b41d54fbfc
? ./True.dhall
