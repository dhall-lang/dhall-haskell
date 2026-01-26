-- The purpose of this test is to verify that `dhall freeze --cached` is
-- idempotent and doesn't attempt to resolve the `missing` import
  missing
    sha256:27abdeddfe8503496adeb623466caa47da5f63abd2bc6fa19f6cfcb73ecfed70
? ./True.dhall
