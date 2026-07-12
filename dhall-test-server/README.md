# dhall-test-server

`dhall-test-server` is a tiny local HTTP/HTTPS server used by the `dhall` test suite to remove runtime dependencies on public services (`httpbin.org`, `test.dhall-lang.org`, and `raw.githubusercontent.com` fixtures).

## Purpose

The server exists to make CI and local test runs deterministic and offline-capable for the networking-related tests.

It specifically supports tests for:

- custom request headers and `User-Agent` handling,
- transitive header forwarding,
- CORS compliance checks in chained imports,
- import caching behavior for a changing remote resource,
- static remote fixture imports.

## Runtime model

- `Dhall.Test.Server.withServers` starts both servers before tests and stops them afterwards.
- HTTP server: `127.0.0.1:18080`
- HTTPS server: `127.0.0.1:18443`
- Both only implement `GET`; unmatched routes return `404 Not Found` with body `Not Found\n`.

## TLS certificates

Self-signed certificate/key files are required and expected at one of:

- `./dhall-test-server/cert/cert.pem` and `./dhall-test-server/cert/key.pem`, or
- `../dhall-test-server/cert/cert.pem` and `../dhall-test-server/cert/key.pem`

If neither pair exists, startup fails.

## Shared response conventions

- Successful responses: `200 OK`
- `Content-Type` is always set:
  - `application/json` for `/user-agent` and `/headers`
  - `text/plain` for Dhall/text routes
- CORS helpers add `Access-Control-Allow-Origin` when configured for that route

## Stateful behavior

`/random-string` is intentionally stateful:

- an internal counter starts at `0` for each `withServers` run,
- each request increments the counter and returns:
  - `dhall-test-random-string-<n>\n`

This is used by cache tests to verify that repeated imports in one evaluation use cached results.

---

## Full route specification

### HTTPS routes (`https://localhost:18443/...`)

### User agent/httpbin-compatible subset

- `GET /user-agent`
  - Reads `User-Agent` header (if present)
  - Body:
    - with header: `{\n  "user-agent": "<value>"\n}\n`
    - without header: `{\n  "user-agent": "none_given"\n}\n`


### Random string

- `GET /random-string`
  - Body: `dhall-test-random-string-<n>\n` (counter increments per request)

### Header forwarding helper

- `GET /foo`
  - Requires request header `Test` (case-insensitive) with value `example`
  - Success body: `./bar`
  - Otherwise `404`

- `GET /bar`
  - Same `Test: example` requirement
  - Success body: `True`
  - Otherwise `404`

### CORS value endpoints

- `GET /cors/AllowedAll.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `42`

- `GET /cors/OnlyOther.dhall`
  - Header: `Access-Control-Allow-Origin: https://localhost:28080`
  - Body: `42`

- `GET /cors/Empty.dhall`
  - Header: `Access-Control-Allow-Origin: ` (empty value)
  - Body: `42`

- `GET /cors/NoCORS.dhall`
  - No `Access-Control-Allow-Origin` header
  - Body: `42`

- `GET /cors/Null.dhall`
  - Header: `Access-Control-Allow-Origin: null`
  - Body: `42`

- `GET /cors/SelfImportAbsolute.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `https://127.0.0.1:18443/cors/NoCORS.dhall`

- `GET /cors/SelfImportRelative.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `./NoCORS.dhall`

- `GET /cors/TwoHopsFail.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `https://localhost:18443/tests/import/data/cors/OnlySelf.dhall`

- `GET /cors/TwoHopsSuccess.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `https://localhost:18443/tests/import/data/cors/OnlyGithub.dhall`

---

### HTTP routes (`https://localhost:18443/...` and `https://127.0.0.1:18443/...`)

### User agent/httpbin-compatible subset

- `GET /user-agent`
  - Same behavior as HTTPS `/user-agent`

### Random string

- `GET /random-string`
- `GET /foo/../random-string`
  - Both return `dhall-test-random-string-<n>\n`

### Header forwarding helper

- `GET /foo` with `Test: example` -> `./bar`, else `404`
- `GET /bar` with `Test: example` -> `True`, else `404`

### Static fixture routes

- `GET /tests/import/data/example.txt` -> `Hello, world!\n`
- `GET /tests/import/data/simple.dhall` -> `3`
- `GET /tests/import/data/simpleLocation.dhall` -> `./simple.dhall as Location`

- `GET /tests/import/data/Prelude/List/length.dhall` -> `List/length`

- `GET /nadrieril/dhall/tests/import/success/unit/asLocation/Canonicalize3A.dhall`
  -> `./../bar/import.dhall as Location`

- `GET /nadrieril/dhall/tests/import/success/unit/asLocation/Canonicalize5A.dhall`
  -> `./foo/../../bar/import.dhall as Location`

- `GET /nadrieril/dhall/tests/import/success/unit/asLocation/MissingA.dhall`
  -> `missing as Location`

- `GET /nadrieril/dhall/tests/import/success/unit/asLocation/EnvA.dhall`
  -> `env:HOME as Location`

- `GET /nadrieril/dhall/tests/import/success/unit/bar/import.dhall`
  -> `2`

- `GET /tests/import/success/customHeadersA.dhall`
  -> `https://localhost:18443/user-agent using [ { mapKey = "User-Agent", mapValue = "Dhall" } ] as Text`

### CORS indirection fixture routes (`/tests/import/data/cors/...`)

Each of these returns a Dhall import URL as plain text and also sets `Access-Control-Allow-Origin: *`:

- `AllowedAll.dhall` -> `https://127.0.0.1:18443/cors/AllowedAll.dhall`
- `OnlyGithub.dhall` -> `https://127.0.0.1:18443/cors/OnlyGithub.dhall`
- `OnlySelf.dhall` -> `https://127.0.0.1:18443/cors/OnlySelf.dhall`
- `OnlyOther.dhall` -> `https://127.0.0.1:18443/cors/OnlyOther.dhall`
- `Empty.dhall` -> `https://127.0.0.1:18443/cors/Empty.dhall`
- `NoCORS.dhall` -> `https://127.0.0.1:18443/cors/NoCORS.dhall`
- `Null.dhall` -> `https://127.0.0.1:18443/cors/Null.dhall`
- `SelfImportAbsolute.dhall` -> `https://127.0.0.1:18443/cors/SelfImportAbsolute.dhall`
- `SelfImportRelative.dhall` -> `https://127.0.0.1:18443/cors/SelfImportRelative.dhall`

### CORS value/import endpoints (`/cors/...`)

- `GET /cors/AllowedAll.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `42`

- `GET /cors/OnlySelf.dhall`
  - Header: `Access-Control-Allow-Origin: https://127.0.0.1:18443`
  - Body: `42`

- `GET /cors/OnlyOther.dhall`
  - Header: `Access-Control-Allow-Origin: https://localhost:28080`
  - Body: `42`

- `GET /cors/OnlyGithub.dhall`
  - Header: `Access-Control-Allow-Origin: https://localhost:18443`
  - Body: `42`

- `GET /cors/Empty.dhall`
  - Header: `Access-Control-Allow-Origin: ` (empty value)
  - Body: `42`

- `GET /cors/NoCORS.dhall`
  - No `Access-Control-Allow-Origin`
  - Body: `42`

- `GET /cors/Null.dhall`
  - Header: `Access-Control-Allow-Origin: null`
  - Body: `42`

- `GET /cors/SelfImportAbsolute.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `https://127.0.0.1:18443/cors/NoCORS.dhall`

- `GET /cors/SelfImportRelative.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `./NoCORS.dhall`

- `GET /cors/TwoHopsFail.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `https://localhost:18443/tests/import/data/cors/OnlySelf.dhall`

- `GET /cors/TwoHopsSuccess.dhall`
  - Header: `Access-Control-Allow-Origin: *`
  - Body: `https://localhost:18443/tests/import/data/cors/OnlyGithub.dhall`

## Notes on origin design

The server intentionally uses both `localhost` and `127.0.0.1` in URLs to create distinct origins while staying local. This is required to exercise Dhall's CORS checks in realistic cross-origin and same-origin combinations.
