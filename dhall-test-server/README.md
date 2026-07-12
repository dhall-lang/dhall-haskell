# dhall-test-server

`dhall-test-server` implements the local Web server required by the `dhall` test suite.

The Web server responds on HTTP and HTTPS with identical endpoints.

The HTTPS server uses a self-signed certificate.
The test suite configures the import system to accept self-signed certificates.

The APIs include only GET requests and serve:

- static files from `dhall/dhall-lang/tests/import/...`
- routes with custom request headers and `User-Agent` handling
- routes with specific CORS headers
- routes with specific custom behavior

For the full documentation on the required endpoints, see [`dhall/dhall-lang/tests/README.md`](./dhall-lang/blob/main/tests/README.md).

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

## Implementation of `/random-string`

- an internal counter starts at `0` for each `withServers` run
- each request increments the counter and returns `dhall-test-random-string-<n>\n`
