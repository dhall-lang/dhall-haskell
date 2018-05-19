[ http://example.com/someFile.dhall
, https://john:doe@example.com:8080/foo/bar?qux=0#xyzzy
, http://prelude.dhall-lang.org/package.dhall
, https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude
, https://127.0.0.1/index.dhall
, https://[::]/index.dhall
, https://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]/tutorial.dhall

  -- Yes, this is legal
, https://-._~%2C!$&'()*+,;=:@-._~%2C!$&'()*+,;=:/foo?/-._~%2C!$&'()*+,;=:@/?#/-._~%2C!$&'()*+,;=:@/?
]
