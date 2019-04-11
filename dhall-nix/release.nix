let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev = "1d4de0d552ae9aa66a5b8dee5fb0650a4372d148";

    sha256 = "09qx58dp1kbj7cpzp8ahbqfbbab1frb12sh1qng87rybcaz0dz01";

    outputSha256 = "0xpqc1fhkvvv5dv1zmas2j1q27mi7j7dgyjcdh82mlgl1q63i660";
  };

  mass = function: names: haskellPackagesNew: haskellPackagesOld:
    let
      toNameValue = name: {
        inherit name;

        value = function haskellPackagesOld."${name}";
      };

    in
      builtins.listToAttrs (map toNameValue names);

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override (old: {
          overrides =
            let
              dontCheck =
                mass pkgs.haskell.lib.dontCheck [
                  "adjunctions"
                  "base-orphans"
                  "base64-bytestring"
                  "cereal"
                  "blaze-builder"
                  "neat-interpolation"
                  "pureMD5"
                  "pem"
                  "lens"
                  "th-orphans"
                  "mockery"
                  "megaparsec"
                  "lens-family-th"
                  "network-uri"
                  "invariant"
                  "interpolate"
                  "http-types"
                  "parsers"
                  "dhall"
                  "aeson"
                  "half"
                  "generic-deriving"
                  "distributive"
                  "deriving-compat"
                  "monad-control"
                  "logging-facade"
                  "bifunctors"
                  "exceptions"
                  "cborg-json"
                  "cryptohash-sha512"
                  "Diff"
                  "hashable"
                  "hnix"
                  "hnix-store-core"
                  "optparse-generic"
                  "serialise"
                  "SHA"
                  "these"
                  "unordered-containers"
                  "vector"
                ];

              extension = haskellPackagesNew: haskellPackagesOld: {
                dhall-nix =
                  pkgs.haskell.lib.failOnAllWarnings
                    (pkgs.haskell.lib.justStaticExecutables
                      haskellPackagesOld.dhall-nix
                    );
              };

            in
              pkgs.lib.fold
                pkgs.lib.composeExtensions
                (old.overrides or (_: _: {}))
                [ (pkgs.haskell.lib.packagesFromDirectory { directory = ./nix; })
                  dontCheck
                  extension
                ];
        }
      );
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

  inherit (pkgs) dhallToNix;

in
  { dhall-nix = pkgs.haskellPackages.dhall-nix;

    shell = pkgs.haskellPackages.dhall-nix.env;

    # Test that various Dhall to Nix conversions work
    tests =
      let
        testConst = dhallToNix "Type";
        testLam = dhallToNix "λ(x : Bool) → x";
        testPi = dhallToNix "Bool → Bool";
        testApp = dhallToNix "λ(f : Bool → Bool) → λ(x : Bool) → f x";
        testLet = dhallToNix "λ(b : Bool) → let x = b in x";
        testAnnot = dhallToNix "True : Bool";
        testBool = dhallToNix "Bool";
        testBoolLit = dhallToNix "True";
        testBoolAnd = dhallToNix "λ(l : Bool) → λ(r : Bool) → l && r";
        testBoolOr = dhallToNix "λ(l : Bool) → λ(r : Bool) → l || r";
        testBoolEQ = dhallToNix "λ(l : Bool) → λ(r : Bool) → l == r";
        testBoolNE = dhallToNix "λ(l : Bool) → λ(r : Bool) → l != r";
        testBoolIf = dhallToNix "λ(x : Bool) → if x then True else False";
        testNatural = dhallToNix "Natural";
        testNaturalLit = dhallToNix "123";
        testNaturalFold = dhallToNix ''
            λ(x : Natural)
          → Natural/fold x Natural (λ(n : Natural) → 2 + n) 0
        '';
        testNaturalBuild = dhallToNix ''
            λ(b : Bool)
          → Natural/build
            ( λ(natural : Type)
            → λ(succ : natural → natural)
            → λ(zero : natural)
            → if b then succ zero else zero
            )
        '';
        testNaturalIsZero = dhallToNix "Natural/isZero";
        testNaturalEven = dhallToNix "Natural/even";
        testNaturalOdd = dhallToNix "Natural/odd";
        testNaturalToInteger = dhallToNix "Natural/toInteger";
        testNaturalShow = dhallToNix "Natural/show";
        testNaturalPlus = dhallToNix "λ(x : Natural) → λ(y : Natural) → x + y";
        testNaturalTimes = dhallToNix "λ(x : Natural) → λ(y : Natural) → x * y";
        testInteger = dhallToNix "Integer";
        testIntegerLit = dhallToNix "+123";
        testIntegerShow = dhallToNix "Integer/show";
        testDouble = dhallToNix "Double";
        testTextLit = dhallToNix ''"ABC"'';
        testInterpolation = dhallToNix ''λ(x : Text) → "ABC''${x}GHI"'' "DEF";
        testTextAppend = dhallToNix "λ(x : Text) → λ(y : Text) → x ++ y";
        testList = dhallToNix "List Natural";
        testListLit = dhallToNix "[1, 2, 3] : List Natural";
        testListAppend = dhallToNix ''
          λ(xs : List Natural) → λ(ys : List Natural) → xs # ys
        '';
        testListBuild = dhallToNix ''
            λ(b : Bool)
          → List/build
            Natural
            ( λ(list : Type)
            → λ(cons : Natural → list → list)
            → λ(nil : list)
            → if b then cons 1 (cons 2 (cons 3 nil)) else nil
            )
        '';
        testListFold = dhallToNix ''
            List/fold
            Natural
            ([1, 2, 3] : List Natural)
            Natural
        '';
        testListLength = dhallToNix "List/length Natural";
        testListHead = dhallToNix "List/head Natural";
        testListLast = dhallToNix "List/last Natural";
        testListIndexed = dhallToNix "List/indexed Natural";
        testListReverse = dhallToNix "List/reverse Natural";
        testOptional = dhallToNix "Optional";
        testOptionalLit = dhallToNix ''
            λ(b : Bool)
          → if b
            then ([0] : Optional Natural)
            else ([]  : Optional Natural)
        '';
        testOptionalFold = dhallToNix ''
          Optional/fold
          Natural
          ([1] : Optional Natural)
          Natural
        '';
        testOptionalBuild = dhallToNix ''
            λ(b : Bool)
          → Optional/build
            Natural
            ( λ(optional : Type)
            → λ(just : Natural → optional)
            → λ(nothing : optional)
            → if b then just 1 else nothing
            )
        '';
        testNone = dhallToNix "None Natural";
        testSome = dhallToNix "Some 4";
        testRecord = dhallToNix "{}";
        testRecordLit = dhallToNix "{ foo = 1, bar = True}";
        testUnion = dhallToNix "< Left : Natural | Right : Bool >";
        testUnionLit = dhallToNix "< Left = 2 | Right : Bool >";
        testCombine = dhallToNix ''
            λ(x : { foo : { bar : Text } })
          → λ(y : { foo : { baz : Bool } })
          → x ∧ y
        '';
        testCombineTypes = dhallToNix ''
          { foo : Text } ⩓ { bar : Bool, baz : Natural }
        '';
        testMerge = dhallToNix ''
            λ(r : < Left : Natural | Right : Bool >)
          → merge
            { Left = Natural/isZero, Right = λ(b : Bool) → b }
            r : Bool
        '';
        testField = dhallToNix "λ(r : { foo : Bool, bar : Text }) → r.foo";
        testProject = dhallToNix ''
          λ(r : { foo : Bool, bar : Text, baz : Natural }) → r.{ foo, bar }
        '';
      in
        assert (testConst == {});
        assert (testLam true == true);
        assert (testPi == {});
        assert (testApp (b : b) true == true);
        assert (testLet true == true);
        assert (testAnnot == true);
        assert (testBool == {});
        assert (testBoolLit == true);
        assert (testBoolAnd true false == false);
        assert (testBoolOr true false == true);
        assert (testBoolEQ true false == false);
        assert (testBoolNE true false == true);
        assert (testBoolIf true == true);
        assert (testNatural == {});
        assert (testNaturalLit == 123);
        assert (testNaturalFold 123 == 246);
        assert (testNaturalBuild true == 1);
        assert (testNaturalBuild false == 0);
        assert (testNaturalIsZero 0 == true);
        assert (testNaturalIsZero 3 == false);
        assert (testNaturalEven 2 == true);
        assert (testNaturalEven 3 == false);
        assert (testNaturalOdd 2 == false);
        assert (testNaturalToInteger 2 == 2);
        assert (testNaturalShow 2 == "2");
        assert (testNaturalOdd 3 == true);
        assert (testNaturalPlus 2 3 == 5);
        assert (testNaturalTimes 2 3 == 6);
        assert (testInteger == {});
        assert (testIntegerLit == 123);
        assert (testIntegerShow 2 == "+2");
        assert (testIntegerShow (-3) == "-3");
        assert (testDouble == {});
        assert (testTextLit == "ABC");
        assert (testInterpolation == "ABCDEFGHI");
        assert (testTextAppend "ABC" "DEF" == "ABCDEF");
        assert (testList == {});
        assert (testListLit == [1 2 3]);
        assert (testListAppend [1 2 3] [4 5 6] == [1 2 3 4 5 6]);
        assert (testListBuild false == []);
        assert (testListBuild true == [1 2 3]);
        assert (testListFold (x : y: x + y) 0 == 6);
        assert (testListLength [1 2 3] == 3);
        assert (testListLength [] == 0);
        assert (testListHead [1 2 3] == 1);
        assert (testListHead [] == null);
        assert (testListLast [1 2 3] == 3);
        assert (testListLast [] == null);
        assert (testListIndexed [2 3 5] == [
          { index = 0; value = 2; }
          { index = 1; value = 3; }
          { index = 2; value = 5; }
        ]);
        assert (testListReverse [1 2 3] == [3 2 1]);
        assert (testOptional {} == {});
        assert (testOptionalLit true == 0);
        assert (testOptionalLit false == null);
        assert (testOptionalFold (n : n) 0 == 1);
        assert (testOptionalBuild true == 1);
        assert (testOptionalBuild false == null);
        assert (testRecord == {});
        assert (testRecordLit == { foo = 1; bar = true; });
        assert (testUnion == {});
        assert (testUnionLit { Left = n : n == 0; Right = b : b; } == false);
        assert ((testCombine { foo.baz = true; } { foo.bar = "ABC"; }) == {
          foo = {
            baz = true;
            bar = "ABC";
          };
        });
        assert (testCombineTypes == {});
        assert (testMerge ({ Left, Right }: Left 2) == false);
        assert (testField { foo = true; bar = "ABC"; } == true);
        assert (testProject { foo = true; bar = "ABC"; baz = 1; } == {
          foo = true;
          bar = "ABC";
        });
        pkgs.stdenv.mkDerivation {
          name = "tests-pass";

          buildCommand = "touch $out";
        };
  }
