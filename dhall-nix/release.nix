# You can build this repository using Nix by running:
#
#     $ nix-build -A dhall-nix release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A dhall-nix.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          dhall-nix =
            pkgs.haskell.lib.disableSharedExecutables
              (haskellPackagesNew.callPackage ./default.nix { });
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

  # Convert a Dhall expression (represented as a string) to the equivalent Nix
  # expression
  dhallToNix = code :
    let
      file = builtins.toFile "dhall-expr" code;

      drv = pkgs.stdenv.mkDerivation {
        name = "dhall-expr-as-nix";

        buildCommand = ''
          dhall-to-nix <<< "${file}" > $out
        '';

        buildInputs = [ pkgs.haskellPackages.dhall-nix ];
      };
    in
      import "${drv}";

in
  { dhall-nix = pkgs.haskellPackages.dhall-nix;

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
        testNaturalLit = dhallToNix "+123";
        testNaturalFold = dhallToNix ''
            λ(x : Natural)
          → Natural/fold x Natural (λ(n : Natural) → +2 + n) +0
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
        testNaturalPlus = dhallToNix "λ(x : Natural) → λ(y : Natural) → x + y";
        testNaturalTimes = dhallToNix "λ(x : Natural) → λ(y : Natural) → x * y";
        testInteger = dhallToNix "Integer";
        testIntegerLit = dhallToNix "123";
        testDouble = dhallToNix "Double";
        testTextLit = dhallToNix ''"ABC"'';
        testTextAppend = dhallToNix "λ(x : Text) → λ(y : Text) → x ++ y";
        testList = dhallToNix "List Integer";
        testListLit = dhallToNix "[1, 2, 3] : List Integer";
        testListBuild = dhallToNix ''
            λ(b : Bool)
          → List/build
            Integer
            ( λ(list : Type)
            → λ(cons : Integer → list → list)
            → λ(nil : list)
            → if b then cons 1 (cons 2 (cons 3 nil)) else nil
            )
        '';
        testListFold = dhallToNix ''
            List/fold
            Natural
            ([+1, +2, +3] : List Natural)
            Natural
        '';
        testListLength = dhallToNix "List/length Integer";
        testListHead = dhallToNix "List/head Integer";
        testListLast = dhallToNix "List/last Integer";
        testListIndexed = dhallToNix "List/indexed Integer";
        testListReverse = dhallToNix "List/reverse Integer";
        testOptional = dhallToNix "Optional";
        testOptionalLit = dhallToNix ''
            λ(b : Bool)
          → if b
            then ([0] : Optional Integer)
            else ([]  : Optional Integer)
        '';
        testOptionalFold = dhallToNix ''
          Optional/fold
          Integer
          ([1] : Optional Integer)
          Integer
        '';
        testRecord = dhallToNix "{}";
        testRecordLit = dhallToNix "{ foo = 1, bar = True}";
        testUnion = dhallToNix "< Left : Natural | Right : Bool >";
        testUnionLit = dhallToNix "< Left = +2 | Right : Bool >";
        testCombine = dhallToNix ''
            λ(x : { foo : { bar : Text } })
          → λ(y : { foo : { baz : Bool } })
          → x ∧ y
        '';
        testMerge = dhallToNix ''
            λ(r : < Left : Natural | Right : Bool >)
          → merge
            { Left = Natural/isZero, Right = λ(b : Bool) → b }
            r : Bool
        '';
        testField = dhallToNix "λ(r : { foo : Bool, bar : Text }) → r.foo";
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
        assert (testNaturalOdd 3 == true);
        assert (testNaturalPlus 2 3 == 5);
        assert (testNaturalTimes 2 3 == 6);
        assert (testInteger == {});
        assert (testIntegerLit == 123);
        assert (testDouble == {});
        assert (testTextLit == "ABC");
        assert (testTextAppend "ABC" "DEF" == "ABCDEF");
        assert (testList == {});
        assert (testListLit == [1 2 3]);
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
        assert (testMerge ({ Left, Right }: Left 2) == false);
        assert (testField { foo = true; bar = "ABC"; } == true);
        pkgs.stdenv.mkDerivation {
          name = "tests-pass";

          buildCommand = "touch $out";
        };
  }
