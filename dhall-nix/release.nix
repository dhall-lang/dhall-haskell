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
          dhall-nix = haskellPackagesNew.callPackage ./default.nix { };
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
        testLam = dhallToNix "λ(x : Bool) → x";
        testApp = dhallToNix "λ(f : Bool → Bool) → λ(x : Bool) → f x";
        testLet = dhallToNix "λ(b : Bool) → let x = b in x";
        testAnnot = dhallToNix "True : Bool";
        testBoolLit = dhallToNix "True";
        testBoolAnd = dhallToNix "λ(l : Bool) → λ(r : Bool) → l && r";
        testBoolOr = dhallToNix "λ(l : Bool) → λ(r : Bool) → l || r";
        testBoolEQ = dhallToNix "λ(l : Bool) → λ(r : Bool) → l == r";
        testBoolNE = dhallToNix "λ(l : Bool) → λ(r : Bool) → l != r";
        testBoolIf = dhallToNix "λ(x : Bool) → if x then True else False";
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
        testIntegerLit = dhallToNix "123";
        testTextLit = dhallToNix ''"ABC"'';
        testTextAppend = dhallToNix "λ(x : Text) → λ(y : Text) → x ++ y";
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
        testCombine = dhallToNix ''
            λ(x : { foo : { bar : Text } })
          → λ(y : { foo : { baz : Bool } })
          → x ∧ y
        '';
        testRecord = dhallToNix "{ foo = 1, bar = True}";
      in
        assert (testLam true == true);
        assert (testApp (b : b) true == true);
        assert (testLet true == true);
        assert (testAnnot == true);
        assert (testBoolLit == true);
        assert (testBoolAnd true false == false);
        assert (testBoolOr true false == true);
        assert (testBoolEQ true false == false);
        assert (testBoolNE true false == true);
        assert (testBoolIf true == true);
        assert (testNaturalLit == 123);
        assert (testNaturalFold 123 == 246);
        assert (testNaturalBuild true == 1);
        assert (testNaturalIsZero 3 == false);
        assert (testNaturalEven 3 == false);
        assert (testNaturalOdd 3 == true);
        assert (testNaturalPlus 2 3 == 5);
        assert (testNaturalTimes 2 3 == 6);
        assert (testIntegerLit == 123);
        assert (testTextLit == "ABC");
        assert (testTextAppend "ABC" "DEF" == "ABCDEF");
        assert (testListLit == [1 2 3]);
        assert (testListBuild true == [1 2 3]);
        assert (testListFold (x : y: x + y) 0 == 6);
        assert ((testCombine { foo.baz = true; } { foo.bar = "ABC"; }).foo.baz == true);
        assert (testRecord.bar == true);
        pkgs.stdenv.mkDerivation {
          name = "tests-pass";

          buildCommand = "touch $out";
        };
  }
