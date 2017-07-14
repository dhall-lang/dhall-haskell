let
  region = "us-west-1";

in
  { machine = { resources, ... }: {
      deployment = {
        targetEnv = "ec2";

        ec2 = {
          inherit region;

          inherit (resources.ec2KeyPairs) keyPair;

          instanceType = "t2.nano";
        };
      };
    };

    resources.ec2KeyPairs.keyPair = { inherit region; };
  }
