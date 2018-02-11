let
  region = "us-west-1";

in
  { ipfs = { resources, ... }: {
      deployment = {
        targetEnv = "ec2";

        ec2 = {
          inherit region;

          inherit (resources.ec2KeyPairs) keyPair;

          instanceType = "t2.nano";
        };
      };
    };

    hydra = { resources, ... }: {
      deployment = {
        targetEnv = "ec2";

        ec2 = {
          inherit region;

          inherit (resources.ec2KeyPairs) keyPair;

          instanceType = "t2.micro";
        };
      };
    };

    resources.ec2KeyPairs.keyPair = { inherit region; };
  }
