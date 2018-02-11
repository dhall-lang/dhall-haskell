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
          ebsInitialRootDiskSize = 20;

          inherit region;

          inherit (resources.ec2KeyPairs) keyPair;

          instanceType = "t2.small";
        };
      };
    };

    resources.ec2KeyPairs.keyPair = { inherit region; };
  }
