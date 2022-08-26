{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Semigroup () where

import Dhall.Syntax.Instances.Functor ()
import Dhall.Syntax.Types

instance Semigroup (Chunks s a) where
    Chunks xysL zL <> Chunks         []    zR =
        Chunks xysL (zL <> zR)
    Chunks xysL zL <> Chunks ((x, y):xysR) zR =
        Chunks (xysL ++ (zL <> x, y):xysR) zR

instance Semigroup Directory where
    Directory components0 <> Directory components1 =
        Directory (components1 <> components0)

instance Semigroup File where
    File directory0 _ <> File directory1 file =
        File (directory0 <> directory1) file

instance Semigroup ImportType where
    Local prefix file0 <> Local Here file1 = Local prefix (file0 <> file1)

    Remote (URL { path = path0, ..}) <> Local Here path1 =
        Remote (URL { path = path0 <> path1, ..})

    Local prefix file0 <> Local Parent file1 =
        Local prefix (file0 <> parent <> file1)

    Remote (URL { path = path0, .. }) <> Local Parent path1 =
        Remote (URL { path = path0 <> parent <> path1, .. })

    import0 <> Remote (URL { headers = headers0, .. }) =
        Remote (URL { headers = headers1, .. })
      where
        importHashed0 = Import (ImportHashed Nothing import0) Code

        headers1 = fmap (fmap (importHashed0 <>)) headers0

    _ <> import1 =
        import1

instance Semigroup ImportHashed where
    ImportHashed _ importType0 <> ImportHashed hash importType1 =
        ImportHashed hash (importType0 <> importType1)

instance Semigroup Import where
    Import importHashed0 _ <> Import importHashed1 code =
        Import (importHashed0 <> importHashed1) code

parent :: File
parent = File { directory = Directory { components = [ ".." ] }, file = "" }
