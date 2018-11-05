module PityTheBool where

import Data.Int

data BigSmall = Big Bool
              | Small Bool
              deriving (Eq, Show)

-- the cardinality of the datatype is:
-- Big Bool = 2
-- Small Bool = 2
-- Total = 4

data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Eq, Show)

-- the cardinality of the datatype is:
-- Numa Int8 = 256
-- BoolyBool Bool = 2
-- Total = 258
