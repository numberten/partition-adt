module Partition
    ( partitionADT
    ) where

import Data.Data
import qualified Data.Map.Lazy as M

-- | Takes a list of values and returns a map from strings
-- of their data constructors to lists of values with that
-- constructor.
--
-- e.g. partitionADT [Left 'a', Left 'b',  Right 10, Left 'c', Right 42]
--      ==
--      fromList [("Left",[Left 'c',Left 'b',Left 'a']),("Right",[Right 42,Right 10])]
--
-- If a data constructor is missing from the input list,
-- it will not have a key in the resulting map.
--
-- Lists in returned map respect order of values in input list.
--
-- TODO:
--  - Make missing data constructors return an empty list.
--  - Use a more restrictive type for map keys.
--
partitionADT :: Data a => [a] -> M.Map String [a]
partitionADT xs = M.map reverse
                $ go xs
                $ foldr (\k m -> M.insert k [] m) M.empty
                $ map show (constrs xs)
  where
    go [] acc = acc
    go (x:rest) acc = case (show $ toConstr x) `M.lookup` acc of
      Just vs -> go rest $ M.insert (show $ toConstr x) (x:vs) acc
      Nothing -> go rest $ M.insert (show $ toConstr x) [x] acc

    constrs :: Data a => [a] -> [Constr]
    constrs = constrs' undefined
    constrs' :: Data a => a -> [a] -> [Constr]
    constrs' bottom _ = dataTypeConstrs $ dataTypeOf bottom
