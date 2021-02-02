module Simple where

import Data.Map 

mycollection :: Map Int String 
mycollection = empty 

double :: Num a => a -> a
double x = x * 2
