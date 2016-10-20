module Main where 

import Data.Char
import Control.Arrow

main :: IO()
main = undefined

excellize :: Int -> String
excellize n
  | n < 0 = error "negative value"
  | otherwise = uncurry excellize' $ n `divMod` 26 where
      excellize' :: Int -> Int -> String
      excellize' q 0 = replicate q 'Z'
      excellize' q r = replicate q 'Z' ++ [['A'..'Z'] !! (r-1)]

unexcellize :: String -> Int
unexcellize s
  -- | any (\c -> not $ isAlpha c && isUpper c) s = error "contains unknown charcter"
  | any ((not . uncurry (&&)) . (isAlpha &&& isUpper)) s = error "contains unknown charcter"
  | otherwise = sum $ (flip (-) ((ord 'A') - 1) . ord) <$> s
