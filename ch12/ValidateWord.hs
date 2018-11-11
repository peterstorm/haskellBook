module ValidateWord where

newtype Word' =
  Word' String
  deriving (Eq, Show)

isVowel :: Char -> Bool
isVowel x = x `elem` "aeuio"

mkWord :: String -> Maybe Word'
mkWord xs = if (length $ filter (not . isVowel) xs) > (length $ filter isVowel xs)
               then Just $ Word' xs 
               else Nothing
