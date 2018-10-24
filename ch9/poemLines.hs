module PoemLines where

firstSen  = "Tyger Tyger, burning bright\n"
seconSen  = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"

sentenses :: String
sentenses = firstSen ++ seconSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines x = go x []
  where go x acc
          | x == "" = reverse acc
          | otherwise = go (dropWhile (=='\n') $ dropWhile (>'\n') x) (takeWhile (/='\n') x : acc)

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
    "Are they equal? "
    ++ show (myLines sentenses == shouldEqual)
