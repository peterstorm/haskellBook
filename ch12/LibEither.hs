module LibEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) b = a : b
        f _        b = b

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right b) a = b : a
        f _         a = a

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b)  = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\a -> Nothing) (\b -> Just $ f b)
