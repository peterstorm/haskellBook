module DayOfWeek where

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = True

instance Eq Date where
  (==) (Date dayOfWeek dayOfMonth) (Date dayOfWeek' dayOfMonth') =
         dayOfWeek == dayOfWeek'
      && dayOfMonth == dayOfMonth'


