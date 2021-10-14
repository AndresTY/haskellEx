module Expo where
	
expo :: Integer -> Integer -> Integer

expo base 0 = 1

expo base n =
  if (n `mod` 2) == 0 then
    expo (base * (expo (n`div`2)) 2
  else error "hola"
