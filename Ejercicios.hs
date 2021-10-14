

sumA :: [Int] -> Int
sumA [] = 0
sumA(x:xs) = x + sumA xs 

lenGHT :: [Int] -> Int
lenGHT [] = 0
lenGHT (x:xs) = 1 + lenGHT xs


mnInt :: Int -> Int -> Int
mnInt n m =
  if n > m then
    m
  else 
    n


maxInt :: Int -> Int -> Int
maxInt n m =
  if n < m then
    m
  else 
    n


mnIntL :: [Int]-> Int
mnIntL [] = error "lista Vacia"
mnIntL [x] = x
mnIntl (x:xs) = mnInt (mnIntL xs) x  

maxIntL :: [Int]-> Int 
maxIntL [] = error "ugmm"
maxIntL [x] = x
maxIntl (x:xs) = maxInt (maxIntL xs) x 
  

removeFST :: [Int] -> Int -> [Int]
removeFST [] m = error "vacia"
removeFST (x:xs) m = 
  if x /= m then 
    x : removeFST xs m
  else 
    xs

ordena :: [Int] -> [Int]
ordena [] = []
ordena  xs = m : (ordena (removeFST xs m)) where m = mnIntL xs 

averAGE :: [Int] -> Float
averAGE [] = error "ok"
averAGE x = fromIntegral (sumA x) / fromIntegral(lenGHT x)

prefixSTR :: String -> String -> Bool
prefixSTR [] ys = True
prefixSTR (x:xs) [] = False
prefixSTR (x:xs) (y:ys) = x==y && prefixSTR xs ys

subSTR :: String -> String -> Bool
subSTR _ []  = True       
subSTR [] ys = False
subSTR (x:xs) (y:ys) = prefixSTR ys xs || ys == y:ys ||subSTR (tail xs) ys

subSTRb :: String -> String -> Bool
subSTRb [] ys = True
subSTRb (x:xs) [] = False
subSTRb (x:xs) (y:ys) = ((x==y) && (prefixSTR xs ys)) 
                        || (subSTRb (x:xs) ys)

diviDES :: Integral a => a -> a -> Bool
diviDES d n = rem n d == 0

ld :: Integral t => t -> t
ld n = ldF 2 n

ldF :: Integral t => t -> t -> t
ldF k n |diviDES k n   = k
        |k^2 > n       = n
        |otherwise     = ldF (k+1) n

primeTEST :: Integer -> Bool
primeTEST n | n<1 = error "problemas perro"
            | n == 1 = False
            |otherwise = ld n == n

primo :: Int -> [Int]
primo n | n<1       = error "problemas"
        | n == 1    =[]   
        | otherwise = p : primo(div n p) where p = ld n


funcionLB :: Int -> Int -> Int
funcionLB x y = x^2 + y^2 --composicion de funciones
{-
g :: Int -> Int
g 0 = 0
g x+1 = 2*(g (x-1))

h1 :: Int -> Int
h1 0 = 0
h1 x = 2*(h1 x)

h2 :: Int -> Int
h2 0 = 0
h2 x = h2(x+1)
-}
{-
root a b c =
  ((-b + sqrt(b*b - 4*a*c))/(2*a),(-b - sqrt(b*b - 4*a*c))/(2*a)) 
-}
root a b c = 
  let det = sqrt(b*b - 4*a*c)
      a = 2*a
  in ((-b + det)/a,(-b - det)/a) 
