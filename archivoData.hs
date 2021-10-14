{-
march 24/2020
Sergio Arboleda University 
@version: 1.0.8
-}

{-Importa Data.List y System.IO-}{-Import Data.List and System.IO-}
import Data.List
import System.IO

{-defini si el nuemero es par o impar-}{-define if a number is odd or even-}
isOdd :: Int -> Bool

isOdd n 
	  | n  `mod` 2 == 0 = False
    | otherwise = True

isEven n = n `mod` 2 == 0 

{-realiza un switch para definir el grado segun la edad-}{-make a switch to define grade by the ages-}
queGrado :: Int -> String

queGrado age  | (age >= 5) && (age <= 6 ) = "Kinder"
              | (age > 6 ) && (age <= 10) = "Elemental"
              | (age > 10) && (age <= 14) = "Básica"
              | (age > 14) && (age <= 18) = "Bachillerato"
              | otherwise = "Universidad"

{-muestra la lista que ingresamos segun los parametros-}{-show the list que we input according to the parameters-}
darElementosLista :: [Int] -> String

darElementosLista [] = "Tu lista esta vacia" 
darElementosLista (x:[]) = "Tu lista empieza con " ++ show x 
darElementosLista (x:y:[]) = "Tu lista contiene " ++ show x ++ " y  " ++ show y  
darElementosLista (x:xs) = "El primer elemento es " ++ show x ++ " y el resto es " ++ show xs 

{-multiplica por 4 el numero que ingresemos-}{-make a factor of 4 with a number entered-}
veces4 :: Int -> Int
veces4 x = x * 4

{-tomo una lista de numeros a los cuales le aplica la multiplicacion por 4 a cada uno-}{-take a number list and make the factor of 4 with all numbers in the list-}
multPor4 :: [Int] -> [Int]
multPor4 [] = []
multPor4 (x:xs) = veces4 x : multPor4 xs 

{-evalua si las dos cadenas son identicas-}{-evaluates if 2 Strings are identic-}
sonCadenasIguales :: [Char] -> [Char] -> Bool 
sonCadenasIguales [] [] = True
sonCadenasIguales (x:xs) (y:ys) = x == y && sonCadenasIguales xs ys
sonCadenasIguales _ _ = False

{-la funcion num3Veces usa a doMult con el fin de encontrar la multiplicaion de 3 y 4 usando la funcion veces4-}{-the function num3Veces use doMult for find the multiplication-}
doMult :: (Int -> Int) -> Int
doMult funcion = funcion 3

num3Veces = doMult veces4

{-hace una funcion que permite sumar dos numeros, ademas crea diferentes funciones para que por ejemplo se pueda suma un lista con 3-}{-make a function that add 2 numbers and created functions that use it -}
sumFuncion :: Int -> (Int -> Int)
sumFuncion x y = x + y

suma3 = sumFuncion 3

cuatroMasTres = suma3 4

tresMasLista = map suma3 [0,1,2,3,4]

{-Evalua si un numero es primo y si asi es lo duplica, de lo contario no hace cambios al numero-}{-evaluates if a numbers is prime for duplicate it-}
doubleParNum y = 
    if (y `mod`2 /= 0)
    then y
    else y * 2

{-Crea una base de datos con los datos de un cliente, para despues usar una funcion que busca un tipo de informacion de la base de datos-}{-creater a data base of customers for them use a function that find the information -}
data Cliente = Cliente String String Double
     deriving Show

miguelSaavedra :: Cliente
miguelSaavedra = Cliente "Miguel Saavedra" "Calle 76" 18.50

sanchoPanza :: Cliente
sanchoPanza = Cliente "Sancho Panza" "Calle 72" 17.90

elBalance :: Cliente -> Double
elBalance (Cliente _ _ b) = b

{-crea una simulacion del juego priedra, papel o tijera-}{-make a simulation of the game-}
data PPT = Piedra | Papel | Tijera

tiro :: PPT -> PPT -> String
tiro Papel Piedra = "Papel vence a Piedra!"
tiro _ _ = "Error de Entrada"

{-Calcula el area de un circulo y de un rectangulo-}{-calculate the area of a circle and a rectangle-}
data Forma = Circulo Float Float Float | Rectangulo Float Float Float Float
    deriving Show

area :: Forma -> Float
area(Circulo _ _ r) = pi * r ^ 2
area(Rectangulo x y x2 y2) = (abs (x2 - x) )*(abs (y2 - y) )
{-
 - area(Rectangulo x y x2 y2) = (abs $ x2 - x)*(abs $ y2 - y)
 -}

areaCirculo    = area $ Circulo 30 20 10
areaRectangulo = area $ Rectangulo 10 10 100 100

{-hace la suma de 1 y 2 para mostrar la utilidad del $-}
sumValor = putStrLn (show (1+2) )
sumValor2 = putStrLn . show $ 1 + 2

{-crea un base de datos de los empleados, que evalua si tomas es edgar y muestra todos los datos de Tomas Bill-}{-created a data base of employee that evaluate a condicions-}
data Empleado = Empleado { nombre :: String, 
                           position :: String,
                           idNum :: Int
                         } deriving (Eq, Show)

tomasBill = Empleado {nombre="Tomas Bill", position="Supervisor", idNum = 2000 }
edgarHard = Empleado {nombre="Edgar Hard", position="Ventas", idNum = 4003 }

esTomasEdgar = tomasBill == edgarHard 

tomasBilldata = show tomasBill

{-define unas letras que expresan el tamaño de una camisa, evalua las tallas y define el correspondiente tallaje para cada una-}{-define a words that say the size of a shirt-}
data TallaCamisa = S | M | L
     
instance Eq TallaCamisa where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show TallaCamisa where
    show S = "Pequeña"
    show M = "Media"
    show L = "Grande"

pequeDisponible = S `elem` [S,L,M]

laTalla = show S


{-Genera un saludo personalizado con el nombre que se ingrese-}{-generate a personalized messenger with a name-}
diHola = do
    putStrLn "Cual es tu nombre"
    nombre <- getLine
    putStrLn $ "Hola que tal estas " ++ nombre

{-crea un archivo de texto con los parametros establecidos-}{-creater a txt file with a paremeters-}
escribirArchivo = do
   archivo <- openFile "file.txt" WriteMode
   hPutStrLn archivo ("Texto en el archivo .... !!! Paradigmas de Programacion")
   hClose archivo

{-lee el archivo anteriormente creado-}{-read a txt file-}
leerArchivo = do
   archivo2 <- openFile "file.txt" ReadMode
   contenido <- hGetContents archivo2
   putStr contenido
   hClose archivo2


{-la funcion examen genera una lista de la suma de la funcion examen y la cola del mismo-}{-make a exam list that is the sum of the function exam and the tail -}
examen = 1 : 1 : [a + b | (a, b) <- zip examen (tail examen)]  -- not start the exam because it doesn't end

{-la funcion valor muestra cual es el numero en la lista que estamo buscando-}{-show a number of the position in list exam-}
valor b  = examen !! b 

{-
La libreria Data.List nos permite las operaciones de listas. pudiendo utilizar scans, acumulacion de lsitas, 
sub listas, predicados entre otras.

la libreria System.IO es la libreria principal de la entrada y salida de datos, permite la creacion y la interaccion de archivos,
leer, escribir y hacer operaciones (ej. cambiar o definir el tamaño, operaciones de almacenamiento en búfes, etc...) 

Data
  permite definir un nuevo tipo de dato.
    
    ej: data colorPrimario = Rojo | Amarillo | Azul
      el dato colorPrimario es uno de los valores que despues permite la combinacion y las interacciones para poder usarlos en
      el programa

    ej2:data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
      hemos creado el tipo de datos int que permite el uso de numeros enteros entre los limites definidos.

  ademas, permite hacer que cada tipo que pertenece a una typeclass se le pueda definir una instancia de la misma.
    
    ej:class Eq a where
        (==), (/=) :: a -> a -> Bool
        (/=) x y =  not (x == y)
        (==) x y =  not (x /= y)
      
      instance Eq Bool where
        (==) True True = True
        (==) False False = True
        (==) _ _ = False
      
      tomamos la clase Eq (que deberia estar en algun lugar del prelude) y luego decimos que Bool pertenece a la clase Eq. con esto
      logramos crear la instruccion que nos dice cuando 2 elementos son iguales.

$
  hace la funcion de los parentesis en el programa.
    $ 3 / $ 2+3  :=  (3/(2+3))
    putStrLn . show $ 1 + 2 := putStrLn(show(1+2))


-}
{-
the library Data. List allows operate and interact with lists, being able to use scans, Accumulating maps, sub-List, predicate, among others.
the library System.IO is the principal library of inputs and outputs of data, allows the creation and interaction, read, write and make 
operation due a file. (ex. Chance and define the size, Buffering operations, etc..)  

The data 
  allow to create new data types 
    ex: data colorPrimario = Rojo | Amarillo | Azul 
      we have created a new data that is defined as a color.    
    ex: data into = -2147483648 | -2147483647 |... | -1 | 0 | 1 | 2 |... | 2147483647
      we are creating a Int that is a primitive data.

  also, we can make our own typeclasses and how to make type instances of them by hand.
   
    ex:class Eq a where
        (==), (/=) :: a -> a -> Bool
        (/=) x y =  not (x == y)
        (==) x y =  not (x /= y)
      
      instance Eq Bool where
        (==) True True = True
        (==) False False = True
        (==) _ _ = False
      
     we take Eq (that is defined in the standard prelude) y then we say that Bool belongs to Eq with this we created an instruction that defines 2 Strings equals.
-}
