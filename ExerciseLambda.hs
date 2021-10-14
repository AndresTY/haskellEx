{-
march 12/2020
Sergio Arboleda University 
@version: 1.0.2
-}


{-Calculo del indice de masa corporal de la altura y el peso-}{-body mass index calculation with weight and height-}
bmiCalculo :: Float -> Float-> String
bmiCalculo m h 
	| bmi <= 18 = "debe alimentarse mejor: Delgado"
	| bmi <= 25 = "aceptable o normal: felicidades"
	| bmi <= 30 = "debe perder masa: Hacer ejercicio"
	| otherwise = "Sobre limite: revisar valores"
		where bmi = m/(h**2)

{-Calculo del indice de masa corporal de una lista de pesos y alturas-}{-body mass index calculation with duplex-}

bmiDu :: [(Float,Float)] -> [Float]
bmiDu (x:xs) = [(fst x)/((snd x)**2)| x <-xs]  

{-Calculo del indice de masa corporal de una lista de pesos y alturas-}{-body mass index calculation with a list-}
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi w h = w / h **2

{-Ordenamiento de una lista de menor a mayor-}{-Ordening of a number's list-}
hagoAlgo :: (Ord a) => [a] -> [a]
hagoAlgo [] = []
hagoAlgo (x:xs) = let peques = hagoAlgo [a | a <- xs, a<=x]
                      grandes = hagoAlgo [a | a <- xs, a>x]
                      in peques ++ [x] ++ grandes

{-Conjetura de Collazts-}{-Collazts conjecture-}
pkm :: Int -> [Int]
pkm 1 = [1]
pkm x | even x    = x: pkm (x`div`2)
	  | odd  x    = x: pkm (x*3 + 1)

{-Conjetura de Collazts usando la funcion Lambda para determinar un filtro-}{-Use a Lambda function in a Collazts conjecture from determinate a filter-}
dbzLamb :: Int 
dbzLamb = length (filter (\x -> length x > 15) (map pkm [1..100]))

{-Conjetura de Collazts con una funcion para determinar el filtro -}{-Use a ordinary function in Collazts conjecture-}
numLongPkm :: Int
numLongPkm = length (filter funtionF (map pkm [1..10000]))
	where funtionF xs = length xs > 123

{-Calculo del volumen de una esfera usando la funcion Lambda-}{-Use a Lambda function in sphere's volumen calculation-}
volEsferaLam :: (RealFloat a) => a -> a
volEsferaLam = (\r -> (4/3)*pi*(r**3))

{-Calculo del volumen de una esfera por comprension-}{-Sphere's volumen calculation-}
volEsfera :: (RealFloat a) => a -> a 
volEsfera r = (4/3)*pi*r^3

{-Calculo del area de un circulo con dos radios por comprension-}{-Area calculation of a circle with 2 radius-}
areaCorCir :: Double -> Double -> Double
areaCorCir r1 r2 = pi*((r2-r1)**2)

{-Calculo del area de un circulo con dos radios con la funcion Lambda-}{-Use a lambda function to calculate the area of a circle with 2 radius-}
areaCorCirLam :: Double -> Double -> Double
areaCorCirLam = \r1 -> \r2 -> pi*((r2-r1)^2)

{-intercambio de posiciones de una dupla por comprension-}{-Change of a position of a duplex-}
interCambia :: (b,a) -> (a,b)
interCambia (x,y) = (y,x)

{-intercambio de posiciones de una dupla con la funcion Lambda-}{-Change of a position by lambda function -}
interCambiaLam :: (t1, t) -> (t, t1)
interCambiaLam = \(x,y) -> (y,x)

{-Calculo del modulo direccion de una coordenadas por comprension-}{-Calculation of the direction module of a coordinate by comprehension-}
modulo :: Floating a => (a, a) -> a
modulo (x,y) = sqrt(x**2 + y**2)

{-Calculo del modulo direccion de una coordenadas por medio de la funcion Lambda-} {-Calculation of the direction module of a coordinate by lambda function-}
moduloLam :: (Double, Double) -> Double
moduloLam = \(x,y)->(sqrt(x^2 + y^2))

{-Calculo del modulo direccion de dos coordenadas-}{-Calculation of teh direction module of 2 coordinates-}
distancia :: Floating a => (a, a) -> (a, a) -> a
distancia (x,y) (xs,ys) = modulo((xs-x),(ys-y))

{-Calculo del modulo direccion de dos coordenadas con lambda-}{-Calculation of the direction module of 2 coordinates with lambda function-}
distanciaLam :: (Double, Double) -> (Double, Double) -> Double
distanciaLam = \(x,y) -> \(xs,ys) -> moduloLam((xs-x),(ys-y))

{-
La funcion lambda es una funcion anonima que permite las acciones de un funcion pero sin tener que crearla, ni llamarla.
En el caso de hacerla en haskell, toca usar (\) el backslash siguido de lo datos que se ingresan y despues usa una flecha (->) en donde 
se expresan lo que va hacer la funcion.

Ejemplo:
	pkm x y z = x+y+z
	pkm x y = \z -> x+y+z
	pkm x = \y -> \z ->x+y+z
	pkm = \x -> \y -> \z ->x+y+z (funcion lambda)

Desventajas:
-No es reutilizable por lo que no se puede usar la funcion en otros lugares.
-Como es una función implementada de forma ad-hoc, e incrustada en algún lugar, no es posible probarla de forma aislada. 
-Una función lambda puede tener acceso a todo el estado de su scope. Por tanto, puede tener acceso a algún valor mutable y hacer que tengamos diferentes resultados en cada ejecución. Algo que no queremos en la programación funcional.
-No es bueno abusar de expresiones lambda complicadas y anidadas. Provoca que nuestro código se vuelva dificil de seguir, y por tanto, poco mantenible.
-}

{-
the function lambda is an anonymous function that is as a normal function but without necesity of creater or called it.
In haskell this is make with a backslash (\) before of a variable and then an arrow (->) followed by condicion to do.

example:
 	pkm x y z = x+y+z
 	pkm x y = \z -> x+y+z
	pkm x = \y -> \z ->x+y+z
	pkm = \x -> \y -> \z ->x+y+z (lambda function)

Disadvantages:
-It isn't reusable so the function cannot be used elsewhere.
-as this is an ad-hoc function, it cannot try in isolated form 
-A lambda function can has access to all scope's status. Thus, it can has a mutable varible and this can have differents result. Something that we don't want in a functional programming.
-In this isn't good to abuse of compicate and nested lambda expressions. this can provocate that ours code become hard and have a little maintainable. 
-}
