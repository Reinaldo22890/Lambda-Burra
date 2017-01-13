--------LENGUAJES DE PROGRAMACION - PROYECTO NRO II----------
--------JUEGO CARGA LAMBDA-BURRA - HASKELL-----------------------------
--REALIZADO POR:
--
--
module LambdaBurra where
import System.Random
import Cards-----------------------Importando el modulo Cards que contiene la data y funciones de las cartas

data Player = Lambda | You deriving (Eq, Show) --------Tipo de dato Player (Jugadores) Lambda/You

---------------FUNCION QUE COMPARA LA PINTA DE DOS CARTAS Y DEVUELVE TRUE SI SON IGULES O FALSE SI NO LO SON-------
compara :: Card -> Card -> Bool
compara (Card v s) (Card v1 s1) = if pinta (s) == pinta (s1) then True else False

---------------FUNCION QUE AGREGA LAS CARTAS INICIALES A LAS MANOS DE LOS JUGADOR--------------------------------
iniciar :: Int -> [Card] -> Hand -> Hand
iniciar i (card) (H(hand)) = if i == 7 then (H(hand)) else iniciar (i+1) (tomarCarta(card)) (llenarMano (card) (H(hand)))

---------------FUNCION QUE DEVOLVERA TRUE SI LA CARTA DE YOU ES MAYOR------------------------------------
mayor :: Card -> Card -> Bool
mayor x y = (valor x) > (valor y) 

---------------FUNCION QUE VERIFICA SI EN TU MANO HAY UNA CARTA CON LA PINTA DE LA MESA------------------
verificarMano :: Hand -> Card -> Bool------------Hand seria la mano del jugador y card la carta de la mesa
verificarMano (H([])) _ = False
verificarMano (H(x:xs)) y = if ( (compara x y) == True) then True else verificarMano (H(xs)) y


---------------FUNCION QUE DEVUELVE UNA MANO SACANDO LA CARTA QUE SE LE INDIQUE-------------------------
quitarCarta ::  Card -> Hand ->  Hand ----Card == carta a eliminar; Hand == mano del jugador; Hand == mano vacia
quitarCarta c (H (x))  = ( H([y | y <- x, y /= c]))

-- BUSCAR EN EL MAZO LAS CARTAS QUE SE CARGARAN
-- HASTA ENCONTRAR UNA QUE SE PUEDA JUGAR O CARGAS LA MESA 	
cargar :: [Card] -> Card -> [Card]
cargar [] mesa = [mesa]  -------Si no hay cartas en el mazo se carga la mesa
cargar (x:xs) mesa = if ((compara x mesa)) then [x] else (x:cargar xs mesa)



----FUNCION QUE ENVIA EL MAZO INICIAL---------------------------------------------------------
mazoInicial :: [Card] -> Int -> [Card]
mazoInicial card i = if i == 7 then card else mazoInicial (tomarCarta(card)) (i+1)

----FUNCION QUE ORDENA ASCENDENTEMENTE UNA LISTA DE CARTAS----------------------------------------------
ordenar :: [Card] -> [Card]
ordenar [] = []
ordenar (x:xs) = let menor = ordenar [a | a <- xs, (valor a) <= (valor x)]
                     mayor = ordenar [a | a <- xs, (valor a) > (valor x)]
                 in menor ++ [x] ++ mayor


----FUNCION QUE DECIDE LA CARTA QUE JUGARA LAMBDA CONTRA El USUARIO -------------
----(Recibe una lista de la misma pinta ordenada)--------
tiraLambda :: Card -> [Card] -> Card
--si no tiene una carta mas alta:  devuelvo la mas baja
tiraLambda mesa (x:xs) | valor (last(x:xs)) < valor mesa = head (x:xs)
--- como si tiene una carta  recorre la lista hasta que encuentre la primera mas alta y la devuelve 
                       | valor (head (x:xs)) > valor mesa = head (x:xs)
                       | otherwise = tiraLambda mesa (ordenar xs)


----------------FUNCION QUE DEVUELVE LA CARTA QUE JUGARA LAMBDA--------------------------------
juegaLambda :: Card -> Hand -> Card
juegaLambda mesa h = tiraLambda mesa $ ordenar (encontrarPintas mesa h)

-------------------JUEGA YOU ()()()()()()
juegaYou :: Hand -> Card
juegaYou h = (head $ baraja)


---------------FUNCION QUE DEVUELVE UNA LISTA CON LAS CARTAS DE LA PINTA QUE PUEDO JUGAR------------------------------
encontrarPintas :: Card -> Hand -> [Card]
encontrarPintas mesa (H c) = [x | x<-c,compara mesa x]

primeravez :: [Card] -> Bool
primeravez x = (length x == 25)

not_turn :: Player -> Player
not_turn p | p == You = Lambda
           |otherwise = You

juntarMano :: Hand -> [Card] -> Hand
juntarMano (H(x)) y =  H(x ++ y)


mataYjuegaLambda :: Hand -> Card
mataYjuegaLambda x = head baraja

mataYjuegaYou :: Hand -> Card
mataYjuegaYou x = head baraja

-------------------------------------------------------------------------------------------------------
---------------FUNCION PARA JUGAR---------------------------------------------------------------
juego :: [Card] -> Hand -> Hand -> [Card] -> Player -> IO ()
juego mazo mActual mSiguiente mesa turno = do
    if (mesa /= [])then do
        putStrLn"SIMESA"
        if((encontrarPintas (head mesa) mActual) ==  []) then do
            putStrLn "NO TIENE EN LA MANO y carga"
            let carga = cargar mazo $ head mesa
            let nuevamano = juntarMano mActual carga
            -- quitar del mazo
            if (elem (head mesa) carga) then do-- te has cargado la mesa
                juego mazo mSiguiente nuevamano [] (not_turn turno) 
            else do 
                putStrLn "  nvccv"
                juego mazo nuevamano mSiguiente [] (turno) 
        else do
            putStrLn "TIENE EN LA MANO"
            if (turno == You) then do
                let jugada = juegaYou mActual  -- pedirle la carta a jugar
                let manoNueva = quitarCarta  jugada mActual
                if (manoNueva==empty) then do
                   putStrLn "Has Ganado"
                else
                    if (primeravez mazo) then do
                        juego mazo mSiguiente manoNueva [jugada] (Lambda)
                    else do
                        if (mayor jugada (head mesa))then do
                            juego mazo manoNueva mSiguiente [] (You)
                        else do
                            juego mazo mSiguiente manoNueva [] (Lambda)
            else do
                let jugada = juegaLambda (head mesa) mActual
                let manoNueva = quitarCarta jugada mActual
                putStrLn $ show jugada 
                if (manoNueva==empty) then do
                   putStrLn "Ha Ganado Lambda"
                else
                    if (mayor jugada (head mesa))then do
                        juego mazo manoNueva mSiguiente [] (Lambda)
                    else do
                        juego mazo mSiguiente manoNueva [] (You)
    else do  -- sino hay mesa
        if (turno == You) then do
            let jugada = mataYjuegaYou mActual 
            let manoNueva = quitarCarta jugada mActual
            if (manoNueva==empty) then
               putStrLn "Has Ganado"
            else do
                juego mazo manoNueva mSiguiente [jugada] (Lambda)
                putStrLn "Has Ganado"  
        else do
            let jugada = mataYjuegaLambda mActual 
            let manoNueva = quitarCarta jugada mActual
            if (manoNueva==empty) then
               putStrLn "Has Ganado"
            else do
                juego mazo manoNueva mSiguiente [jugada] (You)
                putStrLn "Has Ganado"


        --mataYjuegaLambda :: Hand -> Card

---NOTA: hacer una funcion que muestre una lista de cartas mas agradables
---NOTA: VERIFICAR SI YOU Y LAMBDA TIENEN CARTAS CON LA PINTA DE LA MESA, SINO CARGAN
---NOTA: HACER VALIDACIONES PARA CUANDO INDIQUE UN NUMERO FUERA DE RANGO
-------------------FUNCION PARA VERIFICAR QUIEN GANA EL TURNO--------------------------------
ganarTurno :: Card -> Card -> Player
ganarTurno you lambda = if ((valor you) > (valor lambda)) then You else Lambda

---------- FUNCION QUE DEVUELVE UNA BARAJA ORDENADA ALEATORIAMENTE ------- 
barajar :: [Card] -> StdGen ->[Card]
barajar b g0 | length b == 0 = []
             | otherwise = (b!!r: barajar [x | x<-b, x /= (b!!r)] gi)
              where (r,gi) = (randomR (0,(length b)-1) g0)

-- repartir_mano ------------------------------------------------------PRINCIPAL------------------------------------------
main :: IO ()
main = do

  -- generador pseudo-aleatorio
  newStdGen
  g <- getStdGen
  -- baraja aletoria a partir de la secuencias de numeros 'g' y una baraja virgen
  let barajadas = barajar baraja g
  putStrLn $ show barajadas
  -- [x | x <- [1 .. 12]]
  let handYou = (H[(barajadas !! 1), (barajadas!! 2), (barajadas !! 3)]) ---------Crea la mano inicial para You
  let handLambda = (H[(barajadas !! 4), (barajadas !! 5), (barajadas !! 6)])---------Crea la mano inicial para Lambda
  let mazo = mazoInicial (barajadas) (0)     ---------Crea el mazo inicial
  let mesa = [head barajadas]
  let turno = (You)
  let lista = [head baraja, last baraja] ++ [head baraja, last baraja]
  putStrLn $ show lista
  putStrLn"   ************WELCOME TO LAMBDA-BURRA*************   "
  putStrLn $ show mesa
  juego mazo handYou handLambda mesa turno

