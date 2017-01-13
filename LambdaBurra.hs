--------LENGUAJES DE PROGRAMACION - PROYECTO NRO II----------
--------JUEGO CARGA LAMBDA-BURRA - HASKELL-----------------------------
--REALIZADO POR:
--Gabriel Rodriguez
--Reinaldo Figuera 
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

----FUNCION QUE ENVIA EL MAZO INICIAL---------------------------------------------------------
mazoInicial :: [Card] -> Int -> [Card]
mazoInicial card i = if i == 7 then card else mazoInicial (tomarCarta(card)) (i+1)


----FUNCION QUE ORDENA ASCENDENTEMENTE UNA LISTA DE CARTAS----------------------------------------------
ordenar :: [Card] -> [Card]
ordenar [] = []
ordenar (x:xs) = let menor = ordenar [a | a <- xs, (valor a) <= (valor x)]
                     mayor = ordenar [a | a <- xs, (valor a) > (valor x)]
                 in menor ++ [x] ++ mayor

---------- FUNCION QUE DEVUELVE UNA BARAJA ORDENADA ALEATORIAMENTE ------- 
barajar :: [Card] -> StdGen ->[Card]
barajar b g0 | length b == 0 = []
             | otherwise = (b!!r: barajar [x | x<-b, x /= (b!!r)] gi)
              where (r,gi) = (randomR (0,(length b)-1) g0)


-- Saca n cantidad de cartas de una lista de cartas ------------------------------------------------------PRINCIPAL------------------------------------------
getnCard :: Int -> [Card] -> [Card]
getnCard n x = [(x !! r )| r <- [0..(n-1)]]


-- Genera una lista de carta de una determinada pinta exitente en una mano
encontrarPintas :: Card -> Hand -> [Card]
encontrarPintas mesa (H c) = [x | x<-c,compara mesa x]



mejorjugada ::[Card] -> [[Card]]->  Card
mejorjugada y [] = last y  
mejorjugada [] (x:xs) = mejorjugada x xs
mejorjugada y (x:xs) = if ((length x) > (length y)) then do
                         mejorjugada x xs
                       else do
                         mejorjugada y xs


                         

----FUNCION QUE DECIDE LA CARTA QUE JUGARA LAMBDA CONTRA El USUARIO -------------
----(Recibe una lista de la misma pinta ordenada)--------
tiraLambda :: Card -> [Card] -> Card
--si no tiene una carta mas alta:  devuelvo la mas baja
tiraLambda mesa (x:xs) | valor (last(x:xs)) < valor mesa = head (x:xs)
--- como si tiene una carta  recorre la lista hasta que encuentre la primera mas alta y la devuelve 
                       | valor (head (x:xs)) > valor mesa = head (x:xs)
                       | otherwise = tiraLambda mesa (xs)


----Funcion que devuelve una mano sacando la carta que se le indique------------
----(Card == carta a eliminar; Hand == mano del jugador; Hand == mano vacia)----
quitardeMano ::  Card -> Hand ->  Hand 
quitardeMano c (H (x))  = ( H([y | y <- x, y /= c]))


----Funcion que devuelve un mazo sin una determinada cantidad de cartas
quitardeMazo ::  Int -> [Card] ->  [Card] 
quitardeMazo _ [] = []
quitardeMazo n  x  = [x !! i | i <- [n..((length x)-1)]]


-- Determina si es la primera ronda de la partida
primeravez :: [Card] -> Hand -> Hand -> Bool
primeravez x (H(h1)) (H(h2))= (length x == 25) && (length h1 == 7) && (length h2 == 7)

-- Devuelve el jugador contrario
not_turn :: Player -> Player
not_turn p | p == You = Lambda
           |otherwise = You

-- Carga del mazo y devuelve la lista de cartas cargada 
cargar :: [Card] -> Card -> [Card]
cargar [] mesa = [mesa]  -------Si no hay cartas en el mazo se carga tambien la mesa
cargar (x:xs) mesa = if ((compara x mesa)) then [x] else (x:cargar xs mesa)

-- Adjunta a una mano la lista de carta que se cargo
juntarMano :: Hand -> [Card] -> Hand
juntarMano (H(x)) y =  H(x ++ y)

----------------FUNCION QUE DEVUELVE LA CARTA QUE JUGARA LAMBDA--------------------------------
juegaLambda :: Card -> Hand -> Card
juegaLambda mesa h = tiraLambda mesa $ ordenar (encontrarPintas mesa h)
-------------------------------------------------------------------------------
mataYjuegaLambda :: Hand -> Card
mataYjuegaLambda (H(x)) = last $ ordenar (x)

-------------------JUEGA YOU ()()()()()()
juegaYou :: Int -> Hand -> Card
juegaYou i (H(x)) = ( x !! i) 
              
              


-------------------------------------------------------------------------------------------------------
---------------FUNCION QUE CONTROLA EL FLUJO DEL JUEGO---------------------------------------------------------------
juego :: [Card] -> Hand -> Hand -> [Card] -> Player -> IO ()
juego mazo mActual mSiguiente mesa turno = do
    if (mesa /= [])then do --si existe una carta en la mesa
        putStrLn ""
        putStrLn "*MESA: "
        print mesa
        if((encontrarPintas (head mesa) mActual) ==  []) then do -- No encontra cartas en la mano y carga
            print turno
            putStr " NO TIENE CARTAS DE ESA PINTA EN SU MANO SE CARGO: "
            let carga = cargar mazo $ head mesa
            let nuevamano = juntarMano mActual carga
            print $ (length carga)
            if (elem (head mesa) carga) then do-- te has cargado la mesa
                let nuevomazo = quitardeMazo ((length carga)-1) mazo
                if (turno == You) then  do
                putStrLn "Usted se ha cargado la mesa, juega Lambda"
                juego nuevomazo mSiguiente nuevamano [] (Lambda)
                else do
                putStrLn ""
                putStrLn "Lambda se ha cargado la mesa, juega usted!!"  
                juego nuevomazo mSiguiente nuevamano [] (You) 
            else do---encontro una carta
                putStrLn "Hasta encontrar una carta"
                let nuevomazo = quitardeMazo (length carga) mazo
                juego nuevomazo nuevamano mSiguiente mesa (turno) 
        else do ---si tiene cartas en su mano
            if (turno == You) then do
                putStrLn ""
                putStrLn "*SU MANO: "
                putStrLn ""
                print mActual
                putStrLn ""
                putStrLn "Juegue una carta:"
                cY <- getLine
                putStrLn"_____________________________________________________________________________________________________" 
                let cYnumber = read cY
                let jugada = (juegaYou (cYnumber - 1) mActual)
                putStr "USTED HA JUGADO: "
                print jugada
                if ((compara  (head mesa)  jugada) == False) then do
                putStrLn "Carta no valida, juegue una carta con la pinta de la mesa."
                juego mazo mActual mSiguiente mesa turno
                else do
                  let manoNueva = quitardeMano  jugada mActual
                  if (manoNueva==empty) then do
                    putStrLn "Has Ganado"
                  else
                    if (primeravez mazo mActual mSiguiente) then do
                        juego mazo mSiguiente manoNueva [jugada] (Lambda)
                    else do
                        if (mayor jugada (head mesa))then do
                            putStrLn ""
                            juego mazo manoNueva mSiguiente [] (You)
                        else do
                            juego mazo mSiguiente manoNueva [] (Lambda)
            else do ----Turno de lambda
                let jugada = juegaLambda (head mesa) mActual
                putStr "LAMBDA HA JUGADO: "
                print jugada
                let manoNueva = quitardeMano jugada mActual 
                if (manoNueva==empty) then do
                   putStrLn "Ha Ganado Lambda"
                else
                    if (mayor jugada (head mesa))then do
                        juego mazo manoNueva mSiguiente [] (Lambda)
                    else do
                        juego mazo mSiguiente manoNueva [] (You)
    else do  -- sino hay carta en mesa
        if (turno == You) then do
            putStrLn""
            putStrLn "*USTED HA MATADO"
            putStrLn "-----------------------------------------------------------------------------------------------------"
            putStrLn ""
            putStrLn "*SU MANO: "
            print mActual
            putStrLn ""
            putStrLn "Juegue una carta:"
            cY <- getLine
            putStrLn "____________________________________________________________________________________________________" 
            let cYnumber = read cY
            let jugada = (juegaYou (cYnumber - 1) mActual)
            putStr "USTED HA JUGADO: "
            print jugada 
            let manoNueva = quitardeMano jugada mActual
            if (manoNueva==empty) then
               putStrLn "Has Ganado"
            else do
               juego mazo mSiguiente manoNueva [jugada] (Lambda)
               putStrLn "Has Ganado"  
        else do ---- Turno es igual a lambda
            let jugada = mataYjuegaLambda mActual
            putStrLn "*LAMBDA HA MATADO"
            putStrLn ""
            putStrLn "LAMBDA HA JUGADO: "
            print jugada
            putStrLn"------------------------------------------------------------------------------------------------------"
            let manoNueva = quitardeMano jugada mActual
            if (manoNueva==empty) then
               putStrLn "Lambda ha Ganado"
            else do
                juego mazo mSiguiente manoNueva  [jugada] (You)


        --mataYjuegaLambda :: Hand -> Card

---NOTA: hacer una funcion que muestre una lista de cartas mas agradables
---NOTA: VERIFICAR SI YOU Y LAMBDA TIENEN CARTAS CON LA PINTA DE LA MESA, SINO CARGAN
---NOTA: HACER VALIDACIONES PARA CUANDO INDIQUE UN NUMERO FUERA DE RANGO
-------------------FUNCION PARA VERIFICAR QUIEN GANA EL TURNO--------------------------------
ganarTurno :: Card -> Card -> Player
ganarTurno you lambda = if ((valor you) > (valor lambda)) then You else Lambda

 

main :: IO ()
main = do

  -- generador pseudo-aleatorio
  newStdGen
  g <- getStdGen
  -- Barajá aletoria a partir de la secuencias de numeros 'g' y una baraja virgen
  let barajadas = barajar baraja g
  -- muestra mesa
  let mesa = getnCard 1 barajadas
  let handYou = H(getnCard 7 $ quitardeMazo 1 barajadas) ---Crea la mano inicial para You
  let handLambda = H(getnCard 7 $ quitardeMazo 8 barajadas)---------Crea la mano inicial para Lambda
  let mazo = quitardeMazo 15 barajadas
  putStrLn "____________________________________________________________________________________________________"
  putStrLn"   ************WELCOME TO LAMBDA-BURRA*************   "
  putStrLn "____________________________________________________________________________________________________"
  juego mazo handYou handLambda mesa (You)
