--------LENGUAJES DE PROGRAMACION - PROYECTO NRO II----------
--------JUEGO CARGA LAMBDA-BURRA - HASKELL-----------------------------
--REALIZADO POR:
--
--
module LambdaBurra where
import System.Random
import Cards-----------------------Importando el modulo Cards que contiene la data y funciones de las cartas

data Player = Lambda | You --------Tipo de dato Player (Jugadores) Lambda/You

---------------FUNCION QUE COMPARA LA PINTA DE DOS CARTAS Y DEVUELVE TRUE SI SON IGULES O FALSE SI NO LO SON-------
compara :: Card -> Card -> Bool
compara (Card v s) (Card v1 s1) = if pinta (s) == pinta (s1) then True else False

---------------FUNCION QUE AGREGA LAS CARTAS INICIALES A LAS MANOS DE LOS JUGADORES--------------------------------
iniciar :: Int -> [Card] -> Hand -> Hand
iniciar i (card) (H(hand)) = if i == 3 then (H(hand)) else iniciar (i+1) (tomarCarta(card)) (llenarMano (card) (H(hand)))

---------------FUNCION QUE DEVOLVERA TRUE SI LA CARTA DE YOU ES MAYOR------------------------------------
mayor :: Card -> Card -> Bool
mayor x y = if ( (valor x) > (valor y) ) then True else False

---------------FUNCION QUE VERIFICA SI EN TU MANO HAY UNA CARTA CON LA PINTA DE LA MESA------------------
verificarMano :: Hand -> Card -> Bool------------Hand seria la mano del jugador y card la carta de la mesa
verificarMano (H([])) _ = False
verificarMano (H(x:xs)) y = if ( (compara x y) == True) then True else verificarMano (H(xs)) y


---------------FUNCION QUE DEVUELVE UNA MANO SACANDO LA CARTA QUE SE LE INDIQUE-------------------------
quitarCarta :: Card -> Hand -> Hand -> Hand ----Card == carta a eliminar; Hand == mano del jugador; Hand == mano vacia
quitarCarta _ (H([])) x = x
quitarCarta card (H(x:xs)) (H(y))= if (card /= x) then do
  quitarCarta card (H(xs)) (H(x:y))
  else do
  quitarCarta card (H(xs)) (H(y))

---------------FUNCION QUE VA AGREGANDO CARTAS DEL MAZO A LA MANO DEL PLAYER HASTA ENCONTRAR UNA QUE SE PUEDA JUGAR--------------------------
cargar :: [Card] -> Hand -> Card -> Hand
cargar [] (H(x)) mesa = (H(mesa:x))  -------Si no hay cartas en el mazo se carga la mesa
cargar (x:xs) (H(y:ys)) mesa = if ((compara x mesa) == True) then do
                                    (H(x:y:ys)) else do
                                      cargar (xs) (H(x:y:ys)) (mesa)



---------------------------------------------------------------------------------------------------------
juego :: [Card] -> Hand -> Hand -> Card -> IO ()
juego mazo you lambda mesa = do
 let manoY = (size you)
 let manoL = (size lambda)
 if (manoY == 0) then do
 putStrLn"Felicidades¡¡!! Has ganado" 
 else do
   if (manoL == 0) then do
    putStrLn"Ha ganado Lambda"
    else do
    putStrLn ""

carta1 = Card (Numeric 1) Oro

------------------------------BARAJAR-------------------
--randomInt :: Int -> Int
--randomInt xi = do
  --          g0 <- newStdGen
    --        let x = randomR (0,xi) g0 
      --      return (fst x)

tam :: [Card] -> Int
tam x = length x
---------- FUNCION QUE DEVUELVE UNA BARAJA ORDENADA ALEATORIAMENTE ------- 
barajar ::  [Card] -> StdGen ->[Card]
barajar [] g0 = []
barajar (x) g0 | tam x == 1 = x
barajar (x) g0 = do let gr = randomR (0,(tam x)-1) g0

------------------------------------------------------PRINCIPAL------------------------------------------
main :: IO ()
main = do

  let handYou = empty ---------Crea la mano vacia para You
  let handLambda = empty ---------Crea una mano vacia para Lambda
  let mazo = baraja      ---------Crea el mazo inicial
  let mazoAux = [Card (Numeric 2) Oro]
  let tamaño = tam mazoAux 
  --let x = randomInt 5
  putStrLn 
  