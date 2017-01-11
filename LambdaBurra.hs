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

---------------FUNCION QUE AGREGA LAS CARTAS INICIALES A LAS MANOS DE LOS JUGADOR--------------------------------
iniciar :: Int -> [Card] -> Hand -> Hand
iniciar i (card) (H(hand)) = if i == 7 then (H(hand)) else iniciar (i+1) (tomarCarta(card)) (llenarMano (card) (H(hand)))

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


---------------FUNCION QUE ENVIA EL MAZO INICIAL---------------------------------------------------------
mazoInicial :: [Card] -> Int -> [Card]
mazoInicial card i = if i == 7 then card else mazoInicial (tomarCarta(card)) (i+1) 

---------------FUNCION QUE DEVUELVE UNA LISTA CON LAS CARTAS DE LA PINTA QUE PUEDO JUGAR------------------------------
listaPintas :: Card -> Hand -> Hand
listaPintas mesa (H c) = H [x | x<-c,compara mesa x]

---------------FUNCION QUE DECIDE LA CARTA QUE LAMBDA VA A DEJAR EN LA MESA---------------------
-------------------------------------------------------------------------------------------------------
---------------FUNCION PARA JUGAR---------------------------------------------------------------
juego :: [Card] -> Hand -> Hand -> Card -> Player -> IO ()
juego mazo (H(x)) lambda mesa turno= do
 let sizeManoY = (size (H(x)))
 let sizeManoL = (size lambda)
 let deck = mazo
 let mYou = x
 let mLambda = lambda
 let table = mesa
 let turn = turno
 if (sizeManoY == 0) then do
 putStrLn"Felicidades¡¡!! Has ganado" 
 else do
   if (sizeManoL == 0) then do
    putStrLn"Ha ganado Lambda"
    else do
    putStrLn"----------------------------------------------"
    putStrLn "**CARTA EN LA MESA**"
    print table
    putStrLn "----------------------------------------------"
    putStrLn "Porfavor indique el numero de la carta que desea jugar: "
    putStrLn "----------------------------------------------"
    putStrLn"SU MANO: "
    print (mYou)
---NOTA: hacer una funcion que muestre una lista de cartas mas agradables
---NOTA: VERIFICAR SI YOU Y LAMBDA TIENEN CARTAS CON LA PINTA DE LA MESA, SINO CARGAN
---NOTA: HACER VALIDACIONES PARA CUANDO INDIQUE UN NUMERO FUERA DE RANGO
    cY <- getLine 
    let cYnumber = read cY-----convertimos en entero el numero indicado por el usuario
    let cartaJugada = (mYou !! cYnumber) 
    if ((compara (cartaJugada) (table)) == False) then do
        putStrLn "Carta no Aceptada, elija una carta con la pinta de la mesa."
        putStrLn"-------------------------------------------------------------"
        juego deck (H(mYou)) mLambda table turn
        else do
---NOTA:Falta pedir la carta de lambda
        --let mYou2 = (quitarCarta (cartaJugada) (H(mYou)) (empty)) -------Eliminar la carta de la mano de You
        --let mLambda2 = (quitarCarta (cartaLambda) (H(mLambda)) (empty))-------Eliminar carta de la mano de lambda
        --if ((ganarTurno (cartaJugada) (cartaLambda)) == You) then do
        --putStrLn "USTED HA GANADO EL TURNO"
        --putStrLn ""
        --putStrLn "Indique la Carta que desea agregar a la mesa:"
        --nT <- getLine ----pido la carta
        --cardT <- read nT---convierto la carta en numero
        --newTable <- (mYou !! cardT)-----nueva carta para la mesa
        --let mYou3 = (quitarCarta (newTable) (H(mYou2)) (empty))
         putStrLn ""



-------------------FUNCION PARA VERIFICAR QUIEN GANA EL TURNO--------------------------------
ganarTurno :: Card -> Card -> Player
ganarTurno you lambda = if ((valor you) > (valor lambda)) then You else Lambda

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

  let handYou = (H[(baraja !! 1), (baraja !! 2), (baraja !! 3)]) ---------Crea la mano inicial para You
  let handLambda = (H[(baraja !! 4), (baraja !! 5), (baraja !! 6)])---------Crea la mano inicial para Lambda
  let mazo = mazoInicial (baraja) (0)     ---------Crea el mazo inicial
  let mesa = (head baraja)
  let turno = (You)
  putStrLn"   ************WELCOME TO LAMBDA-BURRA*************   "
  juego mazo handYou handLambda mesa turno

