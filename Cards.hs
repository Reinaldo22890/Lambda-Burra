-----------------------------------------------------------------------------------------------------------------
----------------------------------------------MODULO CARDS-------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
------------MODULO QUE CONTIENE EL TIPO DE DATO DE LAS CARTAS Y SUS FUNCIONES------------------------------------
module Cards(baraja, empty, size, llenarMano, tomarCarta, pinta, Suit(..), valor, Value(..), Card(..), Hand(..)) where

data Suit = Oro | Espadas | Bastos | Copas deriving (Eq, Show) --------Declaracion del tipo de dato suit (Oro, Espadas, Bastos,Copas)
data Value = Numeric Int | Sota | Caballo | Rey | As deriving (Eq, Show)-----Declaracion del tipo de dato Value (Numeic Int, Sota, Caballo,Rey,As)
data Card = Card {value :: Value, suit :: Suit} deriving (Eq)-------Declaracion del tipo de dato Card (value and suit)

instance Show Card where
    show (Card n s) = (show n) ++(" ")++ (show s)

data Hand = H [Card] | Empty deriving (Eq, Show)---------Declaracion del tipo de dato Hand que puede poseer cartas o estar vacio

-------------FUNCION PARA CREAR LA BARAJA DE CARTAS----------------------------------------------------------------
baraja :: [Card]
baraja = [Card value suit | value <- [Numeric 2, Numeric 3, Numeric 4, Numeric 5, Numeric 6, Numeric 7, Sota, Caballo, Rey, As], suit <- [Oro, Espadas, Bastos, Copas]]


-------------FUNCION QUE GENERA UNA MANO VACIA---------------------------------------------------------------------
empty :: Hand
empty = H([])


-------------FUNCION QUE DETERMINA LA CANTIDAD DE CARTAS EN UNA MANO-----------------------------------------------
size :: Hand -> Int
size Empty = 0
size (H([])) = 0
size (H(x:xs)) = 1 + size (H(xs))


-------------FUNCION QUE AGREGA UNA CARTA DE LA BARAJA A LA MANO DEL JUGADOR---------------------------------------
llenarMano :: [Card] -> Hand -> Hand
llenarMano [] _ = error"baraja Vacia"
llenarMano (x:xs) Empty = H[(x)]
llenarMano (x:xs) (H(hand)) = H(x:hand)


-------------FUNCION QUE ELIMINA LA CARTA EN EL TOPE DE LA BARAJA--------------------------------------------------
tomarCarta :: [Card] -> [Card]
tomarCarta [] = error"baraja vacia"
tomarCarta x = tail x

-------------FUNCION QUE DEVUELVE EL VALOR DE UNA CARTA------------------------------------------------------------
valor :: Card -> Int
valor (Card v s) = valueCard v

-------------FUNCION UTILIZADA POR LA FUNCION "valor" LA CUAL DEVUELVE SOLO EL VALOR DE LA CARTA-------------------
valueCard :: Value -> Int
valueCard (Numeric n) = n
valueCard value | value == Sota = 10
valueCard value | value == Caballo = 11
valueCard value | value == Rey = 12
valueCard value | value == As = 13 

--------------FUNCION QUE EVALUA LA PINTA DE UNA CARTA Y LA DEVUELVE-----------------------------------------------
pinta :: Suit -> Suit
pinta suit = suit