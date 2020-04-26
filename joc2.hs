import Data.Map.Strict (Map)
import qualified Data.Map as Map
import System.Random
import Control.Monad


data Taulell = Taulell
    { n :: Int
    , m :: Int
    , casella :: Map (Int, Int) Color
    }
    
data Coord = Coord Int Int deriving(Show)

data Color = Groc | Vermell deriving(Show, Eq)

data Estrategia = Random | Greedy | Smart deriving(Eq)

{-
instance Show Taulell where
    show (Taulell n m ma) = unlines [concat [(show i) | i <- [0 .. m-1]] ++ "\n" ++
                            [[(showColor (obteCasella (Coord nn mm) (Taulell n m ma))) |
                             nn <- [0 .. n-1]] | mm <- [0 .. m-1]]    ]
                             -}
                             
instance Show Taulell where
    show (Taulell n m ma) = unlines $  [[(showColor (obteCasella (Coord nn mm) (Taulell n m ma))) | mm <- [1.. m]] | nn <- [1..n]] ++ [concat [(show i) | i <- [1 .. m]]]                            


--filtra els d'un color concret en la forma ((Int,Int),Color)
esGrocFilter :: ((Int,Int),Color) -> Bool
esGrocFilter ((ia,ib),c)
    |c == Groc = True
    |otherwise = False
    
esVermellFilter :: ((Int,Int),Color) -> Bool
esVermellFilter ((ia,ib),c)
    |c == Vermell = True
    |otherwise = False
    
--Separa la key i el value per obtenir una llista de Coordenades
obteCoord :: [((Int,Int),Color)] -> [Coord]
obteCoord [] = []
obteCoord (((ia,ib),c):xs) = [(Coord ia ib)] ++ (obteCoord xs)

--obte els elements d'un mateix color d'un taulell
filtraPerColor :: Color -> Taulell -> [((Int,Int),Color)]
filtraPerColor color (Taulell n m ma)
    |color == Groc = filter esGrocFilter (Map.toList ma)
    |color == Vermell = filter esVermellFilter (Map.toList ma)
    
    {-
filtraPerColor2 :: Color -> Taulell -> Taulell
filtraPerColor2 color (Taulell n m ma) 
    | color == Groc = (Taulell n m (Map.filter (== Groc) ma))
    | color == Vermell = (Taulell n m (Map.filter (== Vermell) ma))
    -}
esJust :: Maybe a -> Bool
esJust (Just a) = True
esJust Nothing = False    

esGroc :: Coord -> Taulell -> Bool
esGroc (Coord n m) (Taulell nn mm ma) 
    |Map.lookup (n,m) ma == Just Groc = True
    |otherwise = False

esVermell :: Coord -> Taulell -> Bool
esVermell (Coord n m) (Taulell nn mm ma) 
    |Map.lookup (n,m) ma == Just Vermell = True
    |otherwise = False
    
---------------------------------
---------------------------------
--Les següents funcions serveixen per comprovacions de si hi ha un valor (num) de fitxes en fila d'un mateix Color
--També hi ha funcions per fer el mateix però contant espais en blanc
--El nom de les funcions en concret és autodescriptiu
---------------------------------
---------------------------------

numDiagonalEsquerraDalt :: Int -> Coord -> Taulell -> Color -> Int -> Bool
numDiagonalEsquerraDalt num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (numDiagonalEsquerraDalt num (Coord (x-1) (y-1)) (Taulell n m ma) color (count+1))
    
    
numDiagonalEsquerraBaix :: Int -> Coord -> Taulell -> Color -> Int -> Bool
numDiagonalEsquerraBaix num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (numDiagonalEsquerraBaix num (Coord (x+1) (y-1)) (Taulell n m ma) color (count+1))

numDiagonalDretaDalt :: Int ->Coord -> Taulell -> Color -> Int -> Bool
numDiagonalDretaDalt num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (numDiagonalDretaDalt num (Coord (x-1) (y+1)) (Taulell n m ma) color (count+1))


    
numDiagonalDretaBaix :: Int -> Coord -> Taulell -> Color -> Int -> Bool
numDiagonalDretaBaix num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (numDiagonalDretaBaix num (Coord (x+1) (y+1)) (Taulell n m ma) color (count+1))


numDiagonal :: Int -> Coord -> Taulell -> Color -> Bool
numDiagonal num coord taulell color
    |(numDiagonalEsquerraDalt num coord taulell color 0) = True
    |(numDiagonalEsquerraBaix num coord taulell color 0) = True
    |(numDiagonalDretaDalt num coord taulell color 0) = True
    |(numDiagonalDretaBaix num coord taulell color 0) = True
    |otherwise = False
    
numHoritzontalEsquerra :: Int ->Coord -> Taulell -> Color -> Int -> Bool
numHoritzontalEsquerra num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (numHoritzontalEsquerra num (Coord x (y-1)) (Taulell n m ma) color (count+1))
    
numHoritzontalDreta :: Int -> Coord -> Taulell -> Color -> Int -> Bool
numHoritzontalDreta num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (numHoritzontalDreta num (Coord x (y+1)) (Taulell n m ma) color (count+1))    
    
    

numHoritzontal :: Int -> Coord -> Taulell -> Color -> Bool
numHoritzontal num coord taulell color
    |(numHoritzontalEsquerra num coord taulell color 0) = True
    |(numHoritzontalDreta num coord taulell color 0) = True
    |otherwise = False
    
    
numVerticalDalt :: Int -> Coord -> Taulell -> Color -> Int -> Bool
numVerticalDalt num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (numVerticalDalt num (Coord (x-1) y) (Taulell n m ma) color (count+1))  
    
    
numVerticalBaix :: Int -> Coord -> Taulell -> Color -> Int -> Bool
numVerticalBaix num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (numVerticalBaix num (Coord (x+1) y) (Taulell n m ma) color (count+1))

  

  
{-    NO CAL
numEspaisVerticalBaix :: Int -> Coord -> Taulell ->  Int -> Bool
numEspaisVerticalBaix num (Coord x y) (Taulell n m ma)  count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma == Nothing = (numEspaisVerticalBaix num (Coord (x+1) y) (Taulell n m ma)  (count+1))   
    |Map.lookup (x,y) ma /= Nothing = False
    -}     




numVertical :: Int -> Coord -> Taulell -> Color -> Bool
numVertical num coord taulell color
    |(numVerticalDalt num coord taulell color 0) = True
    |(numVerticalBaix num coord taulell color 0) = True
    |otherwise = False

numVerticalHoritzontalDiagonal :: Int -> Coord -> Taulell -> Color -> Bool
numVerticalHoritzontalDiagonal num coord taulell color
    |(numVerticalDalt num coord taulell color 0) = True
    |(numVerticalBaix num coord taulell color 0) = True
    |(numHoritzontalEsquerra num coord taulell color 0) = True
    |(numHoritzontalDreta num coord taulell color 0) = True
    |(numDiagonalEsquerraDalt num coord taulell color 0) = True
    |(numDiagonalEsquerraBaix num coord taulell color 0) = True
    |(numDiagonalDretaDalt num coord taulell color 0) = True
    |(numDiagonalDretaBaix num coord taulell color 0) = True
    |otherwise = False
    
--llistaGrocs :: Taulell -> [Coord]
--llistaGrocs (Taulell n m ma) = unlines $ [[(Coord nn mm) | mm <- [1.. m]] | nn <- [1..n], esGroc((Coord nn mm) (Taulell n m ma))]
    
--Donat un color i un taulell diu si hi ha num en ratlla d'aquets color


numEnRatlla :: Int -> Color -> Taulell -> Bool
numEnRatlla num color taulell = (numEnRatllaCoord num color taulell (obteCoord (filtraPerColor color taulell)))
    where
        numEnRatllaCoord :: Int -> Color -> Taulell -> [Coord] -> Bool
        numEnRatllaCoord _ _ _ [] = False
        numEnRatllaCoord num color taulell (x:xs) = (numVerticalHoritzontalDiagonal num x taulell color) || (numEnRatllaCoord num color taulell xs)

  
  
  
  
numEspaisDiagonalEsquerraDalt :: Int -> Coord -> Taulell -> Int -> Bool
numEspaisDiagonalEsquerraDalt num (Coord x y) (Taulell n m ma)  count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma == Nothing = (numEspaisDiagonalEsquerraDalt num (Coord (x-1) (y-1)) (Taulell n m ma) (count+1))   
    |Map.lookup (x,y) ma /= Nothing = False
    
numEspaisDiagonalDretaDalt :: Int -> Coord -> Taulell -> Int -> Bool
numEspaisDiagonalDretaDalt num (Coord x y) (Taulell n m ma)  count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma == Nothing = (numEspaisDiagonalDretaDalt num (Coord (x-1) (y+1)) (Taulell n m ma) (count+1))   
    |Map.lookup (x,y) ma /= Nothing = False  
  

numEspaisHoritzontalEsquerra :: Int -> Coord -> Taulell -> Int -> Bool
numEspaisHoritzontalEsquerra num (Coord x y) (Taulell n m ma)  count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma == Nothing = (numEspaisHoritzontalEsquerra num (Coord x (y-1)) (Taulell n m ma) (count+1))   
    |Map.lookup (x,y) ma /= Nothing = False
    


numEspaisHoritzontalDreta :: Int -> Coord -> Taulell -> Int -> Bool
numEspaisHoritzontalDreta num (Coord x y) (Taulell n m ma)  count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma == Nothing = (numEspaisHoritzontalDreta num (Coord x (y+1)) (Taulell n m ma) (count+1))   
    |Map.lookup (x,y) ma /= Nothing = False

    
numEspaisVerticalDalt :: Int -> Coord -> Taulell -> Int -> Bool
numEspaisVerticalDalt num (Coord x y) (Taulell n m ma)  count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma == Nothing = (numEspaisVerticalDalt num (Coord (x-1) y) (Taulell n m ma) (count+1))   
    |Map.lookup (x,y) ma /= Nothing = False
    
--Comprova si hi ha num en ratlla a partir d'una coordenada i si es possible arribar a 4 en ratlla des d'aquesta en algun moment, és a dir, si hi ha espai en blanc per fer-ho
numEnRatllaY4VerticalHoritzontalDiagonal :: Int -> Coord -> Taulell -> Color -> Bool
numEnRatllaY4VerticalHoritzontalDiagonal num (Coord x y) taulell color
    |(numVerticalDalt num (Coord x y) taulell color 0) && (numEspaisVerticalDalt (4-num) (Coord (x-num) y) taulell 0)= True
    |(numHoritzontalEsquerra num (Coord x y) taulell color 0) && (numEspaisHoritzontalEsquerra (4-num) (Coord x (y-num)) taulell 0)= True
    |(numHoritzontalDreta num (Coord x y) taulell color 0) && (numEspaisHoritzontalDreta (4-num) (Coord x (y+num)) taulell  0) = True
    |(numDiagonalEsquerraDalt num (Coord x y) taulell color 0) && (numEspaisDiagonalEsquerraDalt (4-num) (Coord (x-num) (y-num)) taulell  0)= True
    |(numDiagonalDretaDalt num (Coord x y) taulell color 0) && (numEspaisDiagonalDretaDalt (4-num) (Coord (x-num) (y+num)) taulell  0) = True
    |otherwise = False    
    
--comprova totes les posicions de fitxes del color per veure si hi ha una fila de num fitxes amb la que en un futur es pugui arribar a 4
numEnRatllaY4 :: Int -> Color -> Taulell -> Bool
numEnRatllaY4 num color taulell = (numEnRatllaY4Coord num color taulell (obteCoord (filtraPerColor color taulell)))
    where
        numEnRatllaY4Coord :: Int -> Color -> Taulell -> [Coord] -> Bool
        numEnRatllaY4Coord 0 _ _ _ = True
        numEnRatllaY4Coord _ _ _ [] = False
        numEnRatllaY4Coord num color taulell (x:xs) = (numEnRatllaY4VerticalHoritzontalDiagonal num x taulell color) || (numEnRatllaY4Coord num color taulell xs)
            
            
        --numEnRatllaOMesCoord num color taulell (x:xs) = (numVerticalHoritzontalDiagonal num x taulell color) || (numEnRatllaOMesCoord num color taulell xs)
            

--retorna la coordenada a la que cauria una peça al ser llençada per una columna, pot retornar un valor fora del taulell en cas que una columna estigui plena o s'introdueixi una columna inexistent, aixi que s'haura de comprovar quan es fasi una crida
posicioFinal :: Int -> Taulell -> Coord
posicioFinal col taulell@(Taulell n m ma) = (posicioFinal2 col taulell ((obteCoord ((filtraPerColor Groc taulell)++(filtraPerColor Vermell taulell)))))
    where
        posicioFinal2 :: Int -> Taulell -> [Coord] -> Coord
        posicioFinal2 col (Taulell n m ma) coord = 
            if [x | (Coord x y) <- coord, y == col] == [] then
                (Coord n col) 
            else
               (Coord ((minimum [x | (Coord x y) <- coord, y == col])-1) col)

ficaFitxa :: Int -> Color -> Taulell -> Taulell
ficaFitxa col color taulell = (insertaPeca (posicioFinal col taulell) color taulell)
  {-            
ficaFitxa :: Int -> Taulell -> Taulell
ficaFitxa col taulell = (ficaFitxaCoord col taulell coord)
    where
        ficaFitxaCoord :: Int -> Taulell -> Coord -> Taulell
        ficaFitxaCoord col (Taulell n m ma) (Coord x y)
        -}
        
-- t = Taulell 6 7 (Map.fromList [((6,1),Groc),((6,2),Groc),((6,3),Groc),((6,4),Vermell),((5,2),Vermell),((5,3),Vermell),((4,2),Vermell)])
    
--consulta al taulell si existeix una peça a les coordenades especificades, retorna nothing si no hi ha res o Just Peca
obteCasella :: Coord-> Taulell -> Maybe Color
obteCasella (Coord n m) (Taulell _ _ ma) = Map.lookup (n,m) ma

insertaPeca :: Coord -> Color -> Taulell -> Taulell
insertaPeca (Coord n m) c (Taulell nn mm t) = (Taulell nn mm (Map.insert (n,m) c t))


    
inicialitzaTaulell :: Int -> Int -> Taulell
inicialitzaTaulell n m = (Taulell n m Map.empty)

detectaGuanyador :: Taulell -> Maybe Color
detectaGuanyador taulell
    |(numEnRatlla 4 Groc taulell) = Just Groc
    |(numEnRatlla 4 Vermell taulell) = Just Vermell
    |otherwise = Nothing


{-
showPeca :: Peca -> Char
showPeca Nothing = ' '
showPeca (Just a) = showColor a

readPeca :: Char -> Peca
readPeca ' ' = Nothing
readPeca c = Just (readColor c)
-}


showColor :: Maybe Color -> Char
showColor Nothing = ' '
showColor (Just c)
    |c == Groc = 'G'
    |otherwise = 'V'

    

readColor :: Char -> Maybe Color
readColor 'G' = (Just Groc)
readColor 'V' = (Just Vermell)
readColor ' ' = Nothing



--IA--
--Random

randomMove :: Taulell -> Color -> IO Int
randomMove taulell@(Taulell n m ma) color = do
    col <- randInt 1 m
    if ((\(Coord x y) -> x) (posicioFinal col taulell) > 0) then do
        return col
    else do
        randomMove taulell color

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).

randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

--Greedy

canviColor :: Color -> Color
canviColor color
    |color == Groc = Vermell
    |otherwise = Groc

--retorna les coord a les que s'ha de tirar per evitar el 4 en ralla del color si hi ha, en cas de que hi hagin 2 posicions que li permetin fer 4 en ratlla, retornarà només la primera, ja que només pot fer un moviment hi haurà perdut igualment, i si per casualitat l'enemic no fa un dels dos moviments guanyadors, a la seguent ronda es tornarà a cridar la funció i retornarà l'unic punt de tall que quedi
tallar4enRalla :: Taulell -> Color -> Maybe Int
tallar4enRalla taulell@(Taulell n m ma) color = tallar4enRalla2 taulell color [(posicioFinal col taulell) | col <- [1 .. m]]
    where 
        tallar4enRalla2 :: Taulell -> Color -> [Coord] ->Maybe Int
        tallar4enRalla2 _ _ [] = Nothing
        tallar4enRalla2 taulell@(Taulell n m ma) color ((Coord x y):xs) 
            |(detectaGuanyador  (ficaFitxa y color taulell) == (Just color)) = (Just y)
            |otherwise = (tallar4enRalla2 taulell color xs)


--numEnRatllaY4 :: Int -> Color -> Taulell -> Bool

--conta la fila més llarga que té un color que es pot arribar a convertir en 4 en ratlla
contaFilaMesLlarga :: Taulell -> Color -> Int 
contaFilaMesLlarga taulell color = maximum [num | num <- [0..4], (numEnRatllaY4 num color taulell)]

{-
contaFilaMesLlarga :: Taulell -> Int 
contaFilaMesLlarga taulell = (contaFilaMesLlarga2 taulell 1)
    where
        contaFilaMesLlarga2 :: Taulell -> Int ->  Int 
        contaFilaMesLlarga2 taulell num
            |(numEnRatlla num Groc taulell) = (contaFilaMesLlarga2 taulell (num+1))
            |otherwise = (num-1)
    
    -}
--[num | num <- [1..4], (numEnRatlla num Groc (Taulell n m ma))]
--(Taulell n m ma) = Taulell 6 7 (Map.fromList [((6,1),Groc),((6,2),Groc),((6,3),Groc),((6,4),Vermell),((5,2),Vermell),((5,3),Vermell),((4,2),Groc),((5,1),Vermell),((4,1),Groc),((3,1),Vermell),((3,2),Vermell),((5,4),Vermell)])



--retorna un [] amb el les coordenades que em permeten posar el maxim nombre de peces seguides, amb num no funciona ja que conta que el maxim nombre de peces es 3 i al posar una més no pot superarho
{-
maximPecesIa :: Taulell -> Color -> [Int]
maximPecesIa (Taulell n m ma) color = maximPecesIa2 (Taulell n m ma) color (contaFilaMesLlarga (Taulell n m ma) color)  [(posicioFinal col (Taulell n m ma)) | col <- [1 .. m]]
    where --retorna totes les columnes [Int] a on si tirem una peça incrementara la linia de peces més llarga
        maximPecesIa2 :: Taulell -> Color -> Int -> [Coord] -> [Int]
        maximPecesIa2 _ _ _ [] = []
        maximPecesIa2 (Taulell n m ma) color maxi ((Coord x y):xs) 
            |((contaFilaMesLlarga (ficaFitxa y color (Taulell n m ma) color)) > maxi) =[y] ++ (maximPecesIa2 (Taulell n m ma) color (contaFilaMesLlarga (ficaFitxa y color (Taulell n m ma)))  xs)
            |otherwise = (maximPecesIa2 (Taulell n m ma) color maxi xs)
            -}
            
--retorna una llista amb les columnes a les que tirar una peça maximitza
maximPecesIa :: Taulell -> Color -> [Int]
maximPecesIa taulell@(Taulell n m ma) color = [y | y <- [1..m], taux <- [(ficaFitxa y color taulell)], (contaFilaMesLlarga taulell color) < (contaFilaMesLlarga taux color), (\(Coord x y) -> x) (posicioFinal y taulell) > 0 ]

            
      --  [y | y <- [1..m], taux <- [(ficaFitxa y Groc (Taulell n m ma))] , (contaFilaMesLlarga (Taulell n m ma)) < (contaFilaMesLlarga taux)]
--escullInt :: Maybe Int -> Int 
--escollInt (Just x) = x


--el greedy necessita aquesta funció per escollir una columna d'entre diverses possibilitats igual de bones, simplement agafa la de la meitat, aquesta decisió és arbitraria ja que dona igual quina agafi
escullTirada :: Taulell -> [Int] -> Int
escullTirada (Taulell n m ma) [] = head [y | y <- [1..m],(\(Coord x y) -> x) (posicioFinal y (Taulell n m ma)) > 0 ]
escullTirada _ [x] = x
escullTirada _ xs =  last (take ((length xs) `div` 2) xs)

greedyIO :: Taulell -> Color -> IO Int 
greedyIO taulell color= do
    let res = greedy taulell color
    return res

greedy :: Taulell -> Color -> Int
greedy taulell@(Taulell n m ma) color
    |(esJust (tallar4enRalla taulell (canviColor color))) = (\(Just i)->i) $ (tallar4enRalla taulell (canviColor color))
    |otherwise = escullTirada taulell (maximPecesIa taulell color)
    
 --------------------------------------------------
 -------------------------------------------------
 
 ----------SMART-------------
 ----------------------------
 

--Realitza una ponderació en forma de campana de Gauss, de forma que les columnes dels extrems valen 0 punts i van sumant x fins arribar al punt mitg 
punts :: Int -> Int -> Int 
punts m col = ((minimum [(col-1),(m-col)]) * 0)

 --numEnRatllaY4VerticalHoritzontalDiagonal :: Int -> Coord -> Taulell -> Color -> Bool

--numVerticalDaltPunts :: Int -> Coord -> Taulell

--suma 1 per cada Num en ratlla (amb possibilitat de ser 4 en ratlla en algun moment) que es troba des de les coordenades especificades. És a dir, si num val 3, retorna el nombre de 3 en ratlla que troba. Utilitza fromEnum per a convertir els booleans que retornen les funcions que crida en 1 i 0, de forma que si val 1 vol dir que ha trobat un Num en ratlla i per tant s'ha de contar
numEnRatllaY4VerticalHoritzontalDiagonalPunts :: Int -> Coord -> Taulell -> Color -> Int
numEnRatllaY4VerticalHoritzontalDiagonalPunts num (Coord x y) taulell color = 
    (fromEnum ((numVerticalDalt num (Coord x y) taulell color 0) && (numEspaisVerticalDalt (4-num) (Coord (x-num) y)) taulell 0)) + 
    (fromEnum ((numHoritzontalEsquerra num (Coord x y) taulell color 0) && (numEspaisHoritzontalEsquerra (4-num) (Coord x (y-num)) taulell 0))) + 
    (fromEnum ((numHoritzontalDreta num (Coord x y) taulell color 0) && (numEspaisHoritzontalDreta (4-num) (Coord x (y+num)) taulell  0))) + 
    (fromEnum ((numDiagonalEsquerraDalt num (Coord x y) taulell color 0) && (numEspaisDiagonalEsquerraDalt (4-num) (Coord (x-num) (y-num)) taulell  0))) + 
    (fromEnum ((numDiagonalDretaDalt num (Coord x y) taulell color 0) && (numEspaisDiagonalDretaDalt (4-num) (Coord (x-num) (y+num)) taulell  0)))

{-
+ (fromEnum ((numHoritzontalEsquerra num (Coord x y) taulell color 0) && (numEspaisHoritzontalEsquerra (4-num) (Coord x (y-num)) taulell 0))) + (fromEnum ((numHoritzontalDreta num (Coord x y) taulell color 0) && (numEspaisHoritzontalDreta (4-num) (Coord x (y+num)) taulell  0))) + (fromEnum ((numDiagonalEsquerraDalt num (Coord x y) taulell color 0) && (numEspaisDiagonalEsquerraDalt (4-num) (Coord (x-num) (y-num)) taulell  0))) + (fromEnum ((numDiagonalDretaDalt num (Coord x y) taulell color 0) && (numEspaisDiagonalDretaDalt (4-num) (Coord (x-num) (y+num)) taulell  0)))
-}
       
--avalua el taulell segons la distribució de peces del color escollit, suma 10 per cada seqüència de 2 fitxes seguides i 20 per cadascuna de 3 seguides. També mira totes les peces individualment i suma punts contra més centrades estiguin, ja que en general, com estar al centre del taulell dóna més possibilitats de joc, és millor idea centrar les peces.
avaluaPosicioPeces :: Taulell -> Color -> Int 
avaluaPosicioPeces taulell@(Taulell n m ma) color =  
    (10* (sum [(numEnRatllaY4VerticalHoritzontalDiagonalPunts 2 coord taulell color) |  coord <- (obteCoord(filtraPerColor color taulell))])) + 
    (20* (sum [(numEnRatllaY4VerticalHoritzontalDiagonalPunts 3 coord taulell color) |  coord <- (obteCoord(filtraPerColor color taulell))])) + 
    (sum [(punts m col) | (Coord x col) <- (obteCoord(filtraPerColor color taulell))])
        
        
            {-
avaluaPosicioPeces :: Taulell -> Color -> Int 
avaluaPosicioPeces taulell color = avaluaPosicioPeces2 taulell color (obteCoord(filtraPerColor color taulell))
    where
        avaluaPosicioPeces2 :: Taulell -> Color -> [Coord]
        avaluaPosicioPeces2 (Taulell n m ma) color (x:xs) =
        -}
        
--retorna tots els taulells que es poden generar a partir d'un taulell inicial, és a dir, és el resultat de col·locar 1 fitxa a cada columna al taulell inicial
taulellsGenerables :: Taulell -> Color -> [Taulell]
taulellsGenerables taulell@(Taulell n m ma) color = [(ficaFitxa col color taulell) | col <- [1..m], (\(Coord x y) -> x) (posicioFinal col taulell) > 0]

--retorna totes les columnes a on es podria ficar una fitxa
taulellsGenerablesCol :: Taulell -> [Int]
taulellsGenerablesCol taulell@(Taulell n m ma)  = [ col | col <- [1..m], (\(Coord x y) -> x) (posicioFinal col taulell) > 0]


smartMinMaxIO :: Taulell -> Color -> IO Int 
smartMinMaxIO taulell color = do
    let res = smartMinMax taulell color
    return res
    

--segueix l'estratègia minmax per tal de generar un arbre de moviments possibles, els quals seran avaluats per obtenir una puntuació (els punts Grocs sumen i els Vermells resten), mitjançant la suposició de què l'enemic sempre farà el seu millor moviment, el jugador intentarà maximitzar o minimitzar la puntuació i escollirà el moviment que li porti a aquest resultat. 
smartMinMax :: Taulell -> Color -> Int
smartMinMax  taulell color  
    |color == Groc =  (\(punts,colum)-> colum)(maxim [((smartMinMaxRec 3 (ficaFitxa col color taulell) (canviColor color) False),col) | col <- (taulellsGenerablesCol taulell)])
    |otherwise = (\(punts,colum)-> colum) (minim [((smartMinMaxRec 3 (ficaFitxa col color taulell) (canviColor color) True),col) | col <- (taulellsGenerablesCol taulell)])
    
    
smartMinMaxRec :: Int -> Taulell -> Color -> Bool -> Int
smartMinMaxRec depth taulell@(Taulell n m ma) color maximitza = 
    if (numEnRatllaY4 4 Vermell taulell) then
            -1000
    else if  (numEnRatllaY4 4 Groc taulell) then
            1000
    else
        if depth == 0 then
            ((avaluaPosicioPeces taulell Groc)-(avaluaPosicioPeces taulell Vermell))
        else
            if maximitza then
                maximum [(smartMinMaxRec (depth - 1) tau (canviColor color) False) | tau <-(taulellsGenerables taulell color)]
            else
                minimum [(smartMinMaxRec (depth - 1) tau (canviColor color) True) | tau <-(taulellsGenerables taulell color)]
                    
--retorna la parella (punts,col) amb punts màxims        
maxim :: [(Int,Int)] -> (Int,Int)
maxim ((punts,col):xs)
    |xs == [] || punts > puntsxs = (punts,col)
    |otherwise = (puntsxs,colxs)
    where (puntsxs,colxs) = maxim xs
          
--retorna la parella (punts,col) amb punts mínims        
minim :: [(Int,Int)] -> (Int,Int)
minim ((punts,col):xs)
    |xs == [] || punts < puntsxs = (punts,col)
    |otherwise = (puntsxs,colxs)
    where (puntsxs,colxs) = minim xs
{-    
myMaximum :: [Int] -> Int
myMaximum [last] = last --si nomes queda lultim element el max es lultim element
myMaximum (head:tail) = head `max` (myMaximum tail) 

maxim ::[(Int,Int)] -> Int 
maxim [(punts,col)] = (punts,col)
maxim ((punts,col):xs)
    |punts > 
    -}
    
    {-
--(Punts, Col) retorna la Col que permet generar el maxim o el minim de punts
maxOMin :: (Int,Int) -> Bool -> Int
maxOMin (punts,col) maximitza 
    |maximitza =


--(Int,Int) = (Punts, Col)
smartMinMaxRec :: Int -> Taulell -> Color -> Bool -> Int
smartMinMaxRec depth taulell@(Taulell n m ma) color maximitza = 
    if depth == 0 then
        ((avaluaPosicioPeces taulell Groc)+(avaluaPosicioPeces taulell Vermell))
    else
        if maximitza then
            maximum [(smartMinMaxRec tau (canviColor color) False),tau <- (taulellsGenerables taulell color)]
        else
            minimum [(smartMinMaxRec tau (canviColor color) True),tau <- (taulellsGenerables taulell color)]
            -}
                   
                                                        
 ----------------------------
 ----------------------------
 
{-
main = do
    putStrLn("Defineix la mida del taulell n*m")
    putStrLn("Introdueix n i m separades per un espai: ")
    line <- getLine
    putStrLn(line)
    do a <- inicialitzaTaulell (words line)
    in
    
    -}
    {- main = forever $ putStrLn "do something" -}
    


empat :: Taulell -> Bool
empat (Taulell n m ma) 
    |[ col | col <- [1..m], (\(Coord x y) -> x) (posicioFinal col (Taulell n m ma)) > 0] == [] = True
    |otherwise = False
    
quinaEstrategia :: Int -> Estrategia
quinaEstrategia ia
    |ia == 1 = Random
    |ia == 2 = Greedy
    |otherwise = Smart

fesMoviment :: Estrategia -> Taulell -> Taulell
fesMoviment x taulell
    |x == Greedy = (ficaFitxa (greedy taulell Groc) Groc taulell)
    |otherwise = undefined

jugada :: (Taulell -> Color -> IO Int) -> Taulell -> Color -> IO Taulell
jugada fun taulell color = do
    x <- (fun taulell color)
    let tau = (ficaFitxa x color taulell)
    return tau


    
iaGestio :: Estrategia -> Taulell -> IO()
iaGestio ia taulell@(Taulell n m ma) = do
    if ia == Random then do
        taulellAct <- (jugada randomMove taulell Groc)
        putStrLn(show taulellAct)
        if ((detectaGuanyador taulellAct) == (Just Groc)) then do
            putStrLn("HAS PERDUT!  :(")
            return()
        else
            (humaGestio ia taulellAct)
        
    else if ia == Greedy then do
        taulellAct <- (jugada greedyIO taulell Groc)
       -- let taulellAct = (jugada greedyIO taulell Groc)
        putStrLn(show taulellAct)
        if ((detectaGuanyador taulellAct) == (Just Groc)) then do
            putStrLn("HAS PERDUT!  :(")
            return()
        else if (empat taulell) then do
            putStrLn("EMPAT!  :/")
            return()
        else
            (humaGestio ia taulellAct)
    else do
        taulellAct <- (jugada smartMinMaxIO taulell Groc)
        putStrLn(show taulellAct)
        if ((detectaGuanyador taulellAct) == (Just Groc)) then do
            putStrLn("HAS PERDUT!  :(")
            return()
        else if (empat taulellAct) then do
            putStrLn("EMPAT!  :/")
            return()
        else
            (humaGestio ia taulellAct)
            
            
humaGestio :: Estrategia -> Taulell -> IO()
humaGestio ia taulell = do
        putStrLn("Tria la columna on vols deixar la peça: ")
        x <- getLine
        let col = (read x::Int)
        --ficaFitxa :: Int -> Color -> Taulell -> Taulell
        --putStrLn("Col = " ++ x ++ read(((\(Coord x y) -> x) (posicioFinal col taulell)) > 0)::[Char])
       -- if (((\(Coord x y) -> x) (posicioFinal col taulell)) > 0) then do
        --    putStrLn("AQUESTA POSICIÓ ESTÀ FORA DEL TAULELL, ESCULL UNA ALTRE")
        --    (humaGestio ia taulell)
       -- else do
        let taulellAct = (ficaFitxa col Vermell taulell)
        putStrLn(show taulellAct)
        --detectaGuanyador :: Taulell -> Maybe Color
        if ((detectaGuanyador taulellAct) == (Just Vermell)) then do
            putStrLn("HAS GUANYAT!")
            return()
        else if (empat taulellAct) then do
            putStrLn("EMPAT!  :/")
            return()
        else 
            (iaGestio ia taulellAct)
            
        
main :: IO ()
main = do
    putStrLn("Defineix la mida del taulell N*M.")
    putStrLn("Introdueix N : ")
    line <- getLine
    let n = (read line::Int)
    putStrLn("Introdueix M : ")
    linee <- getLine
    let m = (read linee::Int)
    putStrLn("Es creará un taulell de " ++ line ++ " per " ++ linee)
    putStrLn(show (inicialitzaTaulell n m))
    let taulell = inicialitzaTaulell n m
    putStrLn("Escull la dificultat de la IA: Contesta 1, 2, o 3")
    putStrLn("  1. Random")
    putStrLn("  2. Greedy")
    putStrLn("  3. Smart")
    line <- getLine
    putStrLn("Has escollit la dificultat " ++ line)
    let ia = (quinaEstrategia (read line::Int))
    putStrLn("Vols començar tu? Contesta si o no.")
    si <- getLine
    let taulellAct = taulell
    if si == "si" then do
        (humaGestio ia taulell)
        
    else
        (iaGestio ia taulell)
        {-
        if ia == 1 then do
            r1 <- randInt 1 m
            let taulellAct = (ficaFitxa r1 Groc taulell)
            putStrLn(show taulellAct)
        else
            return () 
            -}
            
            
            {-
            TODO:
            -Arreglar greedy.
            -intentar utilitzar més coses de haskell
            
            -Lo que me dijo el profe de pasar estrategia como parametro
            
            -Que no se puedan poner fichas si la columna esta llena
            -funcion de empate !!!!!!!!!
            -mirar que pasa con los maximums i los minimums
            -Borrar funcions que no s'utilitzen
            --arreglar la funcion de puntuar para que no cuente 2 veces las filas de 3
            - la funcio de punts tambe compta peces que estan tancades i per tant no serveixen de res
            -}
