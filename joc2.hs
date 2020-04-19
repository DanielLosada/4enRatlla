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
    
esGroc :: Coord -> Taulell -> Bool
esGroc (Coord n m) (Taulell nn mm ma) 
    |Map.lookup (n,m) ma == Just Groc = True
    |otherwise = False

esVermell :: Coord -> Taulell -> Bool
esVermell (Coord n m) (Taulell nn mm ma) 
    |Map.lookup (n,m) ma == Just Vermell = True
    |otherwise = False
    
    
cuatreDiagonalEsquerraDalt :: Coord -> Taulell -> Color -> Int -> Bool
cuatreDiagonalEsquerraDalt (Coord x y) (Taulell n m ma) color count
    |count > 3 = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreDiagonalEsquerraDalt (Coord (x-1) (y-1)) (Taulell n m ma) color (count+1))

cuatreDiagonalEsquerraBaix :: Coord -> Taulell -> Color -> Int -> Bool
cuatreDiagonalEsquerraBaix (Coord x y) (Taulell n m ma) color count
    |count > 3 = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreDiagonalEsquerraBaix (Coord (x+1) (y-1)) (Taulell n m ma) color (count+1))

cuatreDiagonalDretaDalt :: Coord -> Taulell -> Color -> Int -> Bool
cuatreDiagonalDretaDalt (Coord x y) (Taulell n m ma) color count
    |count > 3 = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreDiagonalDretaDalt (Coord (x-1) (y+1)) (Taulell n m ma) color (count+1))

cuatreDiagonalDretaBaix :: Coord -> Taulell -> Color -> Int -> Bool
cuatreDiagonalDretaBaix (Coord x y) (Taulell n m ma) color count
    |count > 3 = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreDiagonalDretaBaix (Coord (x+1) (y+1)) (Taulell n m ma) color (count+1))


cuatreDiagonal :: Coord -> Taulell -> Color -> Bool
cuatreDiagonal coord taulell color
    |(cuatreDiagonalEsquerraDalt coord taulell color 0) = True
    |(cuatreDiagonalEsquerraBaix coord taulell color 0) = True
    |(cuatreDiagonalDretaDalt coord taulell color 0) = True
    |(cuatreDiagonalDretaBaix coord taulell color 0) = True
    |otherwise = False
    
cuatreHoritzontalEsquerra :: Coord -> Taulell -> Color -> Int -> Bool
cuatreHoritzontalEsquerra (Coord x y) (Taulell n m ma) color count
    |count > 3 = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreHoritzontalEsquerra (Coord x (y-1)) (Taulell n m ma) color (count+1))

cuatreHoritzontalDreta :: Coord -> Taulell -> Color -> Int -> Bool
cuatreHoritzontalDreta (Coord x y) (Taulell n m ma) color count
    |count > 3 = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreHoritzontalDreta (Coord x (y+1)) (Taulell n m ma) color (count+1))
    
    
cuatreHoritzontal :: Coord -> Taulell -> Color -> Bool
cuatreHoritzontal coord taulell color
    |(cuatreHoritzontalEsquerra coord taulell color 0) = True
    |(cuatreHoritzontalDreta coord taulell color 0) = True
    |otherwise = False

cuatreVerticalDalt ::  Coord -> Taulell -> Color -> Int -> Bool
cuatreVerticalDalt (Coord x y) (Taulell n m ma) color count
    |count > 3 = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreVerticalDalt (Coord (x+1) y) (Taulell n m ma) color (count+1))
    
cuatreVerticalBaix ::  Coord -> Taulell -> Color -> Int -> Bool
cuatreVerticalBaix (Coord x y) (Taulell n m ma) color count
    |count > 3 = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreVerticalBaix (Coord (x-1) y) (Taulell n m ma) color (count+1))

cuatreVertical :: Coord -> Taulell -> Color -> Bool
cuatreVertical coord taulell color
    |(cuatreVerticalDalt coord taulell color 0) = True
    |(cuatreVerticalBaix coord taulell color 0) = True
    |otherwise = False

cuatreVerticalHoritzontalDiagonal :: Coord -> Taulell -> Color -> Bool
cuatreVerticalHoritzontalDiagonal coord taulell color
    |(cuatreVerticalDalt coord taulell color 0) = True
    |(cuatreVerticalBaix coord taulell color 0) = True
    |(cuatreHoritzontalEsquerra coord taulell color 0) = True
    |(cuatreHoritzontalDreta coord taulell color 0) = True
    |(cuatreDiagonalEsquerraDalt coord taulell color 0) = True
    |(cuatreDiagonalEsquerraBaix coord taulell color 0) = True
    |(cuatreDiagonalDretaDalt coord taulell color 0) = True
    |(cuatreDiagonalDretaBaix coord taulell color 0) = True
    |otherwise = False
    
--llistaGrocs :: Taulell -> [Coord]
--llistaGrocs (Taulell n m ma) = unlines $ [[(Coord nn mm) | mm <- [1.. m]] | nn <- [1..n], esGroc((Coord nn mm) (Taulell n m ma))]
    
--Donat un color i un taulell diu si hi ha 4 en ratlla d'aquets color
cuatreEnRatlla :: Color -> Taulell -> Bool
cuatreEnRatlla color taulell = (cuatreEnRatllaCoord color taulell (obteCoord (filtraPerColor color taulell)))
    where
        cuatreEnRatllaCoord :: Color -> Taulell -> [Coord] -> Bool
        cuatreEnRatllaCoord _ _ [] = False
        cuatreEnRatllaCoord color taulell (x:xs) = (cuatreVerticalHoritzontalDiagonal x taulell color) || (cuatreEnRatllaCoord color taulell xs)
            

--retorna la coordenada a la que cauria una peça al ser llençada per una columna, pot retornar un valor fora del taulell en cas que una columna estigui plena o s'introdueixi una columna inexistent, aixi que s'haura de comprovar quan es fasi una crida
posicioFinal :: Int -> Taulell -> Coord
posicioFinal col (Taulell n m ma) = (posicioFinal2 col (Taulell n m ma) ((obteCoord ((filtraPerColor Groc (Taulell n m ma))++(filtraPerColor Vermell (Taulell n m ma))))))
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
    |(cuatreEnRatlla Groc taulell) = Just Groc
    |(cuatreEnRatlla Vermell taulell) = Just Vermell
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
randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).

randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result
    


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
quinaEstrategia :: Int -> Estrategia
quinaEstrategia ia
    |ia == 1 = Random
    |ia == 2 = Greedy
    |otherwise = Smart
    
iaGestio :: Estrategia -> Taulell -> IO()
iaGestio ia (Taulell n m ma) = do
    if ia == Random then do
        r1 <- (randInt 1 m)
        let taulellAct = (ficaFitxa r1 Groc (Taulell n m ma))
        putStrLn(show taulellAct)
        if ((detectaGuanyador taulellAct) == (Just Groc)) then do
            putStrLn("HAS PERDUT!  :(")
            return()
        else
            (humaGestio ia taulellAct)
        
    else if ia == Greedy then
        putStrLn("2")
    else
        putStrLn("3")

humaGestio :: Estrategia -> Taulell -> IO()
humaGestio ia taulell = do
        putStrLn("Tria la columna on vols deixar la peça: ")
        x <- getLine
        let col = (read x::Int)
        --ficaFitxa :: Int -> Color -> Taulell -> Taulell
        let taulellAct = (ficaFitxa col Vermell taulell)
        putStrLn(show taulellAct)
        --detectaGuanyador :: Taulell -> Maybe Color
        if ((detectaGuanyador taulellAct) == (Just Vermell)) then do
            putStrLn("HAS GUANYAT!")
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
