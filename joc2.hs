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
    
    
cuatreDiagonalEsquerraDalt :: Int -> Coord -> Taulell -> Color -> Int -> Bool
cuatreDiagonalEsquerraDalt num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreDiagonalEsquerraDalt num (Coord (x-1) (y-1)) (Taulell n m ma) color (count+1))

cuatreDiagonalEsquerraBaix :: Int -> Coord -> Taulell -> Color -> Int -> Bool
cuatreDiagonalEsquerraBaix num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreDiagonalEsquerraBaix num (Coord (x+1) (y-1)) (Taulell n m ma) color (count+1))

cuatreDiagonalDretaDalt :: Int ->Coord -> Taulell -> Color -> Int -> Bool
cuatreDiagonalDretaDalt num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreDiagonalDretaDalt num (Coord (x-1) (y+1)) (Taulell n m ma) color (count+1))

cuatreDiagonalDretaBaix :: Int -> Coord -> Taulell -> Color -> Int -> Bool
cuatreDiagonalDretaBaix num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreDiagonalDretaBaix num (Coord (x+1) (y+1)) (Taulell n m ma) color (count+1))


cuatreDiagonal :: Int -> Coord -> Taulell -> Color -> Bool
cuatreDiagonal num coord taulell color
    |(cuatreDiagonalEsquerraDalt num coord taulell color 0) = True
    |(cuatreDiagonalEsquerraBaix num coord taulell color 0) = True
    |(cuatreDiagonalDretaDalt num coord taulell color 0) = True
    |(cuatreDiagonalDretaBaix num coord taulell color 0) = True
    |otherwise = False
    
cuatreHoritzontalEsquerra :: Int ->Coord -> Taulell -> Color -> Int -> Bool
cuatreHoritzontalEsquerra num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreHoritzontalEsquerra num (Coord x (y-1)) (Taulell n m ma) color (count+1))

cuatreHoritzontalDreta :: Int -> Coord -> Taulell -> Color -> Int -> Bool
cuatreHoritzontalDreta num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreHoritzontalDreta num (Coord x (y+1)) (Taulell n m ma) color (count+1))
    
    
cuatreHoritzontal :: Int -> Coord -> Taulell -> Color -> Bool
cuatreHoritzontal num coord taulell color
    |(cuatreHoritzontalEsquerra num coord taulell color 0) = True
    |(cuatreHoritzontalDreta num coord taulell color 0) = True
    |otherwise = False

cuatreVerticalDalt :: Int -> Coord -> Taulell -> Color -> Int -> Bool
cuatreVerticalDalt num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreVerticalDalt num (Coord (x+1) y) (Taulell n m ma) color (count+1))
    
cuatreVerticalBaix :: Int -> Coord -> Taulell -> Color -> Int -> Bool
cuatreVerticalBaix num (Coord x y) (Taulell n m ma) color count
    |count >= num = True
    |x < 1 || y < 1 || x > n || y > m = False 
    |Map.lookup (x,y) ma /= (Just color) = False
    |Map.lookup (x,y) ma == (Just color) = (cuatreVerticalBaix num (Coord (x-1) y) (Taulell n m ma) color (count+1))

cuatreVertical :: Int -> Coord -> Taulell -> Color -> Bool
cuatreVertical num coord taulell color
    |(cuatreVerticalDalt num coord taulell color 0) = True
    |(cuatreVerticalBaix num coord taulell color 0) = True
    |otherwise = False

cuatreVerticalHoritzontalDiagonal :: Int -> Coord -> Taulell -> Color -> Bool
cuatreVerticalHoritzontalDiagonal num coord taulell color
    |(cuatreVerticalDalt num coord taulell color 0) = True
    |(cuatreVerticalBaix num coord taulell color 0) = True
    |(cuatreHoritzontalEsquerra num coord taulell color 0) = True
    |(cuatreHoritzontalDreta num coord taulell color 0) = True
    |(cuatreDiagonalEsquerraDalt num coord taulell color 0) = True
    |(cuatreDiagonalEsquerraBaix num coord taulell color 0) = True
    |(cuatreDiagonalDretaDalt num coord taulell color 0) = True
    |(cuatreDiagonalDretaBaix num coord taulell color 0) = True
    |otherwise = False
    
--llistaGrocs :: Taulell -> [Coord]
--llistaGrocs (Taulell n m ma) = unlines $ [[(Coord nn mm) | mm <- [1.. m]] | nn <- [1..n], esGroc((Coord nn mm) (Taulell n m ma))]
    
--Donat un color i un taulell diu si hi ha 4 en ratlla d'aquets color
cuatreEnRatlla :: Int -> Color -> Taulell -> Bool
cuatreEnRatlla num color taulell = (cuatreEnRatllaCoord num color taulell (obteCoord (filtraPerColor color taulell)))
    where
        cuatreEnRatllaCoord :: Int -> Color -> Taulell -> [Coord] -> Bool
        cuatreEnRatllaCoord _ _ _ [] = False
        cuatreEnRatllaCoord num color taulell (x:xs) = (cuatreVerticalHoritzontalDiagonal num x taulell color) || (cuatreEnRatllaCoord num color taulell xs)
            

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
    |(cuatreEnRatlla 4 Groc taulell) = Just Groc
    |(cuatreEnRatlla 4 Vermell taulell) = Just Vermell
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

--Greedy

--retorna les coord a les que s'ha de tirar per evitar el 4 en ralla de l'enemic si hi ha
tallar4enRalla :: Taulell -> Maybe Int
tallar4enRalla (Taulell n m ma) = tallar4enRalla2 (Taulell n m ma) [(posicioFinal col (Taulell n m ma)) | col <- [1 .. m]]
    where 
        tallar4enRalla2 :: Taulell -> [Coord] ->Maybe Int
        tallar4enRalla2 _ [] = Nothing
        tallar4enRalla2 (Taulell n m ma) ((Coord x y):xs) 
            |(detectaGuanyador  (ficaFitxa y Vermell (Taulell n m ma)) == (Just Vermell)) = (Just y)
            |otherwise = (tallar4enRalla2 (Taulell n m ma) xs)
            
contaFilaMesLlarga :: Taulell -> Int 
contaFilaMesLlarga taulell = (contaFilaMesLlarga2 taulell 1)
    where
        contaFilaMesLlarga2 :: Taulell -> Int ->  Int 
        contaFilaMesLlarga2 taulell num
            |(cuatreEnRatlla num Groc taulell) = (contaFilaMesLlarga2 taulell (num+1))
            |otherwise = (num-1)
            
--retorna un [] amb el les coordenades que em permeten posar el maxim nombre de peces seguides, amb t = Taulell 6 7 (Map.fromList [((6,1),Groc),((6,2),Groc),((6,3),Groc),((6,4),Vermell),((5,2),Vermell),((5,3),Vermell),((4,2),Vermell),((5,1),Vermell),((4,1),Vermell),((3,1),Groc)]) no funciona ja que conta que el maxim nombre de peces es 3 i al posar una més no pot superarho

maximPecesIa :: Taulell -> [Coord]
maximPecesIa (Taulell n m ma) = maximPecesIa2 (Taulell n m ma) (contaFilaMesLlarga (Taulell n m ma))  [(posicioFinal col (Taulell n m ma)) | col <- [1 .. m]]
    where
        maximPecesIa2 :: Taulell -> Int -> [Coord] -> [Coord]
        maximPecesIa2 _ _ [] = []
        maximPecesIa2 (Taulell n m ma) maxi ((Coord x y):xs) 
            |((contaFilaMesLlarga (ficaFitxa y Groc (Taulell n m ma))) > maxi) =[(Coord x y)] ++ (maximPecesIa2 (Taulell n m ma) (contaFilaMesLlarga (ficaFitxa y Vermell (Taulell n m ma)))  xs)
            |otherwise = (maximPecesIa2 (Taulell n m ma) maxi xs)

--escullInt :: Maybe Int -> Int 
--escollInt (Just x) = x

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False



greedy :: Taulell -> Int
greedy taulell 
    |(isJust (tallar4enRalla taulell)) = (\(Just i)->i) $ (tallar4enRalla taulell)
    |otherwise = 
    
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
