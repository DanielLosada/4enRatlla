import Data.Map.Strict (Map)
import qualified Data.Map as Map


data Taulell = Taulell
    { n :: Int
    , m :: Int
    , casella :: Map (Int, Int) Color
    }
    
data Coord = Coord Int Int

data Color = Groc | Vermell deriving(Show, Eq)

{-
instance Show Taulell where
    show (Taulell n m ma) = unlines [concat [(show i) | i <- [0 .. m-1]] ++ "\n" ++
                            [[(showColor (obteCasella (Coord nn mm) (Taulell n m ma))) |
                             nn <- [0 .. n-1]] | mm <- [0 .. m-1]]    ]
                             -}
                             
instance Show Taulell where
    show (Taulell n m ma) = unlines $  [[(showColor (obteCasella (Coord nn mm) (Taulell n m ma))) | mm <- [1.. m]] | nn <- [1..n]] ++ [concat [(show i) | i <- [1 .. m]]]                            


esGroc :: Coord -> Taulell -> Bool
esGroc (Coord n m) (Taulell nn mm ma) 
    |Map.lookup (n,m) ma == Just Groc = True
    |otherwise = False

esVermell :: Coord -> Taulell -> Bool
esVermell (Coord n m) (Taulell nn mm ma) 
    |Map.lookup (n,m) ma == Just Vermell = True
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
    
--llistaGrocs :: Taulell -> [Coord]
--llistaGrocs (Taulell n m ma) = unlines $ [[(Coord nn mm) | mm <- [1.. m]] | nn <- [1..n], esGroc((Coord nn mm) (Taulell n m ma))]
    
--Donat un color i un taulell diu si hi ha 4 en ratlla d'aquets color
cuatreEnRatlla :: Color -> Taulell -> Bool
cuatreEnRatlla = undefined


-- t = Taulell 6 7 (Map.fromList [((6,1),Groc),((6,2),Groc),((6,3),Groc),((6,4),Vermell),((5,2),Vermell),((5,3),Vermell),((4,2),Vermell)])
    
--consulta al taulell si existeix una peça a les coordenades especificades, retorna nothing si no hi ha res o Just Peca
obteCasella :: Coord-> Taulell -> Maybe Color
obteCasella (Coord n m) (Taulell _ _ ma) = Map.lookup (n,m) ma

insertaPeca :: Coord -> Color -> Taulell -> Taulell
insertaPeca (Coord n m) c (Taulell nn mm t) = (Taulell nn mm (Map.insert (n,m) c t))


    
inicialitzaTaulell :: Int -> Int -> Taulell
inicialitzaTaulell n m = (Taulell n m Map.empty)

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

