import Data.Map.Strict (Map)
import qualified Data.Map as Map


data Taulell = Taulell
    { n    :: Int
    , m :: Int
    , casella :: Map (Int, Int) Color
    }
    
data Coord = Coord Int Int

data Color = Groc | Vermell deriving(Show, Eq)

{-# LANGUAGE ParallelListComp #-}
instance Show Taulell where
    show (Taulell n m ma) = unlines [concat [(show i) | i <- [0 .. m-1]] ++ "\n" ++
                            [(showColor (obteCasella (Coord nn mm) (Taulell n m ma))) |
                             nn <- [0 .. n-1] , mm <- [0 .. m-1]]    ]
        

    
    
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

