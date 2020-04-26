# 4 EN RATLLA

Aquest projecte programat en Haskell et permet jugar al 4 en Ratlla contra 3 IA diferents. 

## Com jugar

A continuació s'expliquen els passos a seguir per provar el joc.

### Prerequisits

Cal tenir instal·lat el compilador ghc.
Requereix poder importar:

```
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import System.Random
```

### Compilar i executar

1-Descomprimir l'archiu.

```
Give the example
```

2-Compilar:

```
ghc joc.hs
```

3-Executar
```
./joc
```




### Explicacions i observacions

El codi que es troba a l'arxiu joc.hs té comentaris explicant les funcions.

El jugador humà juga amb les fitxes vermelles (V) i la IA amb les grogues (G).

El taulell té la numeració de les columnes just a sota, funcionant de terra.

Quan el joc demana un moviment s'ha d'introduir unicament el número de la columna on vols deixar la peça.

L'estrategia smart no sempre fa el 4 en ratlla quan pot, això es degut a que fa un altre moviment que ja sap que té 100% de posibilitats d'acabar guanyant igualment, i fa més tirades de les necesaries per guanyar. Ho he deixat així ja que em semba bé que faci això, perquè sembla que s'hagi equivocat i sembla una mica més humà.
```
Give an example
```


