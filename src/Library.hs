module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Auto = UnAuto {
    color :: String,
    velocidad :: Number,
    distancia :: Number
} deriving Show

type Evento = String

data Carrera = UnaCarrera {
    autos :: [Auto],
    eventos :: [Evento]
} deriving Show

rojo, azul, verde, amarillo, naranja, violeta, rosa, negro, blanco :: Auto
rojo =      UnAuto "Rojo"       10 100
azul =      UnAuto "Azul"       20 100
verde =     UnAuto "Verde"      15 105
amarillo =  UnAuto "Amarillo"   25 150
naranja =   UnAuto "Naranja"    40 160
violeta =   UnAuto "Violeta"    30 180
rosa =      UnAuto "Rosa"       35 191
negro =     UnAuto "Negro"      25 800
blanco =    UnAuto "Blanco"     25 1000

carrera :: Carrera
carrera = UnaCarrera [rojo, azul, verde, amarillo, naranja, violeta, rosa, negro, blanco] []

------------------------------------------------------------------------------------------------------
--------------------------------------------1a--------------------------------------------------------
------------------------------------------------------------------------------------------------------

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2
    | verificarSiSonDistintos auto1 auto2 = esCorta . ditanciaEntreAutos auto1 $ auto2
    | otherwise = False

verificarSiSonDistintos :: Auto -> Auto -> Bool
verificarSiSonDistintos auto1 auto2
    | color auto1 == color auto2 = False
    | otherwise = True

ditanciaEntreAutos :: Auto -> Auto -> Number
ditanciaEntreAutos auto1 auto2 = abs (distancia auto1 - distancia auto2)

esCorta :: Number -> Bool
esCorta distanciaAutos
    | distanciaAutos < 10 = True
    | otherwise = False

--1b

vaTranquilo :: Carrera -> Auto -> Bool
vaTranquilo carrera auto = noTieneNingunoCerca carrera auto && vaPrimero carrera auto

noTieneNingunoCerca :: Carrera -> Auto -> Bool
noTieneNingunoCerca carrera auto = not (any (estaCerca auto) (autos carrera))

vaPrimero :: Carrera -> Auto -> Bool
vaPrimero carrera auto = puesto carrera auto ==1

esIgual :: Auto -> Auto -> Bool
esIgual auto1 auto2
    | color auto1 == color auto2 = True
    | otherwise = False

--1c

puesto :: Carrera -> Auto -> Number
puesto carrera auto = (cuantosTieneAdelante (autos carrera) auto 0) +1

cuantosTieneAdelante :: [Auto] -> Auto -> Number -> Number
cuantosTieneAdelante (auto1:[]) auto posicion
    | verificarSiSonDistintos auto1 auto && distancia auto1 > distancia auto = posicion +1
    | otherwise = posicion
cuantosTieneAdelante (auto1:siguientes) auto posicion
    | verificarSiSonDistintos auto1 auto && distancia auto1 > distancia auto = cuantosTieneAdelante siguientes auto (posicion +1)
    | otherwise = cuantosTieneAdelante siguientes auto posicion


------------------------------------------------------------------------------------------------------
--------------------------------------------2a--------------------------------------------------------
------------------------------------------------------------------------------------------------------

correr :: Auto -> Number -> Auto
correr auto = asignarNuevaDistancia auto . calcularNuevaDistancia auto

calcularNuevaDistancia :: Auto -> Number -> Number
calcularNuevaDistancia auto tiempo = distancia auto + (tiempo * velocidad auto)

asignarNuevaDistancia :: Auto -> Number -> Auto
asignarNuevaDistancia auto distanciaNueva = auto {distancia = distanciaNueva}

-- 2bi

modificarVelocidad :: Auto -> Auto
modificarVelocidad auto = auto {velocidad = nuevaVelocidad (velocidad auto)} 

nuevaVelocidad :: Number -> Number
nuevaVelocidad velocidad = velocidad + 1