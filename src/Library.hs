module Library where
import PdePreludat

-- Tipeo y data
type Truco = Plato -> Plato 
type Ingredientes = String
type Peso = Number
type Componente = (Ingredientes, Peso)

data Plato = Plato{
    dificultad :: Number,
    componentes :: [Componente]
} deriving (Eq, Show)

data Personaje = Personaje { 
    nombre :: String,
    trucos :: [Truco],
    platoEspecialidad :: Plato
} deriving (Show)

-- Trucos mas famosos

crearComponente :: String -> Number -> Componente 
crearComponente nombreIngrediente cantidad = (nombreIngrediente,cantidad)

agregarComponente :: Plato -> Componente -> Plato
agregarComponente unPlato unComponente = unPlato{componentes = unComponente : componentes unPlato}

cantidadComponentes :: Plato -> Number
cantidadComponentes unPlato = length (componentes unPlato)

esComplejo :: Plato -> Bool 
esComplejo unPlato = dificultad unPlato > 7 && cantidadComponentes unPlato > 5

endulzar :: Number -> Plato -> Plato
endulzar cantidadAzucar unPlato = agregarComponente unPlato (crearComponente "Azucar" cantidadAzucar)

salar :: Number -> Plato -> Plato
salar cantidadSal unPlato = agregarComponente unPlato (crearComponente "Sal" cantidadSal)

darSabor :: Number -> Number -> Plato -> Plato
darSabor cantAzucar cantSal unPlato = salar cantSal (endulzar cantAzucar unPlato)

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato{componentes = map (\(ingrediente, peso) -> (ingrediente, peso*2)) (componentes unPlato)}

simplificar :: Plato -> Plato
simplificar unPlato 
    |esComplejo unPlato = unPlato{dificultad = 5, componentes= filter (\(ingrediente,peso) -> peso>10) (componentes unPlato)}
    |otherwise = unPlato
 
-- Funciones utiles para platos

obtenerPeso :: Componente -> Number 
obtenerPeso (_, peso) = peso 

platoContiene :: Plato -> String -> Bool
platoContiene unPlato unIngrediente =  any (\(ingrediente,peso) -> ingrediente == unIngrediente) (componentes unPlato) 

obtenerComponente :: Plato -> String -> Componente
obtenerComponente unPlato nombreIngrediente = head (filter (\(ingrediente,peso) -> ingrediente == nombreIngrediente) (componentes unPlato))

esVegano :: Plato -> Bool
esVegano unPlato = not (platoContiene unPlato "Carne" || platoContiene unPlato "Huevos" || platoContiene unPlato "Leche")

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (platoContiene unPlato "Harina")

noAptoHipertension unPlato = platoContiene unPlato "Sal" && obtenerPeso (obtenerComponente unPlato "Sal") > 2
platoPrueba = Plato 10 [crearComponente "Leche" 3, crearComponente "Harina" 10] --SACAR
