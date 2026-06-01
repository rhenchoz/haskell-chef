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

data Participante = Participante { 
    nombre :: String,
    trucos :: [Truco],
    platoEspecialidad :: Plato
} deriving (Show)


-- Funciones que trabajan con platos y componentes

crearComponente :: String -> Number -> Componente 
crearComponente nombreIngrediente cantidad = (nombreIngrediente,cantidad)

agregarComponente :: Plato -> Componente -> Plato
agregarComponente unPlato unComponente = unPlato{componentes = unComponente : componentes unPlato} 

obtenerComponente :: Plato -> String -> Componente
obtenerComponente unPlato nombreIngrediente = head (filter (\(ingrediente,peso) -> ingrediente == nombreIngrediente) (componentes unPlato))

cantidadComponentes :: Plato -> Number
cantidadComponentes unPlato = length (componentes unPlato) 

platoContiene :: Plato -> String -> Bool
platoContiene unPlato unIngrediente =  any (\(ingrediente,peso) -> ingrediente == unIngrediente) (componentes unPlato) 

esComplejo :: Plato -> Bool 
esComplejo unPlato = dificultad unPlato > 7 && cantidadComponentes unPlato > 5 

sacarComponente :: Plato -> String -> [Componente] --saca componente de una lista
sacarComponente unPlato nombreIngrediente =  filter(\(ingrediente,peso) -> ingrediente /= nombreIngrediente) (componentes unPlato)

agregarPesoComponente :: Componente -> Number -> Componente
agregarPesoComponente (ingrediente,peso) pesoAgregado = (ingrediente,peso+pesoAgregado) 

--Esta funcion modifica el peso de un solo componente, que esta dentro de una lista de componentes
modificarPesoLista :: Plato -> String -> Number -> Plato
modificarPesoLista unPlato unIngrediente cantidadAgregada = unPlato{componentes = (agregarPesoComponente (obtenerComponente unPlato unIngrediente) cantidadAgregada) : (sacarComponente unPlato unIngrediente)} 

obtenerPeso :: Componente -> Number 
obtenerPeso (_, peso) = peso  


-- Trucos mas famosos



endulzar :: Number -> Plato -> Plato
endulzar cantidadAzucar unPlato 
    |platoContiene unPlato "Azucar" = modificarPesoLista unPlato "Azucar" cantidadAzucar
    |otherwise = agregarComponente unPlato (crearComponente "Azucar" cantidadAzucar)

salar :: Number -> Plato -> Plato
salar cantidadSal unPlato 
    |platoContiene unPlato "Sal" = modificarPesoLista unPlato "Sal" cantidadSal
    |otherwise = agregarComponente unPlato (crearComponente "Sal" cantidadSal)

darSabor :: Number -> Number -> Plato -> Plato
darSabor cantAzucar cantSal unPlato = salar cantSal (endulzar cantAzucar unPlato)

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato{componentes = map (\(ingrediente, peso) -> (ingrediente, peso*2)) (componentes unPlato)}

simplificar :: Plato -> Plato
simplificar unPlato 
    |esComplejo unPlato = unPlato{dificultad = 5, componentes= filter (\(ingrediente,peso) -> peso>10) (componentes unPlato)}
    |otherwise = unPlato
 
-- Funciones utiles para platos

esVegano :: Plato -> Bool
esVegano unPlato = not (platoContiene unPlato "Carne" || platoContiene unPlato "Huevos" || platoContiene unPlato "Leche")

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (platoContiene unPlato "Harina")

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = platoContiene unPlato "Sal" && obtenerPeso (obtenerComponente unPlato "Sal") > 2


-- Parte B 
-- Componentes y plato de pepe
papa :: Componente
papa = crearComponente "Papa" 500

harina :: Componente
harina = crearComponente "Harina" 250 

leche :: Componente
leche = crearComponente "Leche" 100

pimienta :: Componente
pimienta = crearComponente "Pimienta" 4

cebolla :: Componente
cebolla = crearComponente "Cebolla" 250

sal :: Componente
sal = crearComponente "Sal" 3

platoDePepe = Plato 8 [papa, harina, leche, pimienta, cebolla, sal]


-- Modelar a Pepe
pepeDaSabor unPlato = darSabor 2 5 unPlato
pepeSimplifica unPlato = simplificar unPlato
pepeDuplica unPlato = duplicarPorcion unPlato

trucosDePepe :: [Truco] 
trucosDePepe = [pepeDaSabor, pepeSimplifica, pepeDuplica]
pepe = Participante "Pepe" trucosDePepe platoDePepe 



-- Parte C
cocinar :: Participante -> Plato
cocinar unParticipante = foldr ($) (platoEspecialidad unParticipante) (trucos unParticipante) 

listaDePesos :: [Componente] -> [Number]
listaDePesos listaComponente = map (obtenerPeso) (listaComponente)

esMejorQue :: Plato -> Plato -> Plato
esMejorQue unPlato otroPlato
    |dificultad unPlato > dificultad otroPlato && (foldl1 (+) (listaDePesos (componentes unPlato))) < (foldl1 (+) (listaDePesos(componentes otroPlato))) = unPlato
    | dificultad unPlato < dificultad otroPlato && (foldl1 (+) (listaDePesos (componentes unPlato))) > (foldl1 (+) (listaDePesos(componentes otroPlato))) = otroPlato
    |otherwise = unPlato

obtenerParticipante :: [Participante] -> Plato -> Participante
obtenerParticipante participantes unPlato = head(filter (\participante -> cocinar participante == unPlato) (participantes))

mejorPlato :: [Participante] -> Plato
mejorPlato participantes = foldl1 (esMejorQue) (map (cocinar) participantes)

participanteEstrella :: [Participante] -> Participante
participanteEstrella participantes = obtenerParticipante participantes (mejorPlato participantes) 


--Platos y participantes de prueba
platoDeLuz :: Plato
luz :: Participante
platoDeLuz = Plato 9 [("Trufa", 5), ("Azafran", 3), ("Aceite", 2)]
luz = Participante "Luz" [] platoDeLuz

platoDeJuan::Plato
juan :: Participante
platoDeJuan = Plato 2 [("Papa", 5000), ("Agua", 3000)]
juan = Participante "Juan" [] platoDeJuan
