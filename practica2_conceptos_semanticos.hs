module Practica02 where
-- Se importan las funciones nub y sort de la libreria Data.List para eliminar los elementos repetidos de una lista y ordenarla
import Data.List(nub, sort)
-- Practica 2: Conceptos Semanticos 

-- Integrantes: Luis Juárez Erick
--              Herrera Avalos Julio Alejandro
--              Peña Villegas Diego Eduardo

-- 3.1 Sintaxis de la lógica proposicional

-- Se efine el tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Se definen las fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Se implementa un estado
type Estado = [String]
--3.2 Ejericios

-- Funcion que sirve para obtener las variables en una formula proposicional

variables :: Prop -> [String]
variables (Var x) = [x]
variables (Cons _) = []
variables (Not p) = variables p
variables (And p q) = nub (variables p ++ variables q)
variables (Or p q) = nub (variables p ++ variables q)
variables (Impl p q) = nub (variables p ++ variables q)
variables (Syss p q) = nub (variables p ++ variables q)

-- Funcion que sirve para obtener el conjunto potencia de una lista
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia(x:xs) = [x:ys | ys <- conjPotencia xs] ++ conjPotencia xs

-- Funcion que sirve para obtener la interpretacion de una formula proposicional bajo un estado
interpretacion :: Prop -> Estado -> Bool
-- Para asignarle el valor de verdad a una variable verificamos que pertenezca al conjunto de estados (si pertenece es True, si no es False)
interpretacion (Var x) e = elem x e
interpretacion (Cons b) _ = b
interpretacion (Not p) e = not (interpretacion p e)
interpretacion (And p q) e = interpretacion p e && interpretacion q e
interpretacion (Or p q) e = interpretacion p e || interpretacion q e
interpretacion (Impl p q) e = not (interpretacion p e) || interpretacion q e
interpretacion (Syss p q) e = interpretacion p e == interpretacion q e

-- Funcion que sirve para obtener los estados posibles de una formula proposicional
estadosPosibles :: Prop -> [Estado]
estadosPosibles p = conjPotencia (variables p)

-- Funcion que sirve para obtener los modelos de una formula proposicional (Estados que hacen verdadera la formula)
modelos :: Prop -> [Estado]
modelos p = [e | e <- estadosPosibles p, interpretacion p e]

-- Funcion que sirve para verificar si dos formulas proposicionales son equivalentes (si tienen los mismos modelos)
sonEquivalentes :: Prop -> Prop -> Bool
-- Ordenamos los elementes de los modelos para verificar si son iguales (asi evitamos que marque falso si los estados o modelos estan en diferente orden)
sonEquivalentes p q = sort (map sort (modelos p)) == sort (map sort (modelos q))


-- Funcion que sirve para verificar si una formula proposicional es una tautologia
tautologia :: Prop -> Bool
tautologia p = and [interpretacion p e | e <- estadosPosibles p]

-- Funcion que sirve para verificar si una formula proposicional es una contradiccion
contradiccion :: Prop -> Bool
contradiccion p = and [not (interpretacion p e) | e <- estadosPosibles p]

-- Funcion que sirve para verificar si una formula proposicional es una contingencia
contingencia :: Prop -> Bool 
contingencia p = not (tautologia p) && not (contradiccion p)

-- Funcion que sirve para verificar si un estado es un modelo de una formula propoisional (si hace verdadera la formula)
esModelo :: Estado -> Prop -> Bool
esModelo e p = interpretacion p e

-- Funcion que sirve para verificar si una formula proposicional es satisfacible
esSatisfacible :: Prop -> Bool
esSatisfacible p = or [interpretacion p e | e <- estadosPosibles p]