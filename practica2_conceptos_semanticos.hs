module Practica02 where

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
variables (And p q) = variables p ++ variables q
variables (Or p q) = variables p ++ variables q
variables (Impl p q) = variables p ++ variables q
variables (Syss p q) = variables p ++ variables q

-- Ejercicio 2
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia(x:xs) = [x:ys | ys <- conjPotencia xs] ++ conjPotencia xs

-- Ejercicio 3
interpretacion :: Prop -> Estado -> Bool
interpretacion = undefined

-- Ejercicio 4
estadosPosibles :: Prop -> [Estado]
estadosPosibles = undefined

-- Ejercicio 5
modelos :: Prop -> [Estado]
modelos = undefined

-- Ejercicio 6
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes = undefined

-- Ejercicio 7
tautologia :: Prop -> Bool
tautologia = undefined

-- Ejercicio 8
contradiccion :: Prop -> Bool
contradiccion = undefined

-- Ejercicio 9
contingencia :: Prop -> Bool 
contingencia = undefined

-- Ejercicio 10
esModelo :: Estado -> Prop -> Bool
esModelo = undefined

-- Ejercicio 11
esSatisfacible :: Prop -> Bool
esSatisfacible = undefined