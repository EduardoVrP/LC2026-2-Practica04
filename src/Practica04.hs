module Practica04 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

--IMPLEMENTACION PARTE 1
--Ejercicio 1
conflict :: Estado -> Bool
conflict (_, clausulas)= algun esVacia clausulas

--Ejercicio 2
success :: Estado -> Bool
success (_, clausulas)= esVacia clausulas

--Ejercicio 3
unit :: Estado -> Estado
unit (modelo, []) = (modelo, [])
unit (modelo, (c:cs))
    | esUnitaria c =
        let l = head c
            nombre = obtenerNombre l
        in if tieneI nombre modelo
            then unit (modelo, cs)
            else unit (modelo ++ darV [l], simplificar l cs)
    | otherwise =
        let (m, cs') = unit (modelo, cs)
        in (m, c:cs')


--Ejercicio 4
elim :: Estado -> Estado
elim = undefined

--Ejercicio 5
red :: Estado -> Estado
red (interp, clausulas) = (interp, limpiarFalsas interp clausulas)

limpiarFalsas :: Interpretacion -> [Clausula] -> [Clausula]
limpiarFalsas _ [] = []
limpiarFalsas interp (c:cs) = 
    quitarFalsos interp c : limpiarFalsas interp cs

quitarFalsos :: Interpretacion -> Clausula -> Clausula
quitarFalsos _ [] = []
quitarFalsos interp (l:ls) =
    if esLiteralFalso interp l
    then quitarFalsos interp ls
    else l : quitarFalsos interp ls

esLiteralFalso :: Interpretacion -> Literal -> Bool
esLiteralFalso interp (Var px) = tieneVfalso px interp
esLiteralFalso interp (Not (Var px)) = tieneVcierto px interp
esLiteralFalso _ _ = False


--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep l (interp, clausulas) = 
    let nombre = nombreV l
    in (((nombre, True):interp, clausulas), ((nombre, False):interp, clausulas))

--IMPLEMENTACION PARTE 2


--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral = undefined

--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll = undefined

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 = undefined

-- Funciones auxiliares

algun :: (a -> Bool) -> [a] -> Bool
algun _ [] = False
algun p (x:xs) = p x || algun p xs

esVacia :: [a] -> Bool
esVacia [] = True
esVacia _ = False

esUnitaria :: Clausula -> Bool
esUnitaria [x] = True
esUnitaria xs = False

tieneI :: String -> Interpretacion -> Bool
tieneI _ [] = False
tieneI x ((y,b):ys) = if x == y
    then True
    else tieneI x ys

obtenerNombre :: Literal -> String
obtenerNombre (Var x) = x
obtenerNombre (Not (Var x)) = x

obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral _ = Var "" 

darV :: Clausula -> Interpretacion
darV [Var p] = [(p, True)]
darV [Not (Var p)] = [(p, False)]

simplificar :: Literal -> [Clausula] -> [Clausula]
simplificar l = map (filter (/= negar l)) . filter (notElem l)

negar :: Literal -> Literal
negar (Var x) = Not (Var x)
negar (Not (Var x)) = Var x