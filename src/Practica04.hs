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
conflict (_, (x:xs)) = elemento [] (x:xs) 

-- Funcion que verifica si un elemento pertenece a una lista.
elemento :: Eq a => a -> [a] -> Bool
elemento _ [] = False
elemento x (y:ys) = x == y || elemento x ys

--Ejercicio 2
success :: Estado -> Bool
success (_, list) = list == []

--Ejercicio 3
unit :: Estado -> Estado
unit (interp, clausulas) = buscaLiteral interp clausulas

-- Verifica si en la interpretación no existe el complemento de la literal
esValida :: Literal -> Interpretacion -> Bool
esValida _ [] = True
esValida x (y:ys)  
    | esComplemento x y = False
    | otherwise = esValida x ys

-- Verifica si la tupla es complemento de la literal.
esComplemento :: Literal -> (String, Bool) -> Bool
esComplemento (Var l) (s, b) = (l == s) && (b == False)
esComplemento (Not (Var l)) (s,b) = (l == s) && (b == True)

-- Añade la literal a la interpretación. Se supone que no existe complemento en la interpretación.
anadir :: Literal -> Interpretacion -> Interpretacion
anadir (Var l) interp = (l, True):interp
anadir (Not (Var l)) interp = (l, False):interp

-- Busca alguna literal en la lista de cláusulas. Si encuentra, verifica si es valida y de serlo la añade a la interpretación y la elimina de la lista de clausulas.
-- En caso de no se así, busca de nuevo.
buscaLiteral :: Interpretacion -> [Clausula] -> Estado
buscaLiteral interp [] = (interp, [])
buscaLiteral interp ((y:ys):xs)
    | (ys == []) && (esValida y interp) = ((anadir y interp), xs)
    | otherwise = unionTupla ([], [y:ys]) (buscaLiteral interp xs)

-- Une los elementos de dos tuplas de listas a una, las listas se unen como conjunto.
unionTupla :: (Eq a, Eq b) => ([a],[b]) -> ([a],[b]) -> ([a],[b])
unionTupla (w,x) (y,z) = ((union w y),(union x z))

-- Une dos listas en una, como conjunto.
union :: Eq a => [a] -> [a] -> [a]
union ys [] = ys
union ys (x:xs)
    | elemento x ys = union ys xs
    | otherwise      = union (ys ++ [x]) xs

--Ejercicio 4
elim :: Estado -> Estado
elim (interp, clausulas) = (interp, eliminacion interp clausulas)

-- Aplica la eliminacion a las clasulas por cada elemento en interpretación.
eliminacion :: Interpretacion -> [Clausula] -> [Clausula]
eliminacion [] clausulas = clausulas
eliminacion ((s,b):xs) clausulas =
    let l = if b then Var s else Not (Var s)
    in eliminacion xs (subElim l clausulas)

-- Dada una literal, elimina la clausula que la contenga en toda la lista de clausulas.
subElim :: Literal -> [Clausula] -> [Clausula]
subElim _ [] = []
subElim l (x:xs) 
    | elemento l x = subElim l xs
    | otherwise = x:(subElim l xs)


--Ejercicio 5
red :: Estado -> Estado
red (interp, clausulas) = (interp, reduccion interp clausulas)

-- Aplica la reducción a las clausulas para cada elemento en interpretación.
reduccion :: Interpretacion -> [Clausula] -> [Clausula]
reduccion [] clausulas = clausulas
reduccion ((s,b):xs) clausulas =
    let l = if b then Not (Var s) else Var s
    in reduccion xs (subRed l clausulas)

-- Dada una literal, quita la literal de clasula sobre todas las clausulas.
subRed :: Literal -> [Clausula] -> [Clausula]
subRed _ [] = []
subRed l (x:xs) = (quitaLiteral l x):(subRed l xs)

-- Dada una literal, quita la literal de la clausula.
quitaLiteral :: Literal -> Clausula -> Clausula
quitaLiteral _ [] = []
quitaLiteral l (x:xs)
    | l == x = quitaLiteral l xs
    | otherwise = x:(quitaLiteral l xs)

--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep = undefined

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