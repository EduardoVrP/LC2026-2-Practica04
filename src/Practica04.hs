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
sep (Var l) (interp, clausulas) = (((l, True):interp, clausulas),((l, False):interp, clausulas)) 
sep (Not (Var l)) (interp, clausulas) = (((l, True):interp, clausulas),((l, False):interp, clausulas)) 

--sep (Not (Var l)) (interp, clausulas) = (((l, False):interp, clausulas),((l, True):interp, clausulas)) 

--IMPLEMENTACION PARTE 2


--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral cs = 
    let lits = concatenarList cs
    in case lits of
        [] -> error "La lista de cláusulas está vacía"
        (x:xs) -> buscaMasFrecuente lits x 0

-- Función que recorre la lista comparando los conteos
buscaMasFrecuente :: [Literal] -> Literal -> Int -> Literal
buscaMasFrecuente [] mejor _ = mejor
buscaMasFrecuente (x:xs) mejor maxCount =
    let actualCount = cuenta x (x:xs)
    in if actualCount > maxCount
       then buscaMasFrecuente xs x actualCount    
       else buscaMasFrecuente xs mejor maxCount

-- Cuenta cuántas veces aparece una literal
cuenta :: Literal -> [Literal] -> Int
cuenta _ [] = 0
cuenta l (x:xs) 
    | l == x    = 1 + cuenta l xs
    | otherwise = cuenta l xs

-- Concatena las listas de literales (Clausulas)
concatenarList :: [Clausula] -> [Literal]
concatenarList [] = []
concatenarList (x:xs) = x ++ concatenarList xs


--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll c = solveDPLL ([], c)

-- Motor del algoritmo
solveDPLL :: Estado -> Interpretacion
solveDPLL estado@(interp, clausulas)
    | success estado = interp
    | conflict estado = []
    | otherwise = 
        let 
            -- Aplicamos las reglas de la Parte 1
            simplificado = red (elim (unit estado))
        in 
            if simplificado /= estado
            then solveDPLL simplificado
            else 
                -- Splitting usando la heurística
                let 
                    l = heuristicsLiteral (miSnd simplificado)
                    (e1, e2) = sep l simplificado
                    res1 = solveDPLL e1
                in 
                    if nega (null res1) then res1 else solveDPLL e2

-- Extrae el segundo componente de una tupla de dos elementos
miSnd :: (a, b) -> b
miSnd (x, y) = y

nega :: Bool -> Bool
nega True = False
nega False = True

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 p = dpll (propAClausulas p)

-- Convierte la Prop (en FNC) a la estructura [Clausula]
propAClausulas :: Prop -> [Clausula]
propAClausulas p = case fnc p of
    And a b -> propAClausulas a ++ propAClausulas b
    Or a b  -> [clausulaALista (Or a b)]
    Var s   -> [[Var s]]
    Not v   -> [[Not v]]
    Cons b  -> if b then [] else [[]]  -- La lista vacia es de exito, de otro casono lo es
    where 
        clausulaALista (Or a b) = clausulaALista a ++ clausulaALista b
        clausulaALista x = [x]

{-
FORMAS NORMALES
-}

-- forma normal negativa
fnn :: Prop -> Prop
fnn (Cons True) = Cons True
fnn (Cons False) = Cons False
fnn (Var a) = Var a
fnn (Not a) = negar (fnn a)
fnn (Impl a b) = Or (fnn (Not a)) (fnn b)
fnn (Syss a b) = And (fnn (Impl a b)) (fnn (Impl b a))
fnn (Or a b) = Or (fnn a) (fnn b)
fnn (And a b) = And (fnn a) (fnn b)

-- Aplica la negación a una prop
negar :: Prop -> Prop
negar (Cons True) = Cons False
negar (Cons False) = Cons True
negar (Var a) = Not (Var a)
negar (Not a) = a
negar (Or a b) = And (negar a) (negar b)
negar (And a b) = Or (negar a) (negar b)
negar (Impl a b) = And a (negar b)
negar (Syss a b) = negar (And (Impl a b) (Impl b a))

-- Form normal conjuntiva
fnc :: Prop -> Prop
fnc prop = fncAux (fnn prop)

-- Hace todo lo que haria fnc
fncAux :: Prop -> Prop
fncAux (Cons True) = Cons True
fncAux (Cons False) = Cons False
fncAux (Var a) = Var a
fncAux (Not a) = Not a
fncAux (And a b) = And (fncAux a) (fncAux b)
fncAux (Or a b) = dist (fncAux a) (fncAux b)

-- Distribuye propiamente los OR
dist :: Prop -> Prop -> Prop
dist (And a b) c = And (dist a c) (dist b c)
dist a (And b c) = And (dist a b) (dist a c)
dist a b = Or a b

