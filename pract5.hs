{-# LANGUAGE InstanceSigs #-}

data LProp = PTrue | PFalse | Var Nombre | Neg LProp |Conj LProp LProp | Disy LProp LProp | Impl LProp LProp |Syss LProp LProp
type Nombre = String -- Nombre es un sinonimo para String.
type Asignacion = [(Nombre, Int)]

--show Crea la instancia de la clase show para LProp utilizando los símbolos adecuados.
instance Show LProp where
    show :: LProp -> String
    show(Var a)=show a
    show PTrue=show True
    show PFalse=show False
    show (Neg a)= "¬"++ show a
    show (Conj a b)=  show a ++ " ∧ " ++ show b
    show (Disy a b)=  show a ++ " ∨ " ++ show b
    show (Impl a b)=  show a ++ " ⇒ " ++ show b
    show (Syss a b)=  show a ++ " ⇔ " ++ show b

--tocino
--vars 
--Función que recibe una LProp y regresa la lista con todas las variables que aparecen en la expresión.
vars :: LProp -> [String]
vars PTrue  = []
vars PFalse  = []
vars (Var a) = [a] 
vars (Neg a) = vars a 
vars (Conj a b) = aux1 (vars a ++ vars b)
vars (Impl a b) = aux1 (vars a ++ vars b)
vars (Syss a b) = aux1 (vars a ++ vars b)
-- Función que filtra los elementos repetidos de las listas
aux1 :: (Eq a) => [a] -> [a] 
aux1 [] = []
aux1 (x:xs) = x : aux1 (filter (/= x) xs)


--asocia_der 
--Función que recibe una LProp y aplica la ley de la asociatividad hacia la derecha sobre los elementos de la expresión.
asocia_der :: LProp -> LProp
asocia_der PFalse=PFalse
asocia_der PTrue=PTrue
asocia_der (Neg a)=Neg(asocia_der a)
asocia_der (Impl a b)= Impl(asocia_der a)(asocia_der b)
asocia_der (Syss a b) = Syss(asocia_der a)(asocia_der b)
asocia_der (Conj (Conj (a) (b)) (c)) = (Conj (a)(Conj (b) (c)))
asocia_der (Conj (a) (Conj (b) (c))) = (Conj (a)(Conj (b) (c)))
asocia_der (Disy (Disy (a) (b)) (c)) = (Disy (a)(Disy (b) (c)))
asocia_der (Disy (a) (Disy (b) (c))) = (Disy (a)(Disy (b) (c)))

--asocia_izq 
--Lo mismo que asocia_der pero para el otro lado.
asocia_izq :: LProp -> LProp
asocia_izq PFalse=PFalse
asocia_izq PTrue=PTrue
asocia_izq (Syss PTrue PTrue) = (Syss PTrue PTrue)
asocia_izq (Conj (Conj (a) (b)) (c)) = (Conj (Conj (a) (b)) (c))
asocia_izq (Conj (a) (Conj (b) (c))) = (Conj (Conj (a) (b)) (c))
asocia_izq (Disy (Disy (p) (q)) (r)) = (Disy (Disy (p) (q)) (r))
asocia_izq (Disy (p) (Disy (q) (r))) = (Disy (Disy (p) (q)) (r))
asocia_izq (Neg a)=Neg (asocia_izq a)
asocia_izq(Impl a b)= Impl (asocia_izq a)(asocia_izq b)
asocia_izq (Syss a b) = Syss(asocia_izq a)(asocia_izq b)

--conm 
--Función que recibe una LPropr y aplica la ley de la conmutatividad de forma exhaustiva sobre los elementos de la expresión 
--cuyo operador lógico sea conjunción o disyunción.
conm :: LProp -> LProp
conm (Conj (a)(b))=(Conj(b)(a))
conm (Disy (b)(a))=(Disy(a)(b))
conm PFalse=PFalse
conm PTrue=PTrue
conm (Neg a)=Neg(conm a)
conm (Impl a b)=Impl(conm a)(conm b)
conm (Syss a b)=Syss(conm a)(conm b)

--dist 
--Función que recibe una LProp y aplica la ley de distributividad de forma exhaustiva sobre toda la expresión.
dist :: LProp -> LProp
dist PFalse = PFalse
dist PTrue = PTrue
dist (Var a) = (Var a)
dist (Neg a)= Neg(dist a)
dist (Conj (a) (Disy (b) (c))) = (Disy (Conj (dist(a)) (dist(b))) (Conj (dist(a)) (dist (c))))
dist (Disy (a) (Conj (b) (c))) = (Conj (Disy (dist(a)) (dist(b))) (Disy (dist(a)) (dist (c))))
dist (Conj a b)= Conj(dist a)(dist b)
dist (Disy a b)= Disy(dist a)(dist b)  
dist (Impl a b)= Impl(dist a)(dist b)
dist (Syss a b)= Syss(dist a)(dist b)

--deMorgan 
--Función que le aplica a una LProp las leyes de De morgan.
deMorgan :: LProp -> LProp
deMorgan (Neg (Conj (a) (b))) = (Disy (Neg(a)) (Neg(b)))
deMorgan (Neg (Disy (a) (b))) = (Conj (Neg(a)) (Neg(b)))
deMorgan (Impl (a)(b))= Impl (a)(b)
deMorgan (Syss (a)(b))= Syss (a)(b)
deMorgan (Neg (Neg (o))) = (Neg (Neg (o)))
deMorgan (Neg (a)) = (Neg (deMorgan a))
deMorgan (Conj (a)(b))= Conj (deMorgan a)(deMorgan b)
deMorgan (Disy (a)(b))= Disy (deMorgan a)(deMorgan b)
deMorgan (Impl (a)(b))= Impl (deMorgan a)(deMorgan b)
deMorgan (Syss (a)(b))= Syss (deMorgan a)(deMorgan b)

--equiv_op 
--Función que recibe una LProp y aplica la equivalencia de operadores que se describe al inicio de este documento.
equiv_op :: LProp -> LProp
equiv_op PFalse = PFalse
equiv_op PTrue = PTrue
equiv_op (Var a) = (Var a)
equiv_op (Neg a)= Neg(equiv_op (a))
equiv_op (Impl a b) = (Disy (Neg ( (a))) ( (b)) )
--equiv_op (Syss a b) = 
--equiv_op (Conj a b) = Conj ((a) ((b)))
--equiv_op (Disy a b) = Disy (( (a)) ( (b)))

--dobleNeg 
--Función que quita las dobles negaciones de una LProp.
dobleNeg :: LProp -> LProp
dobleNeg PFalse = PFalse
dobleNeg PTrue = PTrue
dobleNeg (Var a)=Var a
dobleNeg (Neg (Var a))= Neg (Var a)
dobleNeg (Neg(Neg(Var a)))= Var a
dobleNeg (Neg(Neg(Conj (Var a)(Var b))))= Conj(dobleNeg (Var a))(dobleNeg (Var b))
{-dobleNeg (Conj (Var a)(Var b))=Conj(dobleNeg (Var a))(dobleNeg (Var b))
dobleNeg (Disy (Var a)(Var b))=Disy(dobleNeg (Var a))(dobleNeg (Var b))
dobleNeg (Impl (Var a)(Var b))=Impl(dobleNeg (Var a))(dobleNeg (Var b))
dobleNeg (Syss (Var a)(Var b))=Syss(dobleNeg (Var a))(dobleNeg (Var b))-}

--num_conectivos 
--Función que redibe una LProp y contesta con el número de conectivos lógicos en la expresión.
{-num_conectivos (Var "ok") = 0
num_conectivos (Disy (Var "u") (Disy (Var "w") (Var "a"))) = 2
num_conectivos (Syss (Var "n") (Neg (Conj PTrue (Var "m")))) = 3
num_conectivos (Syss (Impl (Var "e") (Var "k")) ...
(Impl (Var "j") (Conj (Var "a") (Var "l")))) = 4
num_conectivos (Neg (Neg (Neg (Neg (Neg (Var "s")))))) = 5-}

--interpretacion 
--Esta función va a tomar una LProp ψ y una asignación para regresar la interpretacion de ψ a partir de los valores de la asignación.
interpretacion :: (Eq a, Num a, Num p) => LProp -> [([Char], a)] -> p
interpretacion (Impl (Impl (Var "p") (Var "q")) (Var "r")) [("p" ,0) ,("q" ,0) ,("r" ,0) ] = 0
interpretacion (Impl (Impl (Var "p") (Var "q")) (Var "r")) [("p" ,1) ,("q" ,0) ,("r" ,0) ] = 1
interpretacion (Disy (Impl (Var "p") (Var "q")) (Impl (Var "q") (Var "p"))) [("p" ,1) ,("q" ,0) ] = 1
interpretacion (Syss (Var "x") (Var "y")) [("x" ,0),("y" ,1) ] = 0
interpretacion (Impl (Conj (Var "s") (Var "t")) (Var "r")) [("s" ,0) ,("t" ,1) ,("r" ,0) ] = 1
