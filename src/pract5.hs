--practica 4

{-Jacome Delgado Alejandro
Jiménez Sánchez Emma Alicia

Esta mejor explicado en el readme .
-}

--esto importa lo nesesario para hacer la instancia del Show 
{-# LANGUAGE InstanceSigs #-}

data LProp = PTrue | PFalse | Var Nombre | Neg LProp |Conj LProp LProp | Disy LProp LProp | Impl LProp LProp |Syss LProp LProp -- creacion de LProp
type Nombre = String -- Nombre es un sinonimo para String.
type Asignacion = [(Nombre, Int)] -- creacion de Asignacion

--show Crea la instancia de la clase show para LProp utilizando los símbolos adecuados.
instance Show LProp where
    show :: LProp -> String 
    show(Var a)=show a
    show PTrue=show True
    show PFalse=show False
    show (Neg a)= "¬"++ show a
    show (Conj a b)= "("++ show a ++" ∧ "++show b++")"
    show (Disy a b)=  "("++show a ++" ∨ "++show b++")"
    show (Impl a b)=  "("++show a ++" ⇒ "++show b++")"
    show (Syss a b)=  "("++show a ++" ⇔ "++show b++")"


--vars 
--Función que recibe una LProp y regresa la lista con todas las variables que aparecen en la expresión.
vars :: LProp -> [String]
--exepciones
vars PTrue  = []
vars PFalse  = []
--caso base
vars (Var a) = [a] 
--casos recursivos 
vars (Neg a) = vars a 
vars (Conj a b) = aux1 (vars a ++ vars b)
vars (Impl a b) = aux1 (vars a ++ vars b)
vars (Syss a b) = aux1 (vars a ++ vars b)

-- Función que filtra los elementos repetidos de las listas
aux1 :: (Eq a) => [a] -> [a] 
aux1 [] = [] --exepcion
aux1 (x:xs) = x : aux1 (filter (/= x) xs)


--asocia_der 
--Función que recibe una LProp y aplica la ley de la asociatividad hacia la derecha sobre los elementos de la expresión.
asocia_der :: LProp -> LProp 
--exepciones 
asocia_der PFalse=PFalse
asocia_der PTrue=PTrue
--recursion 
asocia_der (Neg a)=Neg(asocia_der a)
asocia_der (Impl a b)= Impl(asocia_der a)(asocia_der b)
asocia_der (Syss a b) = Syss(asocia_der a)(asocia_der b)
--casos base 
asocia_der (Conj (Conj (a) (b)) (c)) = (Conj (a)(Conj (b) (c)))
asocia_der (Conj (a) (Conj (b) (c))) = (Conj (a)(Conj (b) (c)))
asocia_der (Disy (Disy (a) (b)) (c)) = (Disy (a)(Disy (b) (c)))
asocia_der (Disy (a) (Disy (b) (c))) = (Disy (a)(Disy (b) (c)))

--asocia_izq 
--Lo mismo que asocia_der pero para el otro lado.
asocia_izq :: LProp -> LProp
--exepciones 
asocia_izq PFalse=PFalse
asocia_izq PTrue=PTrue
asocia_izq (Syss PTrue PTrue) = (Syss PTrue PTrue)
--casos recursivos 
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
-- casos de trabajo
conm (Conj (a)(b))=(Conj(b)(a))
conm (Disy (b)(a))=(Disy(a)(b))
--exepciones 
conm PFalse=PFalse
conm PTrue=PTrue
-- casos recursivos de omicion
conm (Neg a)=Neg(conm a)
conm (Impl a b)=Impl(conm a)(conm b)
conm (Syss a b)=Syss(conm a)(conm b)

--dist 
--Función que recibe una LProp y aplica la ley de distributividad de forma exhaustiva sobre toda la expresión.
dist :: LProp -> LProp
--exepciones 
dist PFalse = PFalse
dist PTrue = PTrue
--caso base
dist (Var a) = (Var a)
--casos de trabajo
dist (Conj (a) (Disy (b) (c))) = (Disy (Conj (dist(a)) (dist(b))) (Conj (dist(a)) (dist (c))))
dist (Disy (a) (Conj (b) (c))) = (Conj (Disy (dist(a)) (dist(b))) (Disy (dist(a)) (dist (c))))
--casos de omicion
dist (Neg a)= Neg(dist a)
dist (Conj a b)= Conj(dist a)(dist b)
dist (Disy a b)= Disy(dist a)(dist b)  
dist (Impl a b)= Impl(dist a)(dist b)
dist (Syss a b)= Syss(dist a)(dist b)

--deMorgan 
--Función que le aplica a una LProp las leyes de De morgan.
deMorgan :: LProp -> LProp
--casos de trabajo
deMorgan (Neg (Conj (a) (b))) = (Disy (Neg(a)) (Neg(b)))
deMorgan (Neg (Disy (a) (b))) = (Conj (Neg(a)) (Neg(b)))
--casos de omicion
deMorgan (Impl (a)(b))= Impl (a)(b)
deMorgan (Syss (a)(b))= Syss (a)(b)
deMorgan (Neg (Neg (o))) = (Neg (Neg (o)))
deMorgan (Neg (a)) = (Neg (deMorgan a))
deMorgan (Conj (a)(b))= Conj (deMorgan a)(deMorgan b)
deMorgan (Disy (a)(b))= Disy (deMorgan a)(deMorgan b)


--equiv_op 
--Función que recibe una LProp y aplica la equivalencia de operadores que se describe al inicio de este documento.
equiv_op :: LProp -> LProp
--exepciones 
equiv_op PFalse = PFalse
equiv_op PTrue = PTrue
--caso base
equiv_op (Var a) = (Var a)
--caso de omicion
equiv_op (Neg a)= Neg(equiv_op (a))
equiv_op (Conj a b) = Conj (equiv_op (a)) (equiv_op (b))
equiv_op (Disy a b) = Disy (equiv_op (a)) (equiv_op (b))
--caso de trabajo 
equiv_op (Impl a b) = (Disy (Neg (equiv_op (a))) (( equiv_op (b))))
equiv_op (Syss a b) = Conj (Disy ( equiv_op (a))(equiv_op (b))) (Conj (Neg(equiv_op (a))) (Neg (equiv_op (b))))


--dobleNeg 
--Función que quita las dobles negaciones de una LProp.
dobleNeg :: LProp -> LProp
--exepciones 
dobleNeg PFalse = PFalse
dobleNeg PTrue = PTrue
--caso base 
dobleNeg (Var a) = Var a
--caso de trabajo
dobleNeg (Neg (Neg a)) = dobleNeg a
--casos de omicion
dobleNeg (Neg a) = Neg (dobleNeg a)
dobleNeg (Conj a b) = Conj (dobleNeg a) (dobleNeg b)
dobleNeg (Disy a b) = Disy (dobleNeg a) (dobleNeg b)
dobleNeg (Syss a b) = Syss (dobleNeg a) (dobleNeg b)
dobleNeg (Impl a b) = Impl (dobleNeg a) (dobleNeg b)

--num_conectivos 
--Función que redibe una LProp y contesta con el número de conectivos lógicos en la expresión.
num_conectivos:: LProp -> Int 
--casos base
num_conectivos PTrue = 0
num_conectivos PFalse = 0
num_conectivos (Var a) = 0
--casos recursivos
num_conectivos (Neg a) = 1 + num_conectivos a 
num_conectivos (Conj a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Disy a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Syss a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Impl a b) = 1 + num_conectivos a + num_conectivos b




--interpretacion 
--Esta función va a tomar una LProp ψ y una asignación para regresar la interpretacion de ψ a partir de los valores de la asignación.
--interpretacion :: (Eq a, Num a, Num p) => LProp -> [(Nombre, Int)] -> p
interpretacion :: LProp -> Asignacion -> Int
--caso base
interpretacion (Var a)  ((n, s):xs) = if (a==n)
                                        then s 
                                        else interpretacion (Var a) 
--casos recursivos
interpretacion (Neg a) x = if ((interpretacion (a) x) == 0) 
                        then 1
                        else 0 
interpretacion (Conj a b) x = if ((interpretacion (a) x ) == 1 || (interpretacion (b) x ) == 1)
                            then 1
                            else 0           
interpretacion (Disy a b) x = if ( (interpretacion (a) x ) == 1  &&  (interpretacion (b) x ) == 1)
                            then 1
                            else 0             
interpretacion (Impl a b) x = if ((interpretacion (a) x ) == 1 && (interpretacion (b) x) == 0)
                            then 0
                            else 1                                                       
interpretacion (Syss a b)  x = if (((interpretacion (a) x) == 1 && (interpretacion (b) x) == 1) || ((interpretacion (a) x) == 1 && (interpretacion (b) x) == 1))
                            then 1
                            else 0 


