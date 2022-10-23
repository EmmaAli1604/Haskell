p where
    show :: LProp -> String
    show(Var a)=show a
    show PTrue=show True
    show PFalse=show False
    show (Neg a)= "¬"++ show a
    show (Conj a b)=  show a ++" ^ "++show b
    show (Disy a b)=  show a ++" v "++show b
    show (Impl a b)=  show a ++" -> "++show b
    show (Syss a b)=  show a ++" <-> "++show b


--vars 
--Función que recibe una LProp y regresa la lista con todas las variables que aparecen en la expresión.
vars :: LProp -> [String]
vars (Impl PTrue PFalse) = []
vars (Var a)=["a"]
vars (Syss (Impl (Var "a") (Var "b")) (Impl (Var "c") (Conj (Var "b") (Var "c"))))= ["a","b","c"]

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

{-
--asocia_izq 
--Lo mismo que asocia_der pero para el otro lado.
asocia_izq (Conj (Conj (Var "a") (Var "b")) (Var "c")) = ((a ^ b) ^ c)
asocia_izq (Conj (Var "a") (Conj (Var "b") (Var "c"))) = ((a ^ b) ^ c)
asocia_izq (Disy (Disy (Var "p") (Var "q")) (Var "r")) = ((p v q) v r)
asocia_izq (Disy (Var "p") (Disy (Var "q") (Var "r"))) = ((p v q) v r)
asocia_izq (Syss PTrue PTrue) = (True <-> True)-}

--conm 
--Función que recibe una LPropr y aplica la ley de la conmutatividad de forma exhaustiva sobre los elementos de la expresión 
--cuyo operador lógico sea conjunción o disyunción.
conm :: LProp -> LProp
conm (Conj (a)(b))=(Conj(b)(a))
conm (Disy (b)(a))=(Disy(a)(b))

{--dist 
--Función que recibe una LProp y aplica la ley de distributividad de forma exhaustiva sobre toda la expresión.

dist (Conj (Var "d") (Disy (Var "e") (Var "f"))) = ((d ^ e) v (d ^ f))
dist (Disy (Var "s") (Conj (Var "t") (Var "u"))) = ((s v t) ^ (s v u))
dist (Conj (Var "a") (Impl (Var "b") (Var "a"))) = ((b -> a) ^ a)
dist (Var "r") = r
dist (Disy (Var "i") PFalse) = (False v i)
dist PTrue = True-}

--deMorgan 
--Función que le aplica a una LProp las leyes de De morgan.
deMorgan (Conj (a)(b))=Disy(Neg(a) Neg(b))
--deMorgan (Disy (a)(b))=Conj(Neg(a) Neg(b))
--deMorgan (Neg (Neg (o))) = (Neg (Neg (o)))

