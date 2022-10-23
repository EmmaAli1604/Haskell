data LProp = PTrue | PFalse | Var Nombre | Neg LProp |Conj Lprop LProp | Disy LProp LProp | Impl LProp LProp |Syss LProp LProp
type Nombre = String -- Nombre es un sinonimo para String.
type Asignacion = [(Nombre, Int)]

show Crea la instancia de la clase show para LProp utilizando los símbolos adecuados.
vars Función que recibe una LProp y regresa la lista con todas las variables que aparecen en la expresión.
vars (Syss (Impl (Var "a") (Var "b")) (Impl (Var "c") (Conj (Var "b") (Var "c")))) ...
= ["a","b","c"]
vars (Impl PTrue PFalse) = []

asocia_der Función que recibe una LProp y aplica la ley de la asociatividad hacia la derecha sobre los elementos de

la expresión.
asocia_der (Conj (Conj (Var "a") (Var "b")) (Var "c")) = (a ^ (b ^ c))
asocia_der (Conj (Var "a") (Conj (Var "b") (Var "c"))) = (a ^ (b ^ c))
asocia_der (Disy (Disy (Var "p") (Var "q")) (Var "r")) = (p v (q v r))
asocia_der (Disy (Var "p") (Disy (Var "q") (Var "r"))) = (p v (q v r))
asocia_der (Syss PFalse PFalse) = (False <-> False)