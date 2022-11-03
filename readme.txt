--practica 4

Estructuras Discretas 2023-1
Profesora: Dra. Lourdes del Carmen González Huesca

Integrantes del equipo:

Jacome Delgado Alejandro
Jimenez Sanchez Emma Alicia


{-# LANGUAGE InstanceSigs #-} 
esto es nesesario para poder hacer las instancias Show.

dentro de las funciones, se encuentran principalmente 3 tipos de casos.

=> los casos exepcion
- estos casos tienen la fucnion principal de hacer que el programa no falle en escenarios donde no exista respuesta que el ordenador pueda Dar 

=> los casos base y recursivos o de trabajo
- estos casos son los casos que llevan acabo las fucniones que debe realizar la funcion que se esta ejecutando

=> los casos de omicion
- estos casos solo estan para que la funcion pueda hacer su trabajo, puesto que en estos solo se aplica la recursividad a una LProp,
no se le transforma o modifica de alguna forma.




-- la data de tipo LProp se define aqui, ademas, se especifica que "Nombre" no es mas que un "alias" que se usa para llamar a 
los Strings que se introducen a traves de la terminal y que conforman la base de las proposiciones logicas.

data LProp = PTrue | PFalse | Var Nombre | Neg LProp |Conj LProp LProp | Disy LProp LProp | Impl LProp LProp |Syss LProp LProp
type Nombre = String 

-- en esta linea se define que el type llamado Asignacion esta formado de una lista que contiene pares, dentro de estos habra un Nombre
y un Int 
type Asignacion = [(Nombre, Int)]



En esta clase de instancia se indica como se debe de mostrar los resultados de los datos.

-- la fucnion Show  va a recibir LProp y regresara un String, esta funcion se llamara implicitamente cada ves que se vaya a imprimir una
LProp en la terminal

instance Show LProp where
    show :: LProp -> String
    show(Var a)=show a
    show PTrue=show True
    show PFalse=show False
    show (Neg a)= "¬"++ show a
    show (Conj a b)= "("++ show a ++" ^ "++show b++")"
    show (Disy a b)=  "("++show a ++" v "++show b++")"
    show (Impl a b)=  "("++show a ++" -> "++show b++")"
    show (Syss a b)=  "("++show a ++" <-> "++show b++")"

--Funcion vars, esta funcion recibe una LProp y regresa una lista de Strings
vars :: LProp -> [String]

-primero se le dice que si se llaman a las constantes PTrue o PFalse, regrese una lista vacia, puesto que no hay variables que se usen
vars PTrue  = []
vars PFalse  = []
-Si solamente tiene una variable devuelve esa variable
vars (Var a) = [a] 
-si esta la Neg, se evalua dentro de ella, por si adentro de esta se encuentran otras proposiciones
vars (Neg a) = vars a 
- se evalua recursivamente dentro de cada rango de las operaciones y se van pegando la variables encontradas en forma de lista
esa lista es evaluada por la funcion aux1 
vars (Conj a b) = aux1 (vars a ++ vars b)
vars (Impl a b) = aux1 (vars a ++ vars b)
vars (Syss a b) = aux1 (vars a ++ vars b)
-- Función que filtra los elementos repetidos de las listas
aux1 :: (Eq a) => [a] -> [a] 
aux1 [] = []
aux1 (x:xs) = x : aux1 (filter (/= x) xs)



--Función asocia_der que recibe una LProp y aplica la ley de la asociatividad hacia la derecha sobre los elementos de la expresión.
En la firma entra una proposición lógica y nos regresa una proposición lógica, la firma es igual en todas las funciones en esta práctica, excepto por la interpretación, vars y num_conectores.
La asociación a la derecha solamente se debe de hacer en proposiciones lógicas donde haya disyunción y conjunción, así que la recursividad se aplica en los demás conectores que tenemos. Esto aplica lo mismo en la asocia_izq, ya que se utiliza el mismo algoritmo.

asocia_der :: LProp -> LProp

- estas son los caso de omicion de la funcion, ya que solo estan para evitar que el programa falle si les aplica la funcion 
asocia_der PFalse=PFalse
asocia_der PTrue=PTrue

- en estos casos es donde la funcion trabaja, lo que hace es tomar la conjuncion y rescribirla, de tal forma que a la hora de 
aplicar el show se ve diferente de la introducida originalmente
asocia_der (Conj (Conj (a) (b)) (c)) = (Conj (a)(Conj (b) (c)))
asocia_der (Conj (a) (Conj (b) (c))) = (Conj (a)(Conj (b) (c)))
asocia_der (Disy (Disy (a) (b)) (c)) = (Disy (a)(Disy (b) (c)))
asocia_der (Disy (a) (Disy (b) (c))) = (Disy (a)(Disy (b) (c)))

- estos son los casos de omicion, ya que no se les modifica nada en esta funcion
asocia_der (Neg a)=Neg(asocia_der a)
asocia_der (Impl a b)= Impl(asocia_der a)(asocia_der b)
asocia_der (Syss a b) = Syss(asocia_der a)(asocia_der b)

-- la funcion asocia_izq hace lo mismo que asocia_der, solo hace que los parentesis tiendan a la izquierda
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


-- la función conm que recibe una LPropr y aplica la ley de la conmutatividad de forma exhaustiva sobre los elementos de la expresión 
cuyo operador lógico sea conjunción o disyunción.
conm :: LProp -> LProp

-los casos de exepcion son estos
conm PFalse=PFalse
conm PTrue=PTrue

-los casos de trabajo son estos, puesto que la funcion solo se aplica a la distributividad y a la conmutatividad
conm (Conj (a)(b))=(Conj(b)(a))
conm (Disy (b)(a))=(Disy(a)(b))

- estos son casos de omicion, puesto que la funcion no se aplica sobre estas LProp
conm (Neg a)=Neg(conm a)
conm (Impl a b)=Impl(conm a)(conm b)
conm (Syss a b)=Syss(conm a)(conm b)


--la función dist que recibe una LProp y aplica la ley de distributividad de forma exhaustiva sobre toda la expresión. de tal forma que
regresa otra LProp, esta solo se aplica a la distributividad y a la conmutatividad
dist :: LProp -> LProp

-los casos de exepcion son estos
dist PFalse = PFalse
dist PTrue = PTrue

-los casos de omicion son estos:
dist (Neg a)= Neg(dist a)
dist (Conj a b)= Conj(dist a)(dist b)
dist (Disy a b)= Disy(dist a)(dist b)  
dist (Impl a b)= Impl(dist a)(dist b)
dist (Syss a b)= Syss(dist a)(dist b)

- el caso base es este, puesto que la LProp mas basica es la Var a
dist (Var a) = (Var a)

-los caso de trabajo son estos, puesto que son donde se aplica la distributividad
dist (Conj (a) (Disy (b) (c))) = (Disy (Conj (dist(a)) (dist(b))) (Conj (dist(a)) (dist (c))))
dist (Disy (a) (Conj (b) (c))) = (Conj (Disy (dist(a)) (dist(b))) (Disy (dist(a)) (dist (c))))


--la funcion deMorgan que le aplica a una LProp las leyes de De morgan.
En la firma de la funcipon deMorgan se tiene que se ingresa una proposicón lógica y nos devuelve otra proposición lógica.
Aquí nada más se aplica cuando tenemos negado a la disyunción y conjunción, así que estos serán nuestros casos bases a aplicar, ya que en la implicación, negación y la doble implicación no se utiliza la deMorgan, estos serán nuestros casos recursivos.
deMorgan :: LProp -> LProp

--los casos de trabajo son estos, ya que estos son los casos en donde se aplica De morgan.
deMorgan (Neg (Conj (a) (b))) = (Disy (Neg(a)) (Neg(b)))
deMorgan (Neg (Disy (a) (b))) = (Conj (Neg(a)) (Neg(b)))
-- estos son los casos de omicion
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

- estos son los casos de exepcion
equiv_op PFalse = PFalse
equiv_op PTrue = PTrue

-este es el caso base, puesto que la LProp minima es la Var a
equiv_op (Var a) = (Var a)

-estos son los casos de omicion, ya que la funcion no se aplica en estas LProp
equiv_op (Neg a)= Neg(equiv_op (a))
equiv_op (Conj a b) = Conj (equiv_op (a)) (equiv_op (b))
equiv_op (Disy a b) = Disy (equiv_op (a)) (equiv_op (b))

-estos son los casos de trabajo, ya que esta fucnion elimina las implicaciones y los Syss 
equiv_op (Impl a b) = (Disy (Neg (equiv_op (a))) (( equiv_op (b))))
equiv_op (Syss a b) = Conj (Disy ( equiv_op (a))(equiv_op (b))) (Conj (Neg(equiv_op (a))) (Neg (equiv_op (b))))




--la funcion dobleNeg quita las dobles negaciones de una LProp.
En esta función se ingresa una proposición lógica y nos regresa una proposición lógica.
Cuando se tiene que tenemos negado doblenegado una proposición o una variable nos debe de devolver lo mismo, pero sin las negaciones si solamente tiene una se hace una recursión para ver si no hay más negaciones y solamente se quedaría con esa. Fianlmente esto se aplica en los demás conectores en forma recursiva.
dobleNeg :: LProp -> LProp

- estos son los casos de omicion
dobleNeg PFalse = PFalse
dobleNeg PTrue = PTrue

- este es el caso base 
dobleNeg (Var a) = Var a

- el unico caso de trabajo es este, ya que la funcion solo elimina las dobles negaciones
dobleNeg (Neg (Neg a)) = dobleNeg a

- estos son los casos de omicion
dobleNeg (Neg a) = Neg (dobleNeg a)
dobleNeg (Conj a b) = Conj (dobleNeg a) (dobleNeg b)
dobleNeg (Disy a b) = Disy (dobleNeg a) (dobleNeg b)
dobleNeg (Syss a b) = Syss (dobleNeg a) (dobleNeg b)
dobleNeg (Impl a b) = Impl (dobleNeg a) (dobleNeg b)

--la funcion num_conectores que redibe una LProp y contesta con el número de conectivos lógicos en la expresión.
num_conectivos:: LProp -> Int 

- estos son los casos base, valen 0 porque en estos no existe ningun operador logico que los afecte
num_conectivos PTrue = 0
num_conectivos PFalse = 0
num_conectivos (Var a) = 0

-estos son los casos recursivos, en estos se evalua el rango de cada uno y se suma 1 del operador mayor, de esta forma se va sumando 1
por cada conector en la LProp
num_conectivos (Neg a) = 1 + num_conectivos a 
num_conectivos (Conj a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Disy a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Syss a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Impl a b) = 1 + num_conectivos a + num_conectivos b

--la funcion interpretacion va a tomar una LProp ψ y una asignación para regresar la interpretacion de ψ a partir de los valores de la asignación.
interpretacion :: LProp -> Asignacion -> Int

- en el caso base, se evalua cual Nombre dentro de la lista de Asignacion es igual al de Var, cuando se encuentra, regresa su valor s asosiado
interpretacion (Var a)  ((n, s):xs) = if (a==n)
                                        then s 
                                        else interpretacion (Var a) 

-en cada caso se evalua el valor binario de la LProp de forma recursiva
-para la Neg, se evalua lo que esta adentro de ella y despues se invierte el valor que contenga, es decir si adentro de Neg vale 1, Neg regresa 0
interpretacion (Neg a) x = if ((interpretacion (a) x) == 0) 
                        then 1
                        else 0 

- para Conj, se evalua el valor de cada rango, si cualquiera de los 2 vale 1, Conj regresa 1, en otro caso regresa 0
interpretacion (Conj a b) x = if ((interpretacion (a) x ) == 1 || (interpretacion (b) x ) == 1)
                            then 1
                            else 0  

- para Disy, se evaluan los rangos, si ambos valen 1, regresa ese valor, en otro caso regresa 0         
interpretacion (Disy a b) x = if ( (interpretacion (a) x ) == 1  &&  (interpretacion (b) x ) == 1)
                            then 1
                            else 0  

- para Impl, se evaluan los rangos, si el de la izquierda vale 1 y el de la derecha vale 0, regresa un 0, en otro caso regresa 1
interpretacion (Impl a b) x = if ((interpretacion (a) x ) == 1 && (interpretacion (b) x) == 0)
                            then 0
                            else 1 
- para Syss, se evaluan los rangos, si ambos valen lo mismo, regresa 1, en otro caso regresa 0                                                      
interpretacion (Syss a b)  x = if (((interpretacion (a) x) == 1 && (interpretacion (b) x) == 1) || ((interpretacion (a) x) == 1 && (interpretacion (b) x) == 1))
                            then 1
                            else 0 
