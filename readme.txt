--practica 5

Estructuras Discretas 2023-1
Profesora: Dra. Lourdes del Carmen González Huesca

Integrantes del equipo:

Jacome Delgado Alejandro
Jimenez Sanchez Emma Alicia

--esto importa lo nesesario para hacer la instancia del Show 
{-# LANGUAGE InstanceSigs #-}

-- la data de tipo LProp se define aqui, ademas, se especifica que "Nombre" no es mas que un "alias" que se usa para llamar a los Strings que se introducen a traves de la terminal y que conforman la base de las proposiciones logicas.

data LProp = T | F | VarP Nombre | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp | Neg LProp    
data Tableaux = Hoja [LProp] | Alpha [LProp] Tableaux | Beta [LProp] Tableaux Tableaux  deriving Show 

type Nombre = String


-- la fucnion Show  va a recibir LProp y regresara un String, esta funcion se llamara 
implicitamente cada ves que se vaya a imprimir una LProp en la terminal.

instance Show LProp where
    show :: LProp -> String 
    show(VarP a)=show a
    show T=show True
    show F=show False
    show (Neg a)= "¬"++ show a
    show (Conj a b)= "("++ show a ++" ∧ "++show b++")"
    show (Disy a b)=  "("++show a ++" ∨ "++show b++")"
    show (Impl a b)=  "("++show a ++" ⇒ "++show b++")"
    show (Syss a b)=  "("++show a ++" ⇔ "++show b++")"

La función literales nos dice si en una lista de fórmulas, si todas son literales hasta llegar a una lista vacía, 
después de hacer la recursión en toda la lista,es porque en la lista de fórmulas lógicas, todas son literales. 
Pero si tenemos algo diferente de los casos donde es una literal, será falso.
literales :: [LProp] -> Bool
literales [] = True
literales ((VarP a):xs) = literales xs
literales (T : xs) = literales xs
literales (F : xs) = literales xs
literales (Neg (T) : xs) = literales xs
literales (Neg (F) : xs) = literales xs 
literales (Neg (VarP a) : xs) = literales xs
literales (_: xs) = False

La función nextF lo que hacemos es indicar que si en la cabeza hay una fórmula lógica que no sea una literal, regresé la fórmula lógica, si no encuentra en la cabeza una fórmula lógica, 
se debe de checar la cola de lista hasta encontrar una fórmula lógica que no sea una literal.
nextF :: [LProp] ->  LProp
nextF [] = error "implementar"
nextF ((Conj a b):xs)= Conj a b
nextF ((Disy a b):xs)= Disy a b
nextF ((Impl a b):xs)= Impl a b
nextF ((Syss a b):xs)= Syss a b
nextF ((Neg(VarP a)):xs)= nextF xs
nextF ((VarP a ):xs)=nextF xs

La función alpha nos indica si una fórmula es alpha, en los casos donde una fórmula alpha es verdadera es cuando tenemos una conjunción, 
una negación de una implicación y la negación de una disyunción. Otros casos que no sean los que están indicados serán falsos.
alpha :: LProp -> Bool
alpha (Conj a b)=True
alpha (Neg(Impl a b))= True
alpha (Neg(Disy a b))= True
alpha _ = False

La función beta nos indica si una fórmula es beta, en los casos donde una fórmula beta es verdadera es cuando tenemos una disyunción, 
una doble implicación, una doble negación de una disyunción, una doble negación de una doble implicación y la negación de una conjunción. 
Otros casos que no sean los que están indicados serán falsos.
beta :: LProp -> Bool
beta (Disy a b)=True
beta (Syss a b)=True
beta (Neg(Neg (Disy a b)))= True
beta (Neg(Neg (Syss a b)))= True
beta (Neg(Conj a b))= True
beta _ = False

La función sigma nos indica cuando es verdadera una función sigma, esto sucede solamente cuando tenemos negación una doble negación 
de cualquier cosa y cuando estamos negando si algo es verdadero o falso. En otros casos que sean diferentes a los mencionados el resultado será falso.
sigma :: LProp -> Bool
sigma (Neg(Neg(a))) = True
sigma (Neg(T)) = True
sigma (Neg(F)) = True
sigma _ = False

La función expAlpha se tiene que recibir una lista de fórmulas lógicas y una fórmula lógica, 
y se tiene que regresar una lista del tipo LProp. En esta función se expande cuando tenemos casos que una fórmula alpha, 
como la conjunción y la negación de la disyunción, y la acomodamos como forma de una lista. 
En caso de que en la cabeza no se encuentre una función alpha se tiene revisar la cola para encontrar una función alpha. 
expAlpha :: [LProp] -> LProp -> [LProp]
expAlpha ((Conj a b): xs) (Conj x y)  = a : b : xs
expAlpha ((Neg(Disy a b)): xs) (Conj x y) = Neg (a): Neg (b): xs
expAlpha (_:xs) (Conj x y) = x : expAlpha xs (Conj x y)

La función expBeta utiliza el mismo algoritmo que ocupamos en la función expAlpha, solo que en esta función regresamos una lista con dos listas de dentro de ella, 
porque en la disyunción hay una bifurcación donde se debe de poner en una de ellas una fórmula lógica que tenemos unida por el conector de la disyunción, 
en otro caso donde la función beta no se encuentre en la cabeza, se revisara el resto de la lista y pegara la cabeza de la cola con la funcion auxiliar pegar
expBeta :: [LProp] -> LProp -> ([LProp], [LProp])
expBeta ((Disy a b): xs) f@(Disy c d)  = ((c:xs),(d:xs))
expBeta (x: xs) f@(Disy c d)  = pegar (expBeta xs f) (x)

La función auxiliar, recibe una dupla de LProp y una LProp, toma la LProp solitaria y la pega en cada lista de la dupla 
pegar :: ([LProp], [LProp]) -> LProp -> ([LProp], [LProp])
pegar (f,l) x = ((f++[x]),(l++[x])) 

La función expSigma recibe una lista de LProp Y una LProp, regresa la lista de LProp trasformada con las reglas sigma, 
es decir quita las dobles negaciones
expSigma :: [LProp] -> LProp -> [LProp]
expSigma ((Neg(Neg a)):xs) (Neg(Neg b)) = (a:xs)
expSigma (x:xs) f@(Neg(Neg b)) = x: expSigma xs f

En la función tableaux se recibe una LProp y se regresa un tableaux, lo que hace la funcion es tomar la LProp, y descomponerla a partir de si su
esquema es Alpha o Beta, hasta llegar a las literales que se representancomo hojas.
consTableaux :: LProp -> Tableaux

consTableaux (VarP a) = Hoja [VarP a]
consTableaux (Neg(VarP a)) = Hoja [(Neg(VarP a))]
consTableaux (Neg(Neg (VarP a))) = Hoja [(VarP a)]
consTableaux (Disy a b) = Beta [(Disy a b)] (consTableaux a)  (consTableaux b) 
consTableaux (Neg (Conj a b)) = Beta [(Neg (Conj a b))]  (consTableaux (Neg a))  (consTableaux (Neg b))
consTableaux (Impl a b) = Beta [(Impl a b)] (consTableaux (Neg a)) (consTableaux (b))
consTableaux (Conj a b) = Alpha [(Conj a b)] (consTableaux a) (consTableaux b)
consTableaux (Syss a b) = Beta [Syss a b] (consTableaux (Conj a b)) (consTableaux (Conj (Neg a)(Neg b))) 
consTableaux (Neg(Disy a b)) = Alpha [(Neg(Disy a b))] (consTableaux (Neg a)) (consTableaux (Neg b))  
consTableaux (Neg (Impl a b)) = Alpha [(Neg(Impl a b))] (consTableaux a)  (consTableaux (Neg b))
consTableaux (Neg (Syss a b))= Beta [(Neg(Syss a b))] (consTableaux (Neg a)) (consTableaux b)

-- esta Funcion recibe un String y una "clave" (int) y regresa otro String pero estando cifrado a partir del original. 
primero toma el Strin que ingresamos y le aplica la funcion tocino a todo el String, luego le suma a cada int el valor de la clave 
para despeues aplicarle aux2 a toda la lista transformando los numeros char y regresando a un String 
ceasear :: String -> Int -> String
ceasear s n =map aux2 (map (+n)(map (tocino) s)) 

--esta funcion recibre un String modificado y la misma clave de cifrado para devolver el String original.
 -para recuperar el valor original del String, se calcula 27 -((mod n 26)-1)) 
evilceasear :: String -> Int -> String
evilceasear s n =map aux2 (map (+t)(map (tocino) s)) 
                            where t = 27-((mod n 26)-1)
--esta funcion solo sirve como enlace entre la funcion  principal y la funcion aux
tocino::Char -> Int 
tocino a = aux a 0 


--la funcion aux  toma un Char y un 0, tregresa un int generado a partir el char entregado 
aux ::Char -> Int -> Int 
aux a n = if (a == alfabeto!!n)
    then n 
    else aux a (n+1)
    where alfabeto ="abcdefghijklmnopqrstuvwxyz"


-- la fucnion aux2 recibe un int y un char, regresa un char en base al int recibido
aux2 ::Int -> Char
aux2 n = if (n==26)
    then alfabeto!!(n-1)
    else (alfabeto!!((mod n 26 )-1))
    
    where alfabeto ="abcdefghijklmnopqrstuvwxyz"
