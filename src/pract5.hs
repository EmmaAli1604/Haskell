--practica 4

{-Jacome Delgado Alejandro
Jiménez Sánchez Emma Alicia

Esta mejor explicado en el readme .
-}

--esto importa lo nesesario para hacer la instancia del Show 
{-# LANGUAGE InstanceSigs #-}

-- alias para los String que seran las variables
type Nombre = String
-- se define la data para las LProp
data LProp = T | F | VarP Nombre | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp | Neg LProp    
-- se define la data para los Tableaux
data Tableaux = Hoja [LProp] | Alpha [LProp] Tableaux Tableaux | Beta [LProp] Tableaux Tableaux --deriving Show 
--instacia para darle el formato adecuado a las LProp
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

--instancia para darle el formato adecuado a los Tableaux
instance Show Tableaux where
    show :: Tableaux -> String
    show (Hoja [a]) ="\n"++"\t"++ "Hoja " ++"["++show a ++"]"
    show (Alpha [a] x y ) ="\t" ++ "Alpha " ++"["++ show a ++"]" ++ show x ++ show y
    show (Beta [a] x y) ="\n"++"\t"++"Beta " ++"["++ show a ++ "]" ++"\t"++ show x ++"\t"++ show y


--ejercicio 1
literales :: [LProp] -> Bool
literales [] = True
literales ((VarP a):xs) = literales xs
literales (T : xs) = literales xs
literales (F : xs) = literales xs
literales (Neg (T) : xs) = literales xs
literales (Neg (F) : xs) = literales xs 
literales (Neg (VarP a) : xs) = literales xs
literales (_: xs) = False

--ejercicio 2
nextF :: [LProp] ->  LProp
nextF ((Conj a b):xs)= Conj a b
nextF ((Disy a b):xs)= Disy a b
nextF ((Impl a b):xs)= Impl a b
nextF ((Syss a b):xs)= Syss a b
nextF ((Neg(VarP a)):xs)= nextF xs
nextF ((VarP a ):xs)=nextF xs

--ejercicio 3
alpha :: LProp -> Bool
alpha (Conj a b)=True
alpha (Neg(Impl a b))= True
alpha (Neg(Disy a b))= True
alpha _ = False

--ejercicio 4
beta :: LProp -> Bool
beta (Disy a b)=True
beta (Syss a b)=True
beta (Neg(Neg (Disy a b)))= True
beta (Neg(Neg (Syss a b)))= True
beta (Neg(Conj a b))= True
beta _ = False

--ejercicio 5
sigma :: LProp -> Bool
sigma (Neg(Neg(a))) = True
sigma (Neg(T)) = True
sigma (Neg(F)) = True
sigma _ = False

--ejercicio 6
expAlpha :: [LProp] -> LProp -> [LProp]
expAlpha ((Conj a b): xs) (Conj x y)  = a : b : xs
expAlpha ((Neg(Disy a b)): xs) (Conj x y) = Neg (a): Neg (b): xs
expAlpha ((Neg(Impl a b)): xs) (Conj x y) =  a : Neg (b): xs
expAlpha (_:xs) (Conj x y) = x : expAlpha xs (Conj x y)

--ejercicio 7
expBeta :: [LProp] -> LProp -> ([LProp], [LProp])
expBeta ((Disy a b): xs) f@(Disy c d)  = ((c:xs),(d:xs))
expBeta (Neg(Conj a b): xs) f@(Disy c d) = ((Neg a:xs),(Neg b:xs))
expBeta ((Impl a b): xs) f@(Disy c d)  = ((Neg a:xs),(Neg b:xs))
expBeta (x: xs) f@(Disy c d)  = pegar (expBeta xs f) (x)

pegar :: ([LProp], [LProp]) -> LProp -> ([LProp], [LProp])
pegar (f,l) x = ((f++[x]),(l++[x])) 


--ejercicio 8
expSigma :: [LProp] -> LProp -> [LProp]
expSigma ((Neg(Neg a)):xs) (Neg(Neg b)) = (a:xs)
expSigma (x:xs) f@(Neg(Neg b)) = x: expSigma xs f


--ejercicio 9
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




--ejercicio extra
ceasear :: String -> Int -> String
ceasear s n =map aux2 (map (+n)(map (tocino) s)) 

   
evilceasear :: String -> Int -> String
evilceasear s n =map aux2 (map (+t)(map (tocino) s)) 
                            where t = 27-((mod n 26)-1)
tocino::Char -> Int 
tocino a = aux a 0 

aux ::Char -> Int -> Int 
aux a n = if (a == alfabeto!!n)
    then n 
    else aux a (n+1)
    where alfabeto ="abcdefghijklmnopqrstuvwxyz"

aux2 ::Int -> Char
aux2 n = if (n==26)
    then alfabeto!!(n-1)
    else (alfabeto!!((mod n 26 )-1))
    
    where alfabeto ="abcdefghijklmnopqrstuvwxyz"