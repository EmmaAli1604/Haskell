--practica 4

{-Jacome Delgado Alejandro
Jiménez Sánchez Emma Alicia

Esta mejor explicado en el readme .
-}

--esto importa lo nesesario para hacer la instancia del Show 
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE InstanceSigs #-}
type Nombre = String
data LProp = T | F | VarP Nombre | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp | Neg LProp 
     
data Tableaux = Hoja [LProp] | Alpha [LProp] Tableaux | Beta [LProp] Tableaux Tableaux

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


{-esLiteral :: Lprop -> Bool
esLiteral (VarP a) = True
esLiteral (Neg a) = esLiteral a
esLiteral T = True
esLiteral F = True
esLiteral _ = False

literales  = and . map esLiteral 
-}

literales :: [LProp] -> Bool
literales [] = True
literales ((VarP a):xs) = literales xs
literales (T : xs) = literales xs
literales (F : xs) = literales xs
literales (Neg (T) : xs) = literales xs
literales (Neg (F) : xs) = literales xs 
literales (Neg (VarP a) : xs) = literales xs
literales (_: xs) = False


nextF :: [LProp] ->  LProp
nextF [] = error "implementar"
nextF ((Conj a b):xs)= Conj a b
nextF ((Disy a b):xs)= Disy a b
nextF ((Impl a b):xs)= Impl a b
nextF ((Syss a b):xs)= Syss a b
nextF ((Neg(VarP a)):xs)= nextF xs
nextF ((VarP a ):xs)=nextF xs

alpha :: LProp -> Bool
alpha (Conj a b)=True
alpha (Neg(Impl a b))= True
alpha (Neg(Disy a b))= True
alpha _ = False

beta :: LProp -> Bool
beta (Disy a b)=True
beta (Syss a b)=True
beta (Neg(Neg (Disy a b)))= True
beta (Neg(Neg (Syss a b)))= True
beta (Neg(Conj a b))= True
beta _ = False

sigma :: LProp -> Bool
sigma (Neg a)=True
sigma _ = False

{-expAlpha :: [LProp] -> LProp -> [LProp]
expAlpha l f@(Conj a b) = a : b : ln
                            where ln = quita f n

expBeta :: [LProp] -> LProp -> ([LProp], [LProp])
expBeta _ _ = error "implementar"-}
