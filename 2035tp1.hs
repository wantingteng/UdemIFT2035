
-- Devoir1 2035
-- Auteur: teng wanting
-- Auteur: yongkang he



-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Librairie d'analyse syntaxique.
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Llit Int             -- Litéral entier.
          | Lid Var              -- Référence à une variable.
          | Labs Var Lexp        -- Fonction anonyme prenant un argument.
          | Lfuncall Lexp [Lexp] -- Appel de fonction, avec arguments "curried"
          | Lmkref Lexp          -- Construire une "ref-cell".
          | Lderef Lexp          -- Chercher la valeur d'une "ref-cell".
          | Lassign Lexp Lexp    -- Changer la valeur d'une "ref-cell".
          | Lite Lexp Lexp Lexp  -- If/then/else.
          | Ldec Var Lexp Lexp   -- Déclaration locale non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lrec [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Conversion de Sexp à Lambda --------------------------------------------

s2l :: Sexp -> Lexp
s2l (Snum n) = Llit n
s2l (Ssym s) = Lid s
-- ¡¡ COMPLETER !!
s2l (Snil) = error "Erreur d'un manque de detail"

-- lambda expression (λ x body)
s2l (Snode (Ssym "λ") [Ssym x, body]) = Labs x (s2l body)

-- expression (ref! expr) Construire une "ref-cell"
s2l (Snode (Ssym "ref!") [expr]) = Lmkref (s2l expr)

-- expression (get! expr) Chercher la valeur d'une "ref-cell"
s2l (Snode (Ssym "get!") [expr]) = Lderef (s2l expr)

-- expression (set! ref val) Changer la valeur d'une "ref-cell".
s2l (Snode (Ssym "set!") [ref, val]) = Lassign (s2l ref) (s2l val)

-- expression (if condition alors sinon) If/then/else.
s2l (Snode (Ssym "if") [condition, alors, sinon]) =
            Lite (s2l condition) (s2l alors) (s2l sinon)

-- expression (let var e1 e2) Déclaration locale non-récursive
s2l (Snode (Ssym "let") [Ssym var, e1, e2]) =  Ldec var (s2l e1) (s2l e2)

-- expression (letrec bindings body) declaration locale posiblement recursive
s2l (Snode (Ssym "letrec") [liaisons, corps]) =
    let (Snode (Snode (Ssym premiere) premiereExpr) reste) = liaisons in
    Lrec (convertirLiaisons (Snode (Ssym premiere) premiereExpr) :
          map convertirLiaisons (getList liaisons)) (s2l corps)
  where
    -- fct convert la liste d'expr S-expr en une liste de paires(String, Lexp)
    convertirLiaisons :: Sexp -> (Var, Lexp)
    convertirLiaisons (Snode (Ssym var) [sexpVal]) = (var, s2l sexpVal)
    convertirLiaisons _ = error "Format de liaison non valide"

    -- Fct auxiliaire convertit des listes d'expr S-expr en listes Haskell
    getList :: Sexp -> [Sexp]
    getList (Snode _ sexps) = sexps
    getList _ = error "Liste d'expressions S-expressions attendue"


-- Appel de fonction, avec arguments "curried".
s2l (Snode func args) = Lfuncall (s2l func) (map s2l args)

s2l se = error ("Expression Slip inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Représentation du "tas" ------------------------------------------------

-- Notre tas est représenté par une arbre binaire de type "trie".
-- La position du nœud qui contient l'info pour l'addresse `p` est
-- déterminée par la séquence de bits dans la représentation binaire de `p`.

data Heap = Hempty | Hnode (Maybe Value) Heap Heap

hlookup :: Heap -> Int -> Maybe Value
hlookup Hempty _ = Nothing
hlookup (Hnode mv _ _) 0 = mv
hlookup _ p | p < 0 = error "hlookup sur une adresse négative"
hlookup (Hnode _ e o) p = 
    hlookup (if p `mod` 2 == 0 then e else o) (p `div` 2)

hinsert :: Heap -> Int -> Value -> Heap
hinsert _ p _ | p < 0 = error "hinsert sur une adresse négative"
-- ¡¡ COMPLETER !!


-- Inserer dans un tas vide 
-- cas 1) a la position 0 en creant un nouveau noeud avec v
-- cas 2) a une autre position que celle du 0 en inserant v dans un sous-tas
hinsert Hempty p v
          | p == 0 = Hnode (Just v) Hempty Hempty
          | p `mod` 2 == 0 = Hnode Nothing(hinsert Hempty (p `div` 2) v) Hempty
          | otherwise = Hnode Nothing Hempty (hinsert Hempty (p `div` 2) v)

-- Inserer dans un tas non vide
-- cas 1) a la position 0 en inserant v dans le noeud courant
-- cas 2) a une autre position que celle du 0 en inserant v dans un sous-tas
hinsert (Hnode mv e o) p v
    | p == 0 = Hnode (Just v) e o
    | p `mod` 2 == 0 = Hnode mv (hinsert e (p `div` 2) v) o
    | otherwise = Hnode mv e (hinsert o (p `div` 2) v)


-- Représentation de l'environnement --------------------------------------

-- Type des tables indexées par des `α` et qui contiennent des `β`.
-- Il y a de bien meilleurs choix qu'une liste de paires, mais
-- ça suffit pour notre prototype.
type Map α β = [(α, β)]

-- Transforme une `Map` en une fonctions (qui est aussi une sorte de "Map").
mlookup :: Map Var β -> (Var -> β)
mlookup [] x = error ("Variable inconnue: " ++ show x)
mlookup ((x,v) : xs) x' = if x == x' then v else mlookup xs x'

madd :: Map Var β -> Var -> β -> Map Var β
madd m x v = (x,v) : m

-- On représente l'état de notre mémoire avec non seulement le "tas" mais aussi
-- avec un compteur d'objets de manière a pouvoir créer une "nouvelle" addresse
-- (pour `ref!`) simplement en incrémentant ce compteur.
type LState = (Heap, Int)

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vref Int
           | Vfun ((LState, Value) -> (LState, Value))

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _p (Vref p) = (\s -> "ptr<" ++ show p ++ ">" ++ s)
    showsPrec _ _ = showString "<function>"

type Env = Map Var Value

-- L'environnement initial qui contient les fonctions prédéfinies.

env0 :: Env
env0 = let binop :: (Value -> Value -> Value) -> Value
           binop op = Vfun (\ (s1, v1)
                            -> (s1, Vfun (\ (s2, v2)
                                         -> (s2, v1 `op` v2))))

           biniiv :: (Int -> Int -> Value) -> Value
           biniiv op = binop (\ v1 v2
                              -> case (v1, v2) of
                                  (Vnum x, Vnum y) -> x `op` y
                                  _ -> error ("Pas des entiers: "
                                             ++ show v1 ++ "," ++ show v2))

           binii wrap f = biniiv (\ x y -> wrap (f x y))

       in [("+", binii Vnum (+)),
           ("*", binii Vnum (*)),
           ("/", binii Vnum div),
           ("-", binii Vnum (-)),
           ("true",  Vbool True),
           ("false", Vbool False),
           ("<",  binii Vbool (<)),
           (">",  binii Vbool (>)),
           ("=",  binii Vbool (==)),
           (">=", binii Vbool (>=)),
           ("<=", binii Vbool (<=)),
           -- l'ajoute de trois fonctions predefinies dans l'environement
           ("odd", Vfun oddFunction),
           ("even", Vfun evenFunction),
           ("fac", Vfun facFunction)]
           where
            -- odd:: un entier en entre puis renvoie vrai si impair sinon faux
            oddFunction :: (LState, Value) -> (LState, Value)
            oddFunction (entier, Vnum x) = (entier, Vbool (odd x))
            oddFunction _ = error "Argument non valide pour odd"

            -- even:: un entier en entre puis renvoie vrai si pair sinon faux
            evenFunction :: (LState, Value) -> (LState, Value)
            evenFunction (entier, Vnum x) = (entier, Vbool (even x))
            evenFunction _ = error "Argument non valide pour even"

            -- fac::  un entier en entre puis renvoie le factoriel
            facFunction :: (LState, Value) -> (LState, Value)
            facFunction (entier, Vnum x)
                | x < 2 = (entier, Vnum x)
                | otherwise =
                    let (s', Vnum result) = 
                          eval entier env0 (Lfuncall(Lid "fac") [Llit (x - 1)])
                    in (s', Vnum $ x * result)
            facFunction _ = error "Argument non valide pour fac"

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

state0 :: LState
state0 = (Hempty, 0)

eval :: LState -> Env -> Lexp -> (LState, Value)
eval s _env (Llit n) = (s, Vnum n)
-- ¡¡ COMPLETER !!

-- Rechercher la var x dans l'env puis renvoie la val
eval s env (Lid x) =  (s, mlookup env x)

-- Creer une fct anonyme avec un arg puis renvoie le resultat de l'eval
eval s env (Labs var funcPart) = 
    (s, Vfun (\(s1, v)->eval s1 (madd env var v) funcPart))


-- Evaluer la ref, creer une nouvelle ref, puis renvoie la ref
eval s env (Lmkref e) =
    let (s1, v) = eval s env e
        (s2, ref) = newRef s1 v
    in (s2, Vref ref)

-- Evaluer l'obtention de la valeur stockee dans une ref
eval s env (Lderef e)=
   case eval s env e of
    ((heap, _), Vref ref) ->  
        case hlookup heap ref of
            Just val -> (s, val)  -- Si ref est trouvée, -> état + valeur
            Nothing -> error ("reference invalide" ++ show ref)-- sinon erreur
    _ -> error "on attend un reference"  -- Si ne renvoie pas un Vref


-- Evaluer la modifier de la valeur stockee dans une ref
eval s env (Lassign e1 e2) =
     case eval s env e1  of
        ((heap, nb), Vref ref) ->
            let (_, v2) = eval (heap, nb) env e2
                newHeap = hinsert heap ref v2
            in ((newHeap, nb), v2)
        _ -> error "pas de reference"


-- Evaluation de l'expression conditionnelle (if e1 then e2 else e3)
eval s env (Lite e1 e2 e3) =
    let (s1, val) = eval s env e1
        in case val of
            Vbool n -> if n
            then let (s2, val2) = eval s1 env e2
                 in (s2, val2)
            else
                let (s3, val3) = eval s1 env e3
                in (s3, val3)
            _ -> error "e1 ne retourne pas un boolean"

-- evalue une expression de declaration
eval s env (Ldec var e1 e2) =
    let (s1,v1) = eval s env e1 in   -- Évalue e1 et obtient la valeur v1
         eval s1 (madd env var v1) e2  


-- Évalue une expression Lrec x function dans l'environnement actuel env
eval s env (Lrec x function)= 
    let env' = environment s env x -- Cree un environnement en ajoutant liaison
    in uncurry eval env' function  -- Évalue "function" dans la nouvelle env'


-- Evalue une expression de fonction dans l'environemnent actuel
eval s env (Lfuncall funcExpr args) =
    case eval s env funcExpr of
        (s', Vfun f) -> applyFunc s' f args
        (_, val) ->  (s , val)
    where
    -- Appliquer une fct a une liste d'arg et renvoie le resultat
    applyFunc::LState->((LState,Value)->(LState,Value))->[Lexp]->(LState,Value)
    applyFunc st f [] = (st, Vfun f)  -- Pas d'args, renvoyer la fonction.
    applyFunc st f (x:xs) =
        let (st', argVal) = eval st env x
        in case f (st', argVal) of
            (st'', Vfun f') -> applyFunc st'' f' xs  
            (_, val) -> (st, val)  -- Évaluation réussie, renvoyer valeur.

-- Fcr auxiliaire pour construire un nouvel environnement en évaluant 
-- une liste de liaisons.
environment:: LState -> Env -> [(Var,Lexp)] -> (LState,Env)
environment s env [] = (s,env) -- cas liste vide, retourne l'env actuel
environment s env ((x,function):xs) = 
    let evaluation = eval s env function -- evalue lexp fct dans l'env actuel
    in environment (fst evaluation) (madd env x (snd evaluation)) xs -- recurse

-- creer a reference in the state
newRef :: LState -> Value -> (LState, Int)
newRef (heap, nb) value =
    let newHeap = hinsert heap nb value in
        ((newHeap, nb + 1), nb)

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = snd . eval state0 env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       -- s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
