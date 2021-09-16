-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char              -- Conversion de Chars de/vers Int et autres.
import System.IO              -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces =
    do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

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

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbole ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
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
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2') =
              showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.

-- instance Show Sexp where
--     showsPrec p = showSexp'


-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

this is an error

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint                   -- Int
           | Lboo                   -- Bool
           | Larw Ltype Ltype       -- τ₁ → τ₂
           | Ltup [Ltype]           -- tuple τ₁...τₙ
           deriving (Show, Eq)

data Lexp = Lnum Int                -- Constante entière.
          | Lvar Var                -- Référence à une variable.
          | Lhastype Lexp Ltype     -- Annotation de type.
          | Lcall Lexp Lexp         -- Appel de fonction, avec un argument.
          | Lfun Var Lexp           -- Fonction anonyme prenant un argument.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Llet [(Var, Lexp)] Lexp
          | Lif Lexp Lexp Lexp      -- Expression conditionelle.
          | Ltuple [Lexp]           -- Construction de tuple
          | Lfetch Lexp [Var] Lexp  -- lecture d'un tuple
          deriving (Show, Eq)

---------------------------------------------------------------------------
-- Conversion de Sexp à Lexp                                             --
---------------------------------------------------------------------------

argsNumError :: Sexp -> String
argsNumError = \x -> "Insufficient arguments for expression " ++ showSexp x

argsMatchError :: Sexp -> String
argsMatchError = \x -> "Couldn't match expected arguments in: " ++ showSexp x

unrecExp :: Sexp -> String
unrecExp = \x -> "Unrecognized Psil expression: " ++ showSexp x

unrecType :: Sexp -> String
unrecType = \x -> "Unrecognized Psil type: " ++ showSexp x

-- Convertit une liste Sexp en une liste Haskell avec les éléments d'intérêts
sexp2list :: Sexp -> [Sexp]
sexp2list s = loop s []
    where
        loop (Scons hds tl) acc = loop hds (tl : acc)
        loop Snil acc = acc
        loop _ _ = error ("Improper list: " ++ show s)

-- Analyse une Sexp et construit une Lexp équivalente
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (se@(Scons _ _)) =
    let
        selist = sexp2list se
    in
        case selist of
            (Ssym "hastype" : e : t : []) -> Lhastype (s2l e) (s2t t)
            (Ssym "call" : es) ->
                if length es < 2
                then error (argsNumError se)
                else s2l' se selist
            (Ssym "fun" : es) ->
                if length es < 2
                then error (argsNumError se)
                else s2l' se selist
            (Ssym "let" : es) ->
                if length es < 1
                then error (argsNumError se)
                else Llet (s2d se (init es)) (s2l (last es))
            (Ssym "if" : e1 : e2 : e3 : []) -> Lif (s2l e1) (s2l e2) (s2l e3)
            (Ssym "tuple" : es) -> Ltuple (map s2l es)
            (Ssym "fetch" : tpl : xs : e : []) -> Lfetch (s2l tpl)
                (map (\x -> case s2l x of
                    Lvar s -> s
                    _ -> error (argsMatchError se))
                (sexp2list xs)) (s2l e)
            _ -> error (unrecExp se)
s2l se = error (unrecExp se)

-- Fonction auxiliaire de s2l traitant les cas avec récursion (currying)
-- Fonction accomodant au sucre syntaxique de fonctions et d'appels de fonction
s2l' :: Sexp -> [Sexp] -> Lexp
s2l' se selist =
    case selist of
        (Ssym "call" : e : e1 : []) -> Lcall (s2l e) (s2l e1)
        (Ssym "call" : es) ->
            Lcall (s2l' se ([Ssym "call"] ++ init es)) (s2l (last es))
        (Ssym "fun" : v : e : []) ->
            case s2l v of
                Lvar x -> Lfun x (s2l e)
                _ -> error (argsMatchError se)
        (Ssym "fun" : v : vs) ->
            case s2l v of
                Lvar x -> Lfun x (s2l' se ([Ssym "fun"] ++ vs))
                _ -> error (argsMatchError se)
        _ -> error (unrecExp se)

-- Analyse une Sexp et construit un Ltype équivalent
-- Appelée lorsqu'une expression indique des types
s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
s2t (Ssym "Bool") = Lboo
s2t (se@(Scons _ _)) =
    let
        selist = sexp2list se
    in
        case selist of
            (Ssym "Tuple" : ts) -> Ltup (map s2t ts)
            _ | length selist < 2 -> error (unrecType se)
              | (last (init selist)) == Ssym "->" -> s2t' se selist
              | otherwise -> error (unrecType se)
s2t se = error (unrecType se)

-- Fonction auxiliaire de s2t traitant les cas avec récursion (currying)
-- Fonction accomodant au sucre syntaxique de types de fonctions
s2t' :: Sexp -> [Sexp] -> Ltype
s2t' se selist =
    case selist of
        (ta : Ssym "->" : tr : []) -> Larw (s2t ta) (s2t tr)
        _ | (last (init selist)) == Ssym "->" ->
              Larw (s2t (head selist)) (s2t' se (tail selist))
          | otherwise -> error (unrecType se)

-- Analyse une Sexp et construit une liste de tuple (Var, Lexp)
-- Appelée lors de l'analyse de l'expression let où s'y trouve des déclarations
-- Les fonctions getArgs et getTypes récupèrent les variables et les types
    -- associées à une fonction lors d'une déclaration afin de fournir
    -- les informations nécessaires aux étapes d'inférence de types et
    -- d'évaluation d'expression
s2d :: Sexp -> [Sexp] -> [(Var, Lexp)]
s2d _ [] = []
s2d se (d : ds) =
    let
        getArgs [] = []
        getArgs (a : as) = (head (sexp2list a)) : getArgs as 
        getTypes [] = error "Type not specified"
        getTypes (t : []) = Ssym "->" : t : []
        getTypes (t : ts) = (last (sexp2list t)) : getTypes ts
        selist = sexp2list d
    in
        if length selist < 2
        then error ("Invalid declaration: " ++ (showSexp se))
        else 
            case selist of
                (Ssym x : e : []) -> (x, s2l e) : s2d se ds
                (Ssym x : t : e : []) -> (x, Lhastype (s2l e) (s2t t))
                    : s2d se ds
                (Ssym x : es) -> (x, Lhastype
                    (s2l' se ([Ssym "fun"] ++ getArgs (init(init es))
                        ++ [(last es)]))
                    (s2t' se (getTypes (init es)))) : (s2d se ds)
                _ -> error ("Unrecognized Psil declaration: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vbool Bool
           | Vtuple [Value]
           | Vfun (Maybe String) (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec p (Vtuple vs) = showValues "[" vs
        where showValues _ [] = showString "]"
              showValues sep (v:vs')
                = showString sep . showsPrec p v . showValues " " vs'
    showsPrec _ (Vfun (Just n) _) =
          showString "<fun " . showString n . showString ">"
    showsPrec _ (Vfun Nothing _) = showString "<fun>"

type Env = [(Var, Value, Ltype)]

-- L'environnement initial qui contient les fonctions prédéfinies et leur type.
env0 :: Env
env0 = [prim "+"  (+) Vnum  Lint,
        prim "-"  (-) Vnum  Lint,
        prim "*"  (*) Vnum  Lint,
        prim "/"  div Vnum  Lint,
        prim "="  (==) Vbool Lboo,
        prim ">=" (>=) Vbool Lboo,
        prim "<=" (<=) Vbool Lboo]
       where prim name op cons typ =
              (name,
               Vfun (Just name)
                    (\ (Vnum x) -> Vfun Nothing
                                       (\ (Vnum y) -> cons (x `op` y))),
               Larw Lint (Larw Lint typ))

-- Point d'entrée de l'évaluation
eval :: Env -> Lexp -> Value
eval env e =
  -- Extrait la liste des variables et la liste de leur valeurs,
  -- et ignore leurs types, qui n'est plus utile pendant l'évaluation.
  eval2 (map (\(x,_,_) -> x) env) e (map (\(_,v,_) -> v) env)

e2lookup :: [Var] -> Var -> Int          -- Find position within environment
e2lookup env x = e2lookup' env 0
    where e2lookup' :: [Var] -> Int -> Int
          e2lookup' [] _ = error ("Unknown variable: " ++ show x)
          e2lookup' (x':_) i | x == x' = i
          e2lookup' (_:xs) i = e2lookup' xs (i + 1)

-------------- La fonction d'évaluation principale.  ------------------------
-- Au lieu de recevoir une liste de paires (Var, Val), on passe la liste
-- des noms de variables (`senv`) et la liste des valeurs correspondantes
-- (`venv`) séparément de manière à ce que (eval2 senv e) renvoie une
-- fonction qui a déjà fini d'utiliser `senv`.
eval2 :: [Var] -> Lexp -> ([Value] -> Value)
eval2 _ (Lnum n) = \_ -> Vnum n
eval2 senv (Lhastype e _) = eval2 senv e
eval2 senv (Lvar x) =
    -- Calcule la position que la variable aura dans `venv`.
    let
        i = e2lookup senv x
    -- Renvoie une fonction qui n'a plus besoin de charcher et comparer le nom.
    -- De cette manière, si la fonction renvoyée par (eval2 senv v) est appelée
    -- plusieurs fois, on aura fait la recherche dans `senv` une seule fois.
    in
        \venv -> venv !! i

eval2 senv (Lcall o a) = \venv ->
    let
        Vfun _ f = eval2' o
        n = eval2' a
        eval2' = \x -> (eval2 senv x) venv
    in
        f n

eval2 senv (Lfun a e) =
    \venv -> (Vfun Nothing (\v -> (eval2 (a : senv) e) (v : venv)))

eval2 senv (Llet ds b) = \venv ->
    let
        (vars, exps) = unzip ds
        senv' = vars ++ senv
        venv' = (map eval2' exps) ++ venv
        eval2' = \v -> (eval2 senv' v) venv'
    in
        eval2' b

eval2 senv (Lif e1 e2 e3) = \venv ->
    let
        eval2' = \x -> (eval2 senv x) venv
    in
        case eval2' e1 of
            Vbool True -> eval2' e2
            _ -> eval2' e3

eval2 senv (Ltuple e) = \venv ->
    let
        eval2' = \v -> (eval2 senv v) venv
    in
        Vtuple (map eval2' e)

eval2 senv (Lfetch tup vs e) = \venv ->
    let
        Vtuple tuplist = (eval2 senv tup) venv
        senv' = vs ++ senv
        venv' = tuplist ++ venv
    in
        (eval2 senv' e) venv'

---------------------------------------------------------------------------
-- Vérificateur de types                                                 --
---------------------------------------------------------------------------

type TEnv = [(Var, Ltype)]
type TypeError = String

-- Les valeurs ne servent à rien pendant la vérification de type,
-- donc extrait la partie utile de `env0`.
tenv0 :: TEnv
tenv0 = (map (\(x,_,t) -> (x,t)) env0)

-- Recherche du type d'une variable
tlookup :: [(Var, a)] -> Var -> a
tlookup [] x = error ("Unknown variable: " ++ x)
tlookup ((x',t):_) x | x == x' = t
tlookup (_:env) x = tlookup env x

-- Règles de typage - Règle de synthèse
infer :: TEnv -> Lexp -> Ltype
infer _ (Lnum _) = Lint
infer tenv (Lvar x) = tlookup tenv x

infer tenv (Lhastype e t)
    | te == Nothing = t
    | otherwise = let Just msg = te in error msg
    where
        te = check tenv e t

infer tenv (Lcall e1 e2)
    | te == Nothing = t2
    | otherwise = let Just msg = te in error msg
    where
        Larw t1 t2 = infer tenv e1
        te = check tenv e2 t1

infer tenv (Llet ds b) =
    let
        (tenvn, tenvt) = unzip tenv
        (vars, exps) = unzip ds
        tenvn' = vars ++ tenvn
        tenvt' = (map infer' exps) ++ tenvt
        infer' = \e -> infer (zip tenvn' tenvt') e
    in
        infer' b

infer tenv (Ltuple es) = Ltup (map (infer tenv) es)
infer _ (Lfun _ _)     = error "Can't infer type of `fun`"
infer _ (Lif _ _ _)    = error "Can't infer type of `if`"
infer _ (Lfetch _ _ _) = error "Can't infer type of `fetch`"

-- Règles de typage - Jugement de vérification
check :: TEnv -> Lexp -> Ltype -> Maybe TypeError
check tenv (Lfun x body) (Larw t1 t2) = check ((x, t1) : tenv) body t2
check _ (Lfun _ _) t = Just ("Not a function type: " ++ show t)

check tenv (Lif e1 e2 e3) t
    | te1 /= Nothing = te1
    | te2 /= Nothing = te2
    | te3 /= Nothing = te3
    | otherwise = Nothing
    where
        te1 = check tenv e1 Lboo
        te2 = check tenv e2 t
        te3 = check tenv e3 t

check tenv (Lfetch tup xs e) t =
    let
        tupleType = case tup of
            Ltuple _ -> let Ltup list = infer tenv tup in list
            Lvar var -> case tlookup tenv var of
                Ltup list -> list
                _ -> error ("Not a tuple")
            _ -> error ("Not a tuple")
        tuplength = length tupleType
        varlength = length xs
    in
        if tuplength /= varlength
        then error ("Tuple length and number of variables mismatch: " ++
            show tuplength ++ " != "  ++ show varlength)
        else check ((zip xs tupleType) ++ tenv) e t

check tenv e t =
    -- Essaie d'inférer le type et vérifie s'il correspond au type attendu
    let
        t' = infer tenv e
    in
        if t == t' then Nothing
        else Just ("Type mismatch: " ++ show t ++ " != " ++ show t')

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do filestring <- readFile filename
       (hPutStr stdout)
           (let sexps s = case parse pSexps filename s of
                            Left _ -> [Ssym "#<parse-error>"]
                            Right es -> es
            in (concat
                (map (\ sexp -> let { ltyp = infer tenv0 lexp
                                   ; lexp = s2l sexp
                                   ; val = eval env0 lexp }
                               in "  " ++ show val
                                  ++ " : " ++ show ltyp ++ "\n")
                     (sexps filestring))))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = infer tenv0 . lexpOf

valOf :: String -> Value
valOf = eval env0 . lexpOf
