--
-- Todo, use Language.Haskell
--
-- Doesn't handle string literals?
--
module Plugin.Pl.Parser (parsePF) where

import Plugin.Pl.Common

import qualified Language.Haskell.Exts as HSE

todo :: (Show e) => e -> a
todo thing = error ("pointfree: not supported: " ++ show thing)

nameString :: HSE.Name -> String
nameString (HSE.Ident s) = s
nameString (HSE.Symbol s) = s

qnameString :: HSE.QName -> String
qnameString (HSE.Qual m n) = HSE.prettyPrint m ++ nameString n
qnameString (HSE.UnQual n) = nameString n
qnameString (HSE.Special sc) = case sc of
  HSE.UnitCon -> "()"
  HSE.ListCon -> "[]"
  HSE.FunCon -> "->"
  HSE.TupleCon HSE.Boxed n -> replicate (n-1) ','
  HSE.TupleCon{} -> todo sc
  HSE.Cons -> ":"
  HSE.UnboxedSingleCon -> todo sc

opString :: HSE.QOp -> String
opString (HSE.QVarOp qn) = qnameString qn
opString (HSE.QConOp qn) = qnameString qn

hseToExpr :: HSE.Exp -> Expr
hseToExpr expr = case expr of
  HSE.Var qn -> Var Pref (qnameString qn)
  HSE.IPVar{} -> todo expr
  HSE.Con qn -> Var Pref (qnameString qn)
  HSE.Lit l -> Var Pref (HSE.prettyPrint l)
  HSE.InfixApp p op q -> apps (Var Inf (opString op)) [p,q]
  HSE.App f x -> hseToExpr f `App` hseToExpr x
  HSE.NegApp e -> Var Pref "negate" `App` hseToExpr e
  HSE.Lambda _ ps e -> foldr (Lambda . hseToPattern) (hseToExpr e) ps
  HSE.Let bs e -> case bs of
    HSE.BDecls ds -> Let (map hseToDecl ds) (hseToExpr e)
    HSE.IPBinds ips -> todo ips
  HSE.If b t f -> apps if' [b,t,f]
  HSE.Case{} -> todo expr
  HSE.Do{} -> todo expr
  HSE.MDo{} -> todo expr
  HSE.Tuple es -> foldl (\a x -> a `App` hseToExpr x)
    (Var Pref (replicate (length es - 1) ','))  es
  HSE.TupleSection{} -> todo expr
  HSE.List xs -> foldr (\y ys -> cons `App` hseToExpr y `App` ys) nil xs
  HSE.Paren e -> hseToExpr e
  HSE.LeftSection l op -> Var Inf (opString op) `App` hseToExpr l
  HSE.RightSection op r -> flip' `App` Var Inf (opString op) `App` hseToExpr r
  HSE.RecConstr{} -> todo expr
  HSE.RecUpdate{} -> todo expr
  HSE.EnumFrom x -> apps (Var Pref "enumFrom") [x]
  HSE.EnumFromTo x y -> apps (Var Pref "enumFromTo") [x,y]
  HSE.EnumFromThen x y -> apps (Var Pref "enumFromThen") [x,y]
  HSE.EnumFromThenTo x y z -> apps (Var Pref "enumFromThenTo") [x,y,z]
  _ -> todo expr

apps :: Expr -> [HSE.Exp] -> Expr
apps f xs = foldl (\a x -> a `App` hseToExpr x) f xs 

hseToDecl :: HSE.Decl -> Decl
hseToDecl dec = case dec of
  HSE.PatBind _ (HSE.PVar n) Nothing (HSE.UnGuardedRhs e) (HSE.BDecls []) ->
    Define (nameString n) (hseToExpr e)
  HSE.FunBind [HSE.Match _ n ps Nothing (HSE.UnGuardedRhs e) (HSE.BDecls [])] ->
    Define (nameString n) (foldr (\p x -> Lambda (hseToPattern p) x) (hseToExpr e) ps)
  _ -> todo dec

hseToPattern :: HSE.Pat -> Pattern
hseToPattern pat = case pat of
  HSE.PVar n -> PVar (nameString n)
  HSE.PInfixApp l (HSE.Special HSE.Cons) r -> PCons (hseToPattern l) (hseToPattern r)
  HSE.PTuple [p,q] -> PTuple (hseToPattern p) (hseToPattern q)
  HSE.PParen p -> hseToPattern p
  HSE.PWildCard -> PVar "_"
  _ -> todo pat

parsePF :: String -> Either String TopLevel
parsePF inp = case HSE.parseExp inp of
  HSE.ParseOk e -> Right (TLE (hseToExpr e))
  HSE.ParseFailed _ _ -> case HSE.parseDecl inp of
    HSE.ParseOk d -> Right (TLD True (hseToDecl d))
    HSE.ParseFailed _ err -> Left err
