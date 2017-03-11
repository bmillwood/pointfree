module Plugin.Pl.Parser (parsePF) where

import Plugin.Pl.Common

import qualified Language.Haskell.Exts as HSE

todo :: (Show e) => e -> a
todo thing = error ("pointfree: not supported: " ++ show thing)

nameString :: HSE.Name HSE.SrcSpanInfo -> (Fixity, String)
nameString (HSE.Ident _loc s) = (Pref, s)
nameString (HSE.Symbol _loc s) = (Inf, s)

qnameString :: HSE.QName HSE.SrcSpanInfo -> (Fixity, String)
qnameString (HSE.Qual _loc m n) = fmap ((HSE.prettyPrint m ++ ".") ++) (nameString n)
qnameString (HSE.UnQual _loc n) = nameString n
qnameString (HSE.Special _loc sc) = case sc of
  HSE.UnitCon _loc -> (Pref, "()")
  HSE.ListCon _loc -> (Pref, "[]")
  HSE.FunCon _loc -> (Inf, "->")
  HSE.TupleCon _loc HSE.Boxed n -> (Inf, replicate (n-1) ',')
  HSE.TupleCon{} -> todo sc
  HSE.Cons _loc -> (Inf, ":")
  HSE.UnboxedSingleCon _loc -> todo sc

opString :: HSE.QOp HSE.SrcSpanInfo -> (Fixity, String)
opString (HSE.QVarOp _loc qn) = qnameString qn
opString (HSE.QConOp _loc qn) = qnameString qn

list :: [Expr] -> Expr
list = foldr (\y ys -> cons `App` y `App` ys) nil

hseToExpr :: HSE.Exp HSE.SrcSpanInfo -> Expr
hseToExpr expr = case expr of
  HSE.Var _loc qn -> uncurry Var (qnameString qn)
  HSE.IPVar{} -> todo expr
  HSE.Con  _loc qn -> uncurry Var (qnameString qn)
  HSE.Lit _loc l -> case l of
    HSE.String _locs s _slit -> list (map (Var Pref . show) s)
    _ -> Var Pref (HSE.prettyPrint l)
  HSE.InfixApp _loc p op q -> apps (Var Inf (snd (opString op))) [p,q]
  HSE.App _loc f x -> hseToExpr f `App` hseToExpr x
  HSE.NegApp _loc e -> Var Pref "negate" `App` hseToExpr e
  HSE.Lambda _loc ps e -> foldr (Lambda . hseToPattern) (hseToExpr e) ps
  HSE.Let _loc bs e -> case bs of
    HSE.BDecls _loc ds -> Let (map hseToDecl ds) (hseToExpr e)
    HSE.IPBinds _loc ips -> todo ips
  HSE.If _loc b t f -> apps if' [b,t,f]
  HSE.Case{} -> todo expr
  HSE.Do{} -> todo expr
  HSE.MDo{} -> todo expr
  HSE.Tuple _loc HSE.Boxed es -> apps (Var Inf (replicate (length es - 1) ','))  es
  HSE.TupleSection{} -> todo expr
  HSE.List _loc xs -> list (map hseToExpr xs)
  HSE.Paren _loc e -> hseToExpr e
  HSE.LeftSection _loc l op -> Var Inf (snd (opString op)) `App` hseToExpr l
  HSE.RightSection _loc op r -> flip' `App` Var Inf (snd (opString op)) `App` hseToExpr r
  HSE.RecConstr{} -> todo expr
  HSE.RecUpdate{} -> todo expr
  HSE.EnumFrom _loc x -> apps (Var Pref "enumFrom") [x]
  HSE.EnumFromTo _loc x y -> apps (Var Pref "enumFromTo") [x,y]
  HSE.EnumFromThen _loc x y -> apps (Var Pref "enumFromThen") [x,y]
  HSE.EnumFromThenTo _loc x y z -> apps (Var Pref "enumFromThenTo") [x,y,z]
  _ -> todo expr

apps :: Expr -> [HSE.Exp HSE.SrcSpanInfo] -> Expr
apps f xs = foldl (\a x -> a `App` hseToExpr x) f xs 

hseToDecl :: HSE.Decl HSE.SrcSpanInfo -> Decl
hseToDecl dec = case dec of
  HSE.PatBind _ (HSE.PVar _loc n) (HSE.UnGuardedRhs _ e) Nothing ->
    Define (snd (nameString n)) (hseToExpr e)
  HSE.FunBind _loc [HSE.Match _ n ps (HSE.UnGuardedRhs _ e) Nothing] ->
    Define (snd (nameString n)) (foldr (\p x -> Lambda (hseToPattern p) x) (hseToExpr e) ps)
  _ -> todo dec

hseToPattern :: HSE.Pat HSE.SrcSpanInfo -> Pattern
hseToPattern pat = case pat of
  HSE.PVar _loc n -> PVar (snd (nameString n))
  HSE.PInfixApp _loc l (HSE.Special _ (HSE.Cons _)) r -> PCons (hseToPattern l) (hseToPattern r)
  HSE.PTuple _loc HSE.Boxed [p,q] -> PTuple (hseToPattern p) (hseToPattern q)
  HSE.PParen _loc p -> hseToPattern p
  HSE.PWildCard _loc -> PVar "_"
  _ -> todo pat

parsePF :: String -> Either String TopLevel
parsePF inp = case HSE.parseExp inp of
  HSE.ParseOk e -> Right (TLE (hseToExpr e))
  HSE.ParseFailed _ _ -> case HSE.parseDecl inp of
    HSE.ParseOk d -> Right (TLD True (hseToDecl d))
    HSE.ParseFailed _ err -> Left err
