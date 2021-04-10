module Plugin.Pl.Parser (parsePF) where

import Plugin.Pl.Common

import qualified Language.Haskell.Exts as HSE

todo :: (Functor e, Show (e ())) => e a -> r
todo thing = error ("pointfree: not supported: " ++ show (fmap (const ()) thing))

nameString :: HSE.Name a -> (Fixity, String)
nameString (HSE.Ident _ s) = (Pref, s)
nameString (HSE.Symbol _ s) = (Inf, s)

qnameString :: HSE.QName a -> (Fixity, String)
qnameString (HSE.Qual _ m n) = fmap ((HSE.prettyPrint m ++ ".") ++) (nameString n)
qnameString (HSE.UnQual _ n) = nameString n
qnameString (HSE.Special _ sc) = case sc of
  HSE.UnitCon _ -> (Pref, "()")
  HSE.ListCon _ -> (Pref, "[]")
  HSE.FunCon _ -> (Inf, "->")
  HSE.TupleCon _ HSE.Boxed n -> (Inf, replicate (n-1) ',')
  HSE.TupleCon{} -> todo sc
  HSE.Cons _ -> (Inf, ":")
  HSE.UnboxedSingleCon{} -> todo sc
  HSE.ExprHole{} -> todo sc

opString :: HSE.QOp a -> (Fixity, String)
opString (HSE.QVarOp _ qn) = qnameString qn
opString (HSE.QConOp _ qn) = qnameString qn

list :: [Expr] -> Expr
list = foldr (\y ys -> cons `App` y `App` ys) nil

hseToExpr :: HSE.Exp a -> Expr
hseToExpr expr = case expr of
  HSE.Var _ qn -> uncurry Var (qnameString qn)
  HSE.IPVar{} -> todo expr
  HSE.Con _ qn -> uncurry Var (qnameString qn)
  HSE.Lit _ l -> case l of
    HSE.String _ _ s -> list (map (Var Pref . show) s)
    _ -> Var Pref (HSE.prettyPrint l)
  HSE.InfixApp _ p op q -> apps (Var Inf (snd (opString op))) [p,q]
  HSE.App _ f x -> hseToExpr f `App` hseToExpr x
  HSE.NegApp _ e -> Var Pref "negate" `App` hseToExpr e
  HSE.Lambda _ ps e -> foldr (Lambda . hseToPattern) (hseToExpr e) ps
  HSE.Let _ bs e -> case bs of
    HSE.BDecls _ ds -> Let (map hseToDecl ds) (hseToExpr e)
    HSE.IPBinds _ ips -> todo ips
  HSE.If _ b t f -> apps if' [b,t,f]
  HSE.Case{} -> todo expr
  HSE.Do{} -> todo expr
  HSE.MDo{} -> todo expr
  HSE.Tuple _ HSE.Boxed es -> apps (Var Inf (replicate (length es - 1) ','))  es
  HSE.TupleSection{} -> todo expr
  HSE.List _ xs -> list (map hseToExpr xs)
  HSE.Paren _ e -> hseToExpr e
  HSE.LeftSection _ l op -> Var Inf (snd (opString op)) `App` hseToExpr l
  HSE.RightSection _ op r -> flip' `App` Var Inf (snd (opString op)) `App` hseToExpr r
  HSE.RecConstr{} -> todo expr
  HSE.RecUpdate{} -> todo expr
  HSE.EnumFrom _ x -> apps (Var Pref "enumFrom") [x]
  HSE.EnumFromTo _ x y -> apps (Var Pref "enumFromTo") [x,y]
  HSE.EnumFromThen _ x y -> apps (Var Pref "enumFromThen") [x,y]
  HSE.EnumFromThenTo _ x y z -> apps (Var Pref "enumFromThenTo") [x,y,z]
  _ -> todo expr

apps :: Expr -> [HSE.Exp a] -> Expr
apps f xs = foldl (\a x -> a `App` hseToExpr x) f xs 

hseToDecl :: HSE.Decl a -> Decl
hseToDecl dec = case dec of
  HSE.PatBind _ (HSE.PVar _ n) (HSE.UnGuardedRhs _ e) Nothing ->
    Define (snd (nameString n)) (hseToExpr e)
  HSE.FunBind _ [HSE.Match _ n ps (HSE.UnGuardedRhs _ e) Nothing] ->
    Define (snd (nameString n)) (foldr (\p x -> Lambda (hseToPattern p) x) (hseToExpr e) ps)
  _ -> todo dec

hseToPattern :: HSE.Pat a -> Pattern
hseToPattern pat = case pat of
  HSE.PVar _ n -> PVar (snd (nameString n))
  HSE.PInfixApp _ l (HSE.Special _ (HSE.Cons _)) r -> PCons (hseToPattern l) (hseToPattern r)
  HSE.PTuple _ HSE.Boxed [p,q] -> PTuple (hseToPattern p) (hseToPattern q)
  HSE.PParen _ p -> hseToPattern p
  HSE.PWildCard _ -> PVar "_"
  _ -> todo pat

parseMode :: HSE.ParseMode
parseMode =
  HSE.defaultParseMode{
      HSE.extensions = [HSE.EnableExtension HSE.UnicodeSyntax]
    }

parsePF :: String -> Either String TopLevel
parsePF inp = case HSE.parseExpWithMode parseMode inp of
  HSE.ParseOk e -> Right (TLE (hseToExpr e))
  HSE.ParseFailed _ _ -> case HSE.parseDecl inp of
    HSE.ParseOk d -> Right (TLD True (hseToDecl d))
    HSE.ParseFailed _ err -> Left err
