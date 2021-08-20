{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Resolver (resolve) where

import Control.Monad.Reader
import Control.Monad.Except

import Name
import AST
import Type

-- The resolver pass fully qualifies all names,
-- i.e, turns something like ABC.Def in the namespace "XYZ"
-- to XYZ.ABC.Def

type Resolve = Reader Namespace

resolve :: UProgram -> UProgram
resolve (Program decls) = Program $ runReader (traverse resolveDecl decls) Global

fixName :: QualifiedName -> Resolve QualifiedName
fixName (Qualified ns name) = do
    namespace <- ask
    return $ Qualified (namespace `combine` ns) name

combine :: Namespace -> Namespace -> Namespace
combine ns (Relative Global n) = Relative ns n
combine ns (Relative ns' n) = Relative (combine ns ns') n
combine ns Global = ns

resolveConstructor :: (QualifiedName, [Type]) -> Resolve (QualifiedName, [Type])
resolveConstructor (name, ts) = (,ts) <$> fixName name

resolveDecl :: UDecl -> Resolve UDecl
resolveDecl = \case
    DVar m t name e -> do
        t' <- case t of 
            Just ann -> Just <$> resolveType ann
            Nothing -> return Nothing
        DVar m t' <$> fixName name <*> resolveExpr e
    DNamespace n ds i -> flip (DNamespace n) i <$> local (`Relative` n) (traverse resolveDecl ds)
    DData con tvs vcs -> do
        con' <- fixName con
        vcs' <- traverse resolveConstructor vcs
        return $ DData con' tvs vcs'
    DStmt s -> DStmt <$> resolveStmt s

resolveStmt :: UStmt -> Resolve UStmt
resolveStmt = \case
    SExpr e -> SExpr <$> resolveExpr e
    SPass e -> SPass <$> resolveExpr e

resolveExpr :: UExpr -> Resolve UExpr
resolveExpr = \case
    EIdent t name -> EIdent t <$> fixName name
    EFunc t p e -> EFunc t p <$> resolveExpr e
    EIf t c a b -> do
        c' <- resolveExpr c
        a' <- resolveExpr a
        b' <- resolveExpr b
        return $ EIf t c' a' b'
    EMatch t e bs -> do
        e' <- resolveExpr e
        bs' <- traverse (\(p, pe) -> (,) <$> resolvePattern p <*> resolveExpr pe) bs
        return $ EMatch t e' bs'
    EBlock t ds -> EBlock t <$> traverse resolveDecl ds
    EBinary t o l r -> do
        l' <- resolveExpr l
        r' <- resolveExpr r
        return $ EBinary t o l' r'
    EUnary t o e -> EUnary t o <$> resolveExpr e
    EAssign t id e -> EAssign t <$> fixName id <*> resolveExpr e
    ECall t f a -> ECall t <$> resolveExpr f <*> resolveExpr a
    a@(ELit _ _) -> return a

resolvePattern :: Pattern -> Resolve Pattern
resolvePattern (PCon name ps) = PCon <$> fixName name <*> traverse resolvePattern ps
resolvePattern (PLit l) = return $ PLit l
resolvePattern (PVar v) = return $ PVar v

resolveType :: Type -> Resolve Type
resolveType (TCon name ts) = TCon <$> fixName name <*> traverse resolveType ts
resolveType (TVar tv) = return $ TVar tv
