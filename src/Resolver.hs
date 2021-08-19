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

type Resolve = Reader [String]

resolve :: UProgram -> UProgram
resolve (Program decls) = Program $ runReader (mapM resolveDecl decls) []

fixName :: QualifiedName -> Resolve QualifiedName
fixName (Qualified ns name) = do
    namespace <- ask
    return $ Qualified (namespace ++ ns) name

resolveConstructor :: (QualifiedName, [Type]) -> Resolve (QualifiedName, [Type])
resolveConstructor (name, ts) = (,ts) <$> fixName name

resolveDecl :: UDecl -> Resolve UDecl
resolveDecl = \case
    DVar m t name e -> DVar m t <$> fixName name <*> resolveExpr e
    DNamespace n ds -> DNamespace n <$> local (++ [n]) (mapM resolveDecl ds)
    DData con tvs vcs -> do
        con' <- fixName con
        vcs' <- mapM resolveConstructor vcs
        return $ DData con' tvs vcs'
    DStmt s -> DStmt <$> resolveStmt s
    a@(DImport _) -> return a

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
        bs' <- mapM (\(p, pe) -> (p,) <$> resolveExpr pe) bs
        return $ EMatch t e' bs'
    EBlock t ds -> EBlock t <$> mapM resolveDecl ds
    EBinary t o l r -> do
        l' <- resolveExpr l
        r' <- resolveExpr r
        return $ EBinary t o l' r'
    EUnary t o e -> EUnary t o <$> resolveExpr e
    EAssign t id e -> EAssign t <$> fixName id <*> resolveExpr e
    ECall t f a -> ECall t <$> resolveExpr f <*> resolveExpr a
    a@(ELit _ _) -> return a
