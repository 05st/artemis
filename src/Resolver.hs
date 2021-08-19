{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Resolver (resolve) where

import Control.Monad.Reader
import Control.Monad.Except

import Data.Functor

import Name
import AST
import Type

type Resolve = Reader [String]

resolve :: UProgram -> UProgram
resolve (Program decls) = Program $ runReader (mapM resolveDecl decls) []

fixName :: QualifiedName -> Resolve QualifiedName
fixName (Qualified ns name) = do
    namespace <- ask
    return $ Qualified (namespace ++ ns) name

resolveConstructor :: (QualifiedName, [Type]) -> Resolve (QualifiedName, [Type])
resolveConstructor (name, ts) = fixName name <&> (,ts)

resolveDecl :: UDecl -> Resolve UDecl
resolveDecl = \case
    DVar m t name e -> fixName name <&> \name' -> DVar m t name' e
    DNamespace n ds -> local (++ [n]) (mapM resolveDecl ds) <&> DNamespace n
    DData con tvs vcs -> do
        con' <- fixName con
        vcs' <- mapM resolveConstructor vcs
        return $ DData con' tvs vcs'
    DStmt s -> resolveStmt s <&> DStmt

resolveStmt :: UStmt -> Resolve UStmt
resolveStmt = \case
    SExpr e -> resolveExpr e <&> SExpr
    other -> return other

resolveExpr :: UExpr -> Resolve UExpr
resolveExpr = \case
    EIdent t name -> fixName name <&> EIdent t
    other -> return other
