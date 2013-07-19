module Lamdu.Data.Infer.Unify
  ( unify
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>))
import Control.Lens.Operators
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map.Utils (lookupOrSelf)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM

-- If we don't assert that the scopes have same refs we could be pure
intersectScopes :: Scope -> Scope -> Infer def Scope
intersectScopes (Scope aScope) (Scope bScope) =
  Scope <$> sequenceA (Map.intersectionWith verifyEquiv aScope bScope)
  where
    -- Expensive assertion
    verifyEquiv aref bref = do
      equiv <- ExprRefs.equiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

newtype HoleConstraints = HoleConstraints
  { _hcUnusableInHoleScope :: Set Guid
  }

-- You must apply this recursively
applyHoleConstraints ::
  HoleConstraints -> RefData def -> Either (Error def) (RefData def)
applyHoleConstraints (HoleConstraints unusableSet) refData
  | Lens.anyOf ExprLens.bodyParameterRef (`Set.member` unusableSet) body =
    Left VarEscapesScope
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Set.member` unusableSet) body =
    error "applyHoleConstraints: Shadowing detected"
  | otherwise =
    return $ refData & rdScope . scopeMap %~ (`Map.difference` unusableMap)
  where
    body = refData ^. rdBody
    unusableMap = Map.fromSet (const ()) unusableSet

data UnifyPhase
  = UnifyHoleConstraints HoleConstraints
  | UnifyRef Ref

mergeBodies ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid ->
  Scope -> Expr.Body def Ref ->
  Scope -> Expr.Body def Ref ->
  Infer def (Expr.Body def Ref)
mergeBodies recurse renames xScope xBody yScope yBody =
  case (xBody, yBody) of
  (_, Expr.BodyLeaf Expr.Hole) -> unifyWithHole renames   yScope xScope xBody
  (Expr.BodyLeaf Expr.Hole, _) -> unifyWithHole Map.empty xScope yScope yBody
  _ ->
    case sequenceA <$> ExprUtil.matchBody matchLamResult matchOther (==) xBody yBody of
    Nothing -> InferM.error $ Mismatch xBody yBody
    Just mkBody -> mkBody
  where
    unifyWithHole activeRenames holeScope otherScope nonHoleBody =
      maybeRecurseHoleConstraints activeRenames nonHoleBody $
      makeUnusableScopeSet holeScope otherScope
    maybeRecurseHoleConstraints activeRenames nonHoleBody unusableScopeSet =
      nonHoleBody
      & Lens.traverse %%~
        flip (recurse activeRenames)
        (UnifyHoleConstraints (HoleConstraints unusableScopeSet))
    makeUnusableScopeSet holeScope otherScope =
      Map.keysSet $ Map.difference
      (otherScope ^. scopeMap)
      (holeScope ^. scopeMap)
    matchLamResult xGuid yGuid xRef yRef =
      (yGuid, recurse (renames & Lens.at xGuid .~ Just yGuid) xRef (UnifyRef yRef))
    matchOther xRef yRef = recurse renames xRef (UnifyRef yRef)

renameAppliedPiResult :: Map Guid Guid -> AppliedPiResult -> AppliedPiResult
renameAppliedPiResult renames (AppliedPiResult piGuid argVal destRef copiedNames) =
  AppliedPiResult
  (lookupOrSelf renames piGuid) argVal destRef
  (Map.mapKeys (lookupOrSelf renames) copiedNames)

-- No names in Relation (yet?)
renameRelations :: Map Guid Guid -> Set Relation -> Set Relation
renameRelations _ = id

renameRefData :: Map Guid Guid -> RefData def -> RefData def
renameRefData renames (RefData scope substs renameHistory relations body)
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Map.member` renames) body =
    error "Shadowing encountered, what to do?"
  | otherwise =
    RefData
    (scope & scopeMap %~ Map.mapKeys (lookupOrSelf renames))
    (substs <&> renameAppliedPiResult renames)
    (renameHistory & _RenameHistory %~ Map.union renames)
    (relations & renameRelations renames)
    (body & ExprLens.bodyParameterRef %~ lookupOrSelf renames)

mergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def ([AppliedPiResult], RefData def)
mergeRefData recurse renames
  (RefData aScope aAppliedPiResults aMRenameHistory aRelations aBody)
  (RefData bScope bAppliedPiResults bMRenameHistory bRelations bBody) =
  mkRefData
  <$> intersectScopes aScope bScope
  <*> mergeBodies recurse renames aScope aBody bScope bBody
  where
    newInfoOnSelf =
      Lens.has ExprLens.bodyHole aBody /=
      Lens.has ExprLens.bodyHole bBody
    substsToExecute
      | newInfoOnSelf = mergedAppliedPiResults
      | otherwise = []
    mergedAppliedPiResults = aAppliedPiResults ++ bAppliedPiResults
    mkRefData intersectedScope mergedBody =
      (,) substsToExecute $
      RefData
      { _rdScope = intersectedScope
      , _rdAppliedPiResults = mergedAppliedPiResults
      , _rdRenameHistory = mappend aMRenameHistory bMRenameHistory
      , _rdRelations = mappend aRelations bRelations
      , _rdBody = mergedBody
      }

renameMergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Ref -> Map Guid Guid -> RefData def -> RefData def ->
  Infer def ()
renameMergeRefData recurse rep renames a b = do
  (appliedPiResults, mergedRefData) <-
    mergeRefData recurse renames (renameRefData renames a) b
  -- First let's write the mergedRefData so we're not in danger zone
  -- of reading missing data:
  ExprRefs.write rep mergedRefData
  -- Now we can safely run the relations
  traverse_ (InferM.substOrUnify rep) appliedPiResults


unifyRecurse :: Eq def => Set Ref -> Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref
unifyRecurse visited renames rawNode phase = do
  nodeRep <- ExprRefs.find "unifyRecurse:rawNode" rawNode
  if visited ^. Lens.contains nodeRep
    then InferM.error $ InfiniteType nodeRep
    else
    case phase of
    UnifyHoleConstraints holeConstraints -> do
      rawNodeData <- ExprRefs.readRep nodeRep
      nodeData <-
        InferM.liftError . applyHoleConstraints holeConstraints $
        renameRefData renames rawNodeData
      ExprRefs.writeRep nodeRep nodeData
      traverse_
        (flip (recurse nodeRep renames)
         (UnifyHoleConstraints holeConstraints)) $
        nodeData ^. rdBody
      return nodeRep
    UnifyRef other -> do
      (rep, unifyResult) <- ExprRefs.unifyRefs nodeRep other
      case unifyResult of
        ExprRefs.UnifyRefsAlreadyUnified -> return ()
        ExprRefs.UnifyRefsUnified xData yData -> merge rep xData yData
      return rep
  where
    recurse visitedRef = unifyRecurse (visited & Lens.contains visitedRef .~ True)
    merge rep = renameMergeRefData (recurse rep) rep renames

unify :: Eq def => Ref -> Ref -> Infer def Ref
unify x y = unifyRecurse mempty mempty x (UnifyRef y)
