module Lamdu.Sugar.PresentationModes
    ( addToDef, addToExpr
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

indirectDefinitionGuid :: ExpressionU m pl -> Maybe Guid
indirectDefinitionGuid funcS =
    case funcS ^. Sugar.rBody of
    -- TODO: This is incorrect because BodyGetVar is used even when it's
    -- a GetField behind the scenes, and we probably don't want to
    -- associate the Guid of the tag here? Need to throw this Guid or
    -- associated data into the GetVar/GetField itself anyway!
    Sugar.BodyGetVar (Sugar.GetVarNamed n) -> Just $ n ^. Sugar.nvName
    -- TODO: <-- do we want to make something up for get-fields, etc?
    _ -> Nothing

indirectDefinitionPresentationMode ::
    MonadA m => ExpressionU m pl -> T m (Maybe Sugar.PresentationMode)
indirectDefinitionPresentationMode =
    Lens.traverse (Transaction.getP . Anchors.assocPresentationMode) .
    indirectDefinitionGuid

addToApply ::
    MonadA m =>
    Sugar.Apply name (ExpressionU m pl) ->
    T m (Sugar.Apply name (ExpressionU m pl))
addToApply a =
    case a ^. Sugar.aSpecialArgs of
    Sugar.NoSpecialArgs ->
        do
            presentationMode <-
                a ^. Sugar.aFunc & indirectDefinitionPresentationMode
            let (specialArgs, annotatedArgs) =
                    case (presentationMode, a ^. Sugar.aAnnotatedArgs) of
                    (Just (Sugar.Infix prec), a0:a1:as) ->
                        ( Sugar.InfixArgs prec
                          (a0 ^. Sugar.aaExpr) (a1 ^. Sugar.aaExpr)
                        , as
                        )
                    (Just Sugar.OO, a0:as) ->
                        (Sugar.ObjectArg (a0 ^. Sugar.aaExpr), as)
                    (_, args) -> (Sugar.NoSpecialArgs, args)
            a
                & Sugar.aAnnotatedArgs .~ annotatedArgs
                & Sugar.aSpecialArgs .~ specialArgs
                & return
    _ -> return a

addToHoleResult ::
    MonadA m => Sugar.HoleResult Guid m -> T m (Sugar.HoleResult Guid m)
addToHoleResult = Sugar.holeResultConverted %%~ addToExpr

addToHole :: MonadA m => Sugar.Hole Guid m a -> Sugar.Hole Guid m a
addToHole =
    Sugar.holeMActions . Lens._Just . Sugar.holeOptions .
    Lens.mapped . Lens.mapped . Sugar.hoResults . Lens.mapped .
    _2 %~ (>>= addToHoleResult)

addToBody :: MonadA m => BodyU m pl -> T m (BodyU m pl)
addToBody (Sugar.BodyApply a) = addToApply a <&> Sugar.BodyApply
addToBody (Sugar.BodyHole a) = addToHole a & Sugar.BodyHole & return
addToBody b = return b

addToExpr :: MonadA m => ExpressionU m pl -> T m (ExpressionU m pl)
addToExpr e =
    e
    & Sugar.rBody %%~ addToBody
    >>= Sugar.rBody . Lens.traversed %%~ addToExpr

addToBinder ::
    MonadA m =>
    Sugar.Binder Guid m (ExpressionU m pl) ->
    T m (Sugar.Binder Guid m (ExpressionU m pl))
addToBinder b =
    b
    & Sugar.bBody %%~ addToExpr
    >>= Sugar.bLetItems . Lens.traversed . Sugar.liValue %%~ addToBinder

addToDef ::
    MonadA m =>
    Sugar.Definition Guid m (ExpressionU m a) ->
    T m (Sugar.Definition Guid m (ExpressionU m a))
addToDef def =
    def
    & Sugar.drBody . Sugar._DefinitionBodyExpression .
      Sugar.deContent %%~ addToBinder
