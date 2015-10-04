{-# LANGUAGE Rank2Types, OverloadedStrings, DeriveGeneric, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Anchors
    ( Code(..), onCode
    , Revision(..), onRevision
    , Pane, makePane
    , CodeProps, RevisionProps
    , assocNameRef
    , assocScopeRef
    , PresentationMode(..)
    , assocPresentationMode
    , assocTagOrder
    , ParamList
    , assocFieldParamList
    , BinderParamScopeId(..), bParamScopeId
    ) where

import qualified Control.Lens as Lens
import           Control.MonadA (MonadA)
import           Data.Binary (Binary)
import           Data.ByteString.Char8 ()
import           Data.Store.Rev.Branch (Branch)
import           Data.Store.Rev.Version (Version)
import           Data.Store.Rev.View (View)
import           Data.Store.Transaction (MkProperty(..))
import qualified Data.Store.Transaction as Transaction
import           GHC.Generics (Generic)
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import           Lamdu.Eval.Val (ScopeId)
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId

type Pane m = DefI m

data Code f m = Code
    { repl :: f (ValI m)
    , panes :: f [Pane m]
    , globals :: f [DefI m]
    , preJumps :: f [WidgetId.Id]
    , preCursor :: f WidgetId.Id
    , postCursor :: f WidgetId.Id
    , tags :: f [T.Tag]
    , tids :: f [T.NominalId]
    }
onCode :: (forall a. Binary a => f a -> g a) -> Code f m -> Code g m
onCode f (Code x0 x1 x2 x3 x4 x5 x6 x7) =
    Code (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)

data Revision f m = Revision
    { branches :: f [Branch m]
    , currentBranch :: f (Branch m)
    , cursor :: f WidgetId.Id
    , redos :: f [Version m]
    , view :: f (View m)
    }
onRevision :: (forall a. Binary a => f a -> g a) -> Revision f m -> Revision g m
onRevision f (Revision x0 x1 x2 x3 x4) =
    Revision (f x0) (f x1) (f x2) (f x3) (f x4)

newtype BinderParamScopeId = BinderParamScopeId
    { _bParamScopeId :: ScopeId
    } deriving (Eq, Ord, Binary)

type CodeProps m = Code (MkProperty m) m
type RevisionProps m = Revision (MkProperty m) m

makePane :: DefI m -> Pane m
makePane = id

assocNameRef :: (UniqueId.ToGuid a, MonadA m) => a -> MkProperty m String
assocNameRef = Transaction.assocDataRefDef "" "Name" . UniqueId.toGuid

assocScopeRef ::
    (UniqueId.ToGuid a, MonadA m) => a -> MkProperty m (Maybe BinderParamScopeId)
assocScopeRef = Transaction.assocDataRef "ScopeId" . UniqueId.toGuid

assocTagOrder :: MonadA m => T.Tag -> MkProperty m Int
assocTagOrder = Transaction.assocDataRefDef 0 "Order" . UniqueId.toGuid

type ParamList = [T.Tag]

assocFieldParamList ::
    MonadA m => ValI m -> Transaction.MkProperty m (Maybe ParamList)
assocFieldParamList lambdaI =
    Transaction.assocDataRef "field param list" $ UniqueId.toGuid lambdaI

data PresentationMode = OO | Verbose | Infix Int
    deriving (Eq, Ord, Show, Generic)
instance Binary PresentationMode

assocPresentationMode ::
    (UniqueId.ToGuid a, MonadA m) =>
    a -> Transaction.MkProperty m PresentationMode
assocPresentationMode =
    Transaction.assocDataRefDef OO "PresentationMode" . UniqueId.toGuid

Lens.makeLenses ''BinderParamScopeId
