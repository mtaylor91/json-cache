module JSONCache.State
  ( State(..)
  , StateNode(..)
  , TypeError(..)
  , initState
  , getAncestor
  , getChild
  , getValue
  , putDescendant
  , putValue
  , patchValue
  , deleteValue
  ) where

import Control.Concurrent.STM
import Control.Exception (Exception, throw)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value(..))
import qualified Data.Text as T
import qualified Data.Vector as V


data StateNode
  = StateNodeNull
  | StateNodeBool Bool
  | StateNodeInt Int
  | StateNodeString String
  | StateNodeList (V.Vector State)
  | StateNodeMap (KM.KeyMap State)


newtype TypeError = TypeError String deriving Show


instance Exception TypeError


newtype State = State (TVar StateNode)


initState :: STM State
initState = do
  var <- newTVar StateNodeNull
  pure $ State var


getAncestor :: State -> [T.Text] -> STM (State, [T.Text])
getAncestor (State var) key = do
  node <- readTVar var
  case key of
    [] -> pure (State var, [])
    k:ks ->
      case node of
        StateNodeMap m ->
          let k' = fromText k
           in case KM.lookup k' m of
            Just child -> do
              (ancestor, ks') <- getAncestor child ks
              pure (ancestor, ks')
            Nothing ->
              pure (State var, key)
        _ ->
          pure (State var, key)


getChild :: State -> [T.Text] -> STM State
getChild (State var) key = do
  node <- readTVar var
  case key of
    [] -> pure $ State var
    k:ks ->
      case node of
        StateNodeMap m ->
          let k' = fromText k
           in case KM.lookup k' m of
            Just child -> getChild child ks
            Nothing -> do
              child <- State <$> newTVar StateNodeNull
              writeTVar var $ StateNodeMap $ KM.insert k' child m
              getChild child ks
        _ -> throw $ TypeError "Cannot get child of non-map"


getValue :: State -> STM Value
getValue (State var) = do
  node <- readTVar var
  case node of
    StateNodeNull -> pure Null
    StateNodeBool b -> pure $ Bool b
    StateNodeInt i -> pure $ Number $ fromIntegral i
    StateNodeString s -> pure $ String $ T.pack s
    StateNodeList l -> Array <$> mapM getValue l
    StateNodeMap m -> Object <$> mapM getValue m


putDescendant :: State -> [T.Text] -> Value -> STM ()
putDescendant (State var) key value = do
  node <- readTVar var
  case key of
    [] -> putValue (State var) value
    k:ks ->
      case node of
        StateNodeMap m ->
          let k' = fromText k
           in case KM.lookup k' m of
            Just child -> putDescendant child ks value
            Nothing -> do
              child <- initState
              putDescendant child ks value
              writeTVar var $ StateNodeMap $ KM.insert k' child m
        _ ->
          putDescendantMaps k ks $ State var
  where
    putDescendantMaps :: T.Text -> [T.Text] -> State -> STM ()
    putDescendantMaps k [] (State var') = do
      child <- State <$> newTVar (StateNodeMap KM.empty)
      putValue child value
      writeTVar var' $ StateNodeMap $
        KM.singleton (fromText k) child
    putDescendantMaps k (kn:ks) (State var') = do
      child <- State <$> newTVar (StateNodeMap KM.empty)
      putDescendantMaps kn ks child
      writeTVar var' $ StateNodeMap $
        KM.singleton (fromText k) child


putValue :: State -> Value -> STM ()
putValue (State var) value = do
  case value of
    Null -> writeTVar var StateNodeNull
    Bool b -> writeTVar var $ StateNodeBool b
    Number n -> writeTVar var $ StateNodeInt $ floor n
    String s -> writeTVar var $ StateNodeString $ T.unpack s
    Array a -> do
      l <- V.mapM (\v -> do
        child <- initState
        putValue child v
        pure child) a
      writeTVar var $ StateNodeList l
    Object o -> do
      m <- mapM (\v -> do
        child <- initState
        putValue child v
        pure child) o
      writeTVar var $ StateNodeMap m


patchValue :: State -> Value -> STM ()
patchValue (State var) value = do
  case value of
    Null -> writeTVar var StateNodeNull
    Bool b -> writeTVar var $ StateNodeBool b
    Number n -> writeTVar var $ StateNodeInt $ floor n
    String s -> writeTVar var $ StateNodeString $ T.unpack s
    Array a -> do
      l <- V.mapM (\v -> do
        child <- initState
        putValue child v
        pure child) a
      writeTVar var $ StateNodeList l
    Object o -> do
      s <- readTVar var
      case s of
        StateNodeMap m -> do
          m' <- mapM (\v -> do
            child <- initState
            putValue child v
            pure child) o
          writeTVar var $ StateNodeMap $ KM.union m' m
        _ -> do
          m <- mapM (\v -> do
            child <- initState
            putValue child v
            pure child) o
          writeTVar var $ StateNodeMap m


deleteValue :: State -> [T.Text] -> STM ()
deleteValue (State var) [] = do
  writeTVar var StateNodeNull
deleteValue (State var) [key] = do
  node <- readTVar var
  case node of
    StateNodeMap m -> do
      let k = fromText key
      case KM.lookup k m of
        Just _ -> do
          writeTVar var $ StateNodeMap $ KM.delete k m
        Nothing -> pure ()
    _ -> pure ()
deleteValue (State var) (key:keys) = do
  node <- readTVar var
  case node of
    StateNodeMap m -> do
      let k = fromText key
      case KM.lookup k m of
        Just child -> deleteValue child keys
        Nothing -> pure ()
    _ -> pure ()
