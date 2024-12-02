-- | Key-value database.
module KVDB
  ( KVDB,
    startKVDB,
    kvGet,
    kvPut,
  )
where

import Control.Monad (forM_)
import GenServer

-- | A reference to a KVDB instance that stores keys of type 'k' and
-- corresponding values of type 'v'.

-- TODO
data KVDB k v = KVDB (Server (KVDBMsg k v))

data KVDBMsg k v
  = MsgGet k (ReplyChan v)
  | MsgPut k v

data KVDBState k v = KVDBState 
  {
    kvList :: [(k, v)]
  , waiters :: [(k, ReplyChan v)] 
  }

initialState :: KVDBState k v
initialState = KVDBState 
  {
    kvList = []
  , waiters = []
  }

updateKV :: (Ord k) => k -> v -> [(k, v)] -> [(k, v)]
updateKV key value [] = [(key, value)]
updateKV key value ((k, v):rest)
  | key == k = (key, value) : rest
  | otherwise = (k, v) : updateKV key value rest

getWaiters :: (Ord k) => k -> [(k, ReplyChan v)] -> [ReplyChan v]
getWaiters key = map snd . filter ((== key) . fst)

removeWaiters :: (Ord k) => k -> [(k, ReplyChan v)] -> [(k, ReplyChan v)]
removeWaiters key = filter ((/= key) . fst)

handleGet :: (Ord k) => k -> ReplyChan v -> KVDBState k v -> IO (KVDBState k v)
handleGet key rsvp state = 
  case lookup key (kvList state) of
    Just value -> do
      reply rsvp value
      pure state
    Nothing -> do
      pure state { waiters = (key, rsvp) : waiters state }

handlePut :: (Ord k) => k -> v -> KVDBState k v -> IO (KVDBState k v)
handlePut key value state = 
  let keyWaiters = getWaiters key (waiters state)
  in do
    forM_ keyWaiters $ \rsvp -> reply rsvp value
    pure state { kvList = updateKV key value (kvList state)
               , waiters = removeWaiters key (waiters state)
               }

handleMsg :: (Ord k) => Chan (KVDBMsg k v) -> KVDBState k v -> IO (KVDBState k v)
handleMsg chan state = do
  msg <- receive chan
  case msg of
    MsgGet key rsvp ->
      handleGet key rsvp state
    MsgPut key value -> 
      handlePut key value state

-- | Start a new KVDB instance.
startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB = do
  server <- spawn $ \c -> do
    let loop state = do
          newState <- handleMsg c state
          loop newState
    loop initialState
  pure $ KVDB server

-- | Retrieve the value corresponding to a given key. If that key does
-- not exist in the store, then this function blocks until another
-- thread writes the desired key with 'kvPut', after which this
-- function returns the now available value.
kvGet :: KVDB k v -> k -> IO v
kvGet (KVDB server) key = requestReply server $ MsgGet key

-- | Write a key-value mapping to the database. Replaces any prior
-- mapping of the key.
kvPut :: KVDB k v -> k -> v -> IO ()
kvPut (KVDB server) key value = sendTo server $ MsgPut key value
