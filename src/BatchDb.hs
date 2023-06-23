{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module BatchDb where

import Data.Aeson
import Data.Int
import Data.Text (Text, unpack)
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.SQLite.Simple
import Lightning.Hooks

data BatchDb f = BatchDb
  { _batches :: f (TableEntity BatchT)
  }
  deriving (Generic, Database Sqlite)

data BatchT f = Batch
  { _nodeId  :: Columnar f Int64
  , _amount  :: Columnar f (Maybe Int64)
  , _avoid   :: Columnar f Bool
  }
  deriving (Generic, Beamable)

type Batch = BatchT Identity
type BatchId = PrimaryKey BatchT Identity

instance Table BatchT where
  data PrimaryKey BatchT f = BatchId (Columnar f Int64) deriving (Generic, Beamable)
  primaryKey = BatchId . _nodeId

instance ToJSON (BatchT Identity)
instance Eq (BatchT Identity)
instance Show (BatchT Identity)

createBatchDb :: DatabaseSettings be BatchDb
createBatchDb = defaultDbSettings
database :: CheckedDatabaseSettings Sqlite BatchDb
database = defaultMigratableDbSettings

getDb :: Init -> Text -> IO Connection
getDb (Init _ (InitConfig {lightning5dir})) m = do 
    let file = unpack $ lightning5dir <> "/" <> m <> ".sqlite3"
    conn <- liftIO . open $ file 
    liftIO . createDb $ conn 
    pure conn

createDb :: Connection -> IO () 
createDb conn = runBeamSqlite conn $ do 
   veri <- verifySchema migrationBackend database 
   _ <- checkSchema migrationBackend database mempty
   case veri of 
       VerificationFailed _ -> autoMigrate migrationBackend database
       VerificationSucceeded -> pure () 

insertBatch :: Connection -> Batch -> IO ()
insertBatch conn batch = runBeamSqlite conn $
  runInsert $ insert (_batches createBatchDb) $ insertValues [batch]

lookupBatch :: Connection -> Int64 -> IO [Batch]
lookupBatch conn nodeId = runBeamSqlite conn $
  runSelectReturningList $
  lookup_ (_batches createBatchDb) (BatchId nodeId)

lookupBatches :: Connection -> IO [Batch]
lookupBatches conn = runBeamSqlite conn $
  runSelectReturningList . select $ all_ (_batches createBatchDb)

updateBatchAmount :: Connection -> Batch -> Maybe Int64 -> IO ()
updateBatchAmount conn batch newAmount = runBeamSqlite conn $
  runUpdate $ save (_batches createBatchDb) (batch { _amount = newAmount })

updateBatchAvoid :: Connection -> Batch -> Bool -> IO ()
updateBatchAvoid conn batch newAvoid = runBeamSqlite conn $
  runUpdate $ save (_batches createBatchDb) (batch { _avoid = newAvoid })

deleteBatch :: Connection -> Int64 -> IO ()
deleteBatch conn nodeId = runBeamSqlite conn $
  runDelete $ delete (_batches createBatchDb) (\b -> _nodeId b ==. val_ nodeId)
