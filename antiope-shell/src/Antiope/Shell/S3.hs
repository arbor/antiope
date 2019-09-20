{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Antiope.Shell.S3
  ( putFile
  ) where

import Antiope.S3.Types          (ETag (ETag), S3Uri)
import Control.Lens
import Control.Monad.Except
import Data.Aeson                ((.:))
import Data.Generics.Product.Any
import Data.Monoid               ((<>))
import Data.Text                 as T (Text, pack, unpack)
import GHC.Generics

import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as T
import qualified System.Exit          as IO
import qualified System.Process       as IO

data PutObjectReply = PutObjectReply
  { versionId :: Text
  , eTag      :: Text
  } deriving (Eq, Show, Generic)

instance J.FromJSON PutObjectReply where
  parseJSON = J.withObject "PutObjectReply" $ \v -> PutObjectReply
        <$> v .: "VersionId"
        <*> v .: "ETag"

-- | Puts file into a specified S3 bucket
putFile :: MonadIO m
  => S3Uri                -- ^ File name on S3
  -> FilePath             -- ^ Source file path
  -> ExceptT Text m ETag  -- ^ Etag when the operation is successful
putFile s3Uri filePath = do
  (exitCode, stdout, _) <- liftIO $ IO.readProcessWithExitCode "aws"
    [ "s3api"
    , "put-object"
    , "--bucket"
    , T.unpack $ s3Uri ^. the @"bucket" . the @1
    , "--key"
    , T.unpack $ s3Uri ^. the @"objectKey" . the @1
    , "--body"
    , filePath
    ]
    ""

  case exitCode of
    IO.ExitSuccess   -> do
      let stdoutText = T.pack stdout
      let bs = LBS.fromStrict (T.encodeUtf8 stdoutText)
      let repResult = J.eitherDecode bs :: Either String PutObjectReply
      case repResult of
        Right rep -> return (ETag (T.encodeUtf8 (rep ^. the @"eTag")))
        Left msg  -> throwError $ "Command failed to return expected metadata: " <> T.pack msg <> " given stdout: " <> stdoutText
    IO.ExitFailure n -> throwError $ "Command failed with exit code: " <> T.pack (show n)
