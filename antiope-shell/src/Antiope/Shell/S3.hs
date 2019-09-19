{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Antiope.Shell.S3
  ( putFile
  ) where

import Antiope.S3.Types      (S3Uri)
import Control.Monad.Except
import Data.Monoid           ((<>))
import Data.Text             as T (Text, pack, unpack)
import Network.AWS.Data.Text (toText)

import qualified System.Exit    as IO
import qualified System.Process as IO

-- | Puts file into a specified S3 bucket
putFile :: MonadIO m
  => S3Uri        -- ^ File name on S3
  -> FilePath         -- ^ Source file path
  -> ExceptT Text m ()   -- ^ Etag when the operation is successful
putFile s3Uri filePath = do
  exitCode <- liftIO $ IO.rawSystem "aws" ["s3", "cp", "--quiet", filePath, T.unpack (toText s3Uri)]
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throwError $ "Command failed with exit code: " <> T.pack (show n)
