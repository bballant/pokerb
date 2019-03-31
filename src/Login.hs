{-# LANGUAGE OverloadedStrings #-}

module Login where

import Data.Text
import Data.Text.IO as T
import Data.Map as Map
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

userLogin :: ExceptT LoginError IO Text
userLogin = do
  token    <- getToken
  userpw   <- maybe (throwE NoSuchUser) return (Map.lookup token users)
  password <- lift (T.putStrLn "Enter your password:" >> T.getLine)
  if userpw == password
     then return token
     else throwE WrongPassword 

getToken :: ExceptT LoginError IO Text
getToken = do
  lift (T.putStrLn "Enter email address:")
  input <- lift T.getLine
  ExceptT . return . getDomain $ input

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail
