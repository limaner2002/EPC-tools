-- for https://github.com/haskell-servant/servant/issues/117
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Redirect where

import Control.Monad.Trans.Except
import Data.String
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Server.Internal
import Servant.Server.Internal.ServantErr

class KnownMethod (c :: [*] -> * -> *) where
  methodOf :: Proxy c -> Method

instance KnownMethod Delete where
  methodOf _ = methodDelete

instance KnownMethod Get where
  methodOf _ = methodGet

instance KnownMethod Patch where
  methodOf _ = methodPatch

instance KnownMethod Post where
  methodOf _ = methodPost

instance KnownMethod Put where
  methodOf _ = methodPut

data Redirect (c :: [*] -> * -> *) lnk api

-- instance (KnownMethod c, IsElem lnk api, HasLink lnk)
-- 	    => HasServer (Redirect c lnk api) context where

--   type ServerT (Redirect c lnk api) m
--     = m URI

--   route Proxy getLink req = _

  -- route Proxy getLink req respond
  --   | null (pathInfo req) && requestMethod req == methodOf pc = do
  --       res <- runEitherT getLink
  --       case res of
  --         Left err  -> respond . succeedWith $ responseServantErr err
  --         Right lnk -> respond . succeedWith $
  --           responseLBS seeOther303 [("Location", fromString ("/" ++ show lnk))] "" 
  --   | null (pathInfo req) && requestMethod req /= methodOf pc =
  --       respond $ failWith WrongMethod 
  --   | otherwise = respond $ failWith NotFound

  --   where pc = Proxy :: Proxy c
