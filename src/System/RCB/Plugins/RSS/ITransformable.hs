-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.ITransformable
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.ITransformable
where

class Transformable a where
      transform :: a -> String -> String


