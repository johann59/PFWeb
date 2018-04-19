{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Clay as C
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import GHC.Generics (Generic)
import Lucid
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (middleware, scotty, get, json, html, param, ActionM)

liCss = C.li C.# C.byClass "liCss" C.? do
  C.border           C.solid (C.px 1) C.black
  C.width            (C.px 100)
  C.listStyleType    C.none

dbName :: String
dbName = "mytracker.db"

data Tracker = Tracker 
  { site :: T.Text
  , page :: T.Text
  , hits :: Int
  }
instance FromRow Tracker where
  fromRow = Tracker <$> field <*> field <*> field

--instance ToJSON Tracker where
--  toJSON (Tracker site page hits) = object ["site" .= site, "page" .= page, "hits" .= hits]
 
selectAll :: IO [Tracker]
selectAll = do
  conn <- SQL.open dbName
  let selectReq = "SELECT * FROM tracker"
  res <- SQL.query_ conn selectReq :: IO [Tracker]
  SQL.close conn
  return res

mkpage :: Lucid.Html () -> Lucid.Html () -> L.Text
mkpage titleStr page = renderText $ do
  doctype_
  html_ $ do
    header_ $ do
      title_ titleStr
      header_ $ style_ $ L.toStrict $ C.render liCss
    body_ page

homeRoute :: [Tracker] -> Lucid.Html ()
homeRoute trackers = do
  h1_ "My tracker"

  a_ [href_ "/json"] $ "Get JSON"
  
  ul_ $ mapM_ (li_ [class_ "liCss"] . toHtml . formatTracker) trackers
      where formatTracker tracker = T.concat [site tracker , " ", page tracker, " ", T.pack (show (hits tracker))]
            
  --table_ (mapM_ row trackers)
  --    where row tracker = tr_ (do td_ (site tracker)
  --                                td_ (page tracker)
  --                                td_ (T.pack (show (hits tracker))))

--exportJSON :: [Tracker]
--exportJSON trackers = do
--  writeFile "export.json" (Data.Aeson.encode trackers)

--renderTracker :: T.Text -> ActionM ()

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware simpleCors

  get "/" $ do
     trackers <- liftIO selectAll
     html $ mkpage "My tracker" $ homeRoute trackers

  --get "/tracker" $ do
  --   html $ mkpage "My tracker" $  renderTracker
  
  --get "/json" $ do
  --   trackers <- liftIO selectAll
  --   html $ mkpage "JSON export" $ exportJSON trackers

