{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses  #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Octree as Octree
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Function as F
import qualified Data.Set as Set
import qualified Math.Combinatorics.Multiset as MS
import qualified Data.String as String
import qualified Debug.Trace as Trace

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
System
    Id Int sqltype=int sql=SystemId
    name Text sqltype=text sql=SystemName
    x Double sqltype=double sql=SystemX 
    y Double sqltype=double sql=SystemY 
    z Double sqltype=double sql=SystemZ
    size Double Maybe sqltype=double sql=SystemSize
    deriving Show
Commod
    Id Text sqltype=text sql=name
--    name Text sql=Name
    type Text Maybe sql=Type
    avg Double Maybe sqltype=double sql=Avg
    deriving Show
Station sql=SysStation
    Id Int sqltype=int sql=SysStationID
    systemName Text sqltype=text sql=SysStationSystem
    stationName Text sqltype=text sql=SysStationStation
    -- 'SysStationLookup' TEXT COLLATE NOCASE NOT NULL UNIQUE,
    dist Int sqltype=int sql=SysStationDist
    -- 'SysStationDist'  INTEGER,
    -- 'SysStationBM'  TEXT COLLATE NOCASE ,
    maxPad Text Maybe sqltype=text sql=SysStationMaxPad
    -- 'SysStationMaxPad'  TEXT COLLATE NOCASE ,
    padSize Int Maybe sqltype=int sql=SysStationPadSize
    -- 'SysStationFaction'  TEXT COLLATE NOCASE ,
    -- 'SysStationGovernment'  TEXT COLLATE NOCASE ,
    -- 'SysStationLastUpdate'  DATETIME
    deriving Show
SC
    Id Int sqltype=int sql=SCid
    systemName Text sqltype=text sql=SCStationSystem
    stationName Text sqltype=text sql=SCStationName
    stationCommod Text sqltype=text sql=SCStationCommod
    sell Int sqltype=int sql=SCStationSell -- sell to station
    demand Double Maybe sql=SCStationDemand
    price Int sqltype=int sql=SCStationPrice
    stock Double  sql=SCStationStock
    --     'SCStationLocal'  TEXT COLLATE NOCASE ,
-- 'SCStationCommodGroup'  TEXT COLLATE NOCASE ,
-- 'SCStationCommodAvg'  INTEGER,
-- 'SCStationPrice'  INTEGER,
-- 'SCStationSell'  INTEGER,
-- 'SCStationFence'  INTEGER,
-- 'SCStationDemand'  REAL,
-- 'SCStationDemandCode'  TEXT COLLATE NOCASE , -- unused?
-- 'SCStationSupply'  REAL,
-- 'SCStationSupplyCode'  TEXT COLLATE NOCASE , -- unused?
-- 'SCStationStock'  REAL,
-- 'SCStationIllegal'  TEXT COLLATE NOCASE DEFAULT 0,
-- 'SCStationConsumer'  TEXT COLLATE NOCASE ,
-- 'SCStationProducer'  TEXT COLLATE NOCASE ,
-- 'SCStationLastUpdate'  TEXT COLLATE NOCASE ,
-- 'SCStationForSale'  TEXT DEFAULT 1,
-- 'S1'  TEXT COLLATE NOCASE ,
-- 'S2'  TEXT COLLATE NOCASE ,
-- 'S3'  TEXT COLLATE NOCASE ,
-- 'S4'  TEXT COLLATE NOCASE ,
-- 'N1'  DOUBLE,
-- 'N2'  DOUBLE,
-- 'N3'  DOUBLE,
-- 'N4'  DOUBLE,
-- 'B1'  Bit,
-- 'B2'  Bit,
-- 'SCUNIQUE'  TEXT COLLATE NOCASE NOT NULL,
    deriving Show
|]

connStr = "C:\\Program Files (x86)\\Slopeys ED BPC\\ED4.db"

systemPosition:: System -> Octree.Vector3
systemPosition v = Octree.Vector3 (systemX v) (systemY v) (systemZ v)

extractStations systemStations cur = 
    case Map.lookup (T.toLower $ systemName cur) systemStations of
        Nothing -> []
        Just stations -> stations

scKey x = (T.toLower $ sCSystemName x, T.toLower $ sCStationName x)
stationKey x = (T.toLower $ stationSystemName x, T.toLower $ stationStationName x)
goodKey = T.toLower . sCStationCommod
        
instance Eq Station where
    x == y = sys x == sys y && station x == station y
        where
            sys = T.toLower . stationSystemName
            station = T.toLower . stationStationName
    x /= y = sys x /= sys y || station x /= station y
        where
            sys = T.toLower . stationSystemName
            station = T.toLower . stationStationName

extractGoods resSc nearStations = Map.fromList [(scKey $ head scs, Map.fromList [(goodKey g, g) | g <- scs]) | scs <- grouped]
    where
        grouped = List.groupBy ((==) `F.on` scKey) $ List.sortBy (compare `F.on` scKey) [v | Entity k v <- resSc]

-- safeHead [] = []
-- safeHead (x:ys) = 

profit :: SC -> (Maybe SC) -> Int
profit bg maybe_sg = -- Trace.trace (show bg ++ " " ++ show maybe_sg ++ show (effectiveSell maybe_sg - effectivePrice bg) )
    if baseProfit > 10000 then
        0
    else
        if sCStationCommod bg == "Slaves" then
            0
        else
            baseProfit
    where
        baseProfit = effectiveSell maybe_sg - effectivePrice bg
        effectiveSell Nothing = 0
        effectiveSell (Just sc) = sCSell sc
        effectivePrice sc = if sCStock sc < 1000 then 100000 else if sCPrice sc < 1 then 100000 else sCPrice sc
-- buy at price, sell at sell, 
-- must have supply must have demand, > 1000... demand is blank in bpc's db.

reverseSort :: (Ord a) => [(a, a1)] -> [(a, a1)]
reverseSort = List.sortBy inversecompare
    where
        inversecompare (x, _) (y, _) = y `compare` x

bestbuy stationGoods x y =
    case List.length profits of
      0 -> (0, "Nothing")
      x -> sane (head profits)
    where
        sane (profit, item) = if profit > 0 then (profit, item) else (0, "Nothing")
        buys = l x
        sells = l y
        l s = case Map.lookup (stationKey s) stationGoods  of
            Just b -> b
            Nothing -> Map.empty
        profits = reverseSort [ (profit bg $ Map.lookup k sells, k) | (k, bg) <- Map.toList buys]
        
trades :: (String.IsString t, Ord t) => Map.Map (Text, Text) (Map.Map t SC) -> [Station] -> (Int, [((Int, t), (Text, Text))])
trades stationGoods route = ((foldr f 0 buys), buys)
    where
        pairs [] = []
        pairs [x] = []
        pairs (x:y:xz) = (x,y):pairs (y:xz)
        cycle = (List.last route):route
        buys = [(bestbuy stationGoods x y, stationKey x)   | (x, y) <- pairs cycle]
        f ((p, _), _) acc = p + acc

routes xs steps = List.concat [MS.cycles s | s <- MS.kSubsets steps stations]
    where
        stations = MS.fromDistinctList xs

usableStation x = if stationDist x < 4000 then usablePad x else False
    where
        usablePad x = case stationPadSize x of
           Nothing -> True -- who knows
           Just 0 -> True -- in case 0 == any
           Just s -> if s < 3 then False else True

showRoute (profit, xs) = ((fromIntegral profit) / (fromIntegral $ List.length xs), xs)
     

lookupData = runStdoutLoggingT $ withSqlitePool connStr 10 $ \pool ->
     runResourceT $ flip runSqlPool pool $ do
--        printMigration migrateAll
        res_system  :: [Entity System] <- selectList [] [] --, OffsetBy 1] -- LimitTo 1, OffsetBy 1]
        res_commodities  :: [Entity Commod] <- selectList [] [] 
        resSc :: [Entity SC] <- selectList [ SCStationName !=. "ANY"] []
        -- SCStationCommod ==. "Resonating Separators"
        --                                    , SCSystem ==. "Keiadabiko"
        --                                    , SCPrice ==. 5457
        --                                    , SCStationName ==. "Maclean Vision"
        --                                    ] [LimitTo 1]
        resStations :: [Entity Station] <- selectList [StationStationName !=. "ANY"] []
        return (res_system, res_commodities, resSc, resStations)
        -- res  :: [Entity Commod] <- selectList [] [LimitTo 1, OffsetBy 1] 
        --res  :: [Entity SC] <- selectList [SCStationCommod ==. "Resonating Separators"
        --                                   , SCSystem ==. "Keiadabiko"
        --                                   , SCPrice ==. 5457
        --                                  , SCStationName ==. "Maclean Vision"
        --                                 ] [LimitTo 1] 
        -- liftIO $ print res

main :: IO ()
main = do 
    (res_system, res_commodities, resSc, resStations) <- lookupData
    let systems = [v | Entity k v <- res_system] -- [System]
        stations = List.groupBy ((==) `F.on` (T.toLower . stationSystemName)) $ List.sortBy (compare `F.on` (T.toLower . stationSystemName)) $ List.filter usableStation [v | Entity k v <- resStations]
        systemStations = Map.fromList [(T.toLower $ stationSystemName $ head v, v) | v <- stations]
            --groupBy (\(x _) (y _) -> (x == y)) 
        systemNames = Map.fromList [(T.toLower $ systemName v, v) | v <- systems]
            --stationsByName = Map.fromList stations
        systemMap = Octree.fromList [(systemPosition v, v) | v <- systems] -- octree (pos -> system)
        keiadabiko = case Map.lookup "anlave" systemNames of
                            Nothing -> error "wat"
                            Just x -> x
        near =  Octree.withinRange systemMap 25.0 (systemPosition keiadabiko)
        nearStations = List.concatMap (extractStations systemStations) [s | (_, s) <- near]
        stationGoods = extractGoods resSc nearStations
        route1 = routes nearStations 2
        route2 = routes nearStations 3
        route3 = routes nearStations 4
            -- route4 = routes nearStations 5
    liftIO $ print [systemName s | (_, s) <- near]
        -- liftIO $ print $ take 3 route1
        -- liftIO $ print $ List.length route1
    liftIO $ print $ showRoute $ head $ reverseSort $ map (trades stationGoods) route1
    liftIO $ print $ showRoute $ head $ reverseSort $ map (trades stationGoods) route2
    liftIO $ print $ showRoute $ head $ reverseSort $ map (trades stationGoods) route3
        -- liftIO $ print $ showRoute $ head $ reverseSort $ map (trades stationGoods) route4   