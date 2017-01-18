-- | Simple currency handling and exchange library

module Data.Money
  (
    USD(..)
  , RUB(..)
  , EUR(..)
  , BTC(..)
  , LTC(..)

  , CurrencySymbol(..)
  , ScalableAdditive(..)
  
  , ExchangeRates
  , makeExchangeRates
  , Money
  , makeMoney
  , makeUSD
  , makeRUB
  , makeEUR
  , makeBTC
  , makeLTC
  , amount
  , symbol
  , findRate
  , exchangeTo
  , sampleRates
  ) where

import qualified Data.Map.Strict as M
import Data.Typeable (Typeable, TypeRep, typeOf)

-- | Things that can be looked up in the rates dictionary
class (Typeable a, Eq a) => CurrencySymbol a

-- | U.S. Dollar
data USD = USD deriving (Show, Typeable, Eq)
-- | Russian Ruble
data RUB = RUB deriving (Show, Typeable, Eq)
-- | Euro
data EUR = EUR deriving (Show, Typeable, Eq)
-- | Bitcoin
data BTC = BTC deriving (Show, Typeable, Eq)
-- | Litecoin
data LTC = LTC deriving (Show, Typeable, Eq)

-- | Currency symbols can have exchange rates
instance CurrencySymbol USD
instance CurrencySymbol RUB
instance CurrencySymbol EUR
instance CurrencySymbol BTC
instance CurrencySymbol LTC

-- | Polymorphic type representing money
data Money a = Money
  { amount :: Double
  -- ^ Extract amount (i.e. quantity of the asset) from 'Money'
  , symbol :: a
  -- ^ Extract the currency symbol from 'Money'
  } deriving (Show, Eq)


-- | Things that can be scaled (or shrinked) with scalars, and added (subtracted) together (like money or a mining hashrate)
class ScalableAdditive a where
  -- Minimal instance: (^*), (^+^), (^-^)
  (^*) :: a -> Double -> a
  (^+^) :: a -> a -> a
  (^-^) :: a -> a -> a
  
  (^/) :: a -> Double -> a
  -- default implementation
  i ^/ c = i ^* (1/c)


-- | Money can be `scaled' or added together when they have equal types 
instance ScalableAdditive (Money a) where
  (^*) (Money a sy) c = Money (c * a) sy
  (^+^) (Money a1 sy) (Money a2 _) = Money (a1 + a2) sy
  (^-^) (Money a1 sy) (Money a2 _) = Money (a1 - a2) sy  


-- | Money constructor over currency symbols 
makeMoney :: (CurrencySymbol a) => Double -> a -> Money a
makeMoney = Money

makeUSD = flip makeMoney USD
makeRUB = flip makeMoney RUB
makeEUR = flip makeMoney EUR
makeBTC = flip makeMoney BTC
makeLTC = flip makeMoney LTC


-- | A dictionary of exchange rates (indexed by pairs of base and foreign currency symbols)
data ExchangeRates = ExchangeRates (M.Map (TypeRep, TypeRep) Double)
  deriving (Show)


-- | Maybe returns the direct conversion rate
findDirectRate :: (CurrencySymbol a, CurrencySymbol b) => ExchangeRates -> a -> b -> Maybe Double
findDirectRate (ExchangeRates ex) fc tc
  | typeOf fc == typeOf tc = Just 1
  | otherwise = (M.lookup (typeOf fc, typeOf tc) ex)


-- | Returns the direct conversion rate or tries to compute the reverse one
findRate :: (CurrencySymbol a, CurrencySymbol b) => ExchangeRates -> a -> b -> Maybe Double
findRate ex fc tc = case findDirectRate ex fc tc of
  Nothing -> (/) <$> Just 1 <*> findDirectRate ex tc fc
  dr -> dr


-- | Maybe convert the money to designated currency symbol
exchangeTo :: (CurrencySymbol a, CurrencySymbol b) => ExchangeRates -> Money a -> b -> Maybe (Money b)
exchangeTo ex m sy = do
  r <- findRate ex (symbol m) sy
  let a = (amount m) * r
  return $ Money a sy

-- | Construct the exchange dictionary from instances of 'CurrencySymbol'
makeExchangeRates :: (CurrencySymbol a, CurrencySymbol b) => [((a, b), Double)] -> ExchangeRates
makeExchangeRates rs =
  let rs' = map (\((fc, tc), co) -> ((typeOf fc, typeOf tc), co)) rs
  in
    ExchangeRates $ M.fromList rs'

-- | Sample rates dictionary (19 Jan 2017)
sampleRates :: ExchangeRates
sampleRates = ExchangeRates $ M.fromList
  [ ((typeOf USD, typeOf RUB), 59.24)
  , ((typeOf BTC, typeOf USD), 866.689)
  , ((typeOf LTC, typeOf USD), 3.846)
  , ((typeOf EUR, typeOf RUB), 63.27)
  , ((typeOf EUR, typeOf USD), 1.064)
  , ((typeOf BTC, typeOf RUB), 50951.4)
  , ((typeOf LTC, typeOf RUB), 225.88)
  , ((typeOf LTC, typeOf BTC), 0.00443)  
  ]
