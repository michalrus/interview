module Runner
    ( run
    , Configuration(..)
    ) where

data Configuration = Configuration
  { sendFor :: Int
  , gracePeriod :: Int
  , seed :: Int
  } deriving (Show, Eq)

run :: Configuration -> IO ()
run unsafeConf = putStrLn $ show conf
  where
    conf = sanitize unsafeConf
    sanitize (Configuration a b c) = Configuration (abs a) (abs b) (abs c)
