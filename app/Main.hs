module Main where

import Runner as R
import Options.Applicative

options :: Parser R.Configuration
options = R.Configuration
  <$> option auto
      ( long "send-for" <> metavar "SECONDS"
     <> help "How long to send messages" )
  <*> option auto
      ( long "wait-for" <> metavar "SECONDS"
     <> help "The length of the grace period" )
  <*> option auto
      ( long "with-seed" <> metavar "SEED"
     <> help "Seed for RNGs" )

main :: IO ()
main = execParser opts >>= R.run
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> header "CH/OTP Test Task" )
