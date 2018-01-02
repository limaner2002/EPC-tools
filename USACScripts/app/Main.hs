{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Scripts.Opts
import Options.Applicative (execParser)

main :: IO ()
main = join $ execParser commandsInfo
