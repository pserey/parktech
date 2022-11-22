module Service.EstacionamentoService where

import Util.DatabaseManager
import Model.Vaga
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.Time.Calendar.WeekDate

taxaPagamento :: String -> Integer -> Float
taxaPagamento tipo dataInicial
    |tipo == "carro" = 6 + (((now - dataInicial)*3600000)*1.50)
    |tipo == "moto" = 4 + (((now - dataInicial)*3600000)*1)
    |tipo == "van" = 8 + (((now - dataInicial)*3600000)*2)
    |otherwise = "Tipo não existe"
    where now <- round `fmap` getPOSIXTime
