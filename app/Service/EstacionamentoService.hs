{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Service.EstacionamentoService where

import Control.Monad ()
import Model.Vaga (tipo, tempoInicial, Vaga())
import Util.DatabaseManager ()
import Data.Time.Clock.POSIX ( getPOSIXTime )


date :: IO  Integer
date = round `fmap` getPOSIXTime

taxaPagamento :: Vaga -> Int -> IO Float
taxaPagamento vaga diaSemana = do
    x <- date
    let dataFinal = fromIntegral (x)
    let dataInicial = fromIntegral (tempoInicial vaga) 
    if tipo vaga == "carro" then do
        if (dataFinal - dataInicial) > 7200 then do
            if diaSemana >= 1 && diaSemana <= 5 then do
                return (6 + ((((dataFinal - dataInicial) / 3600)-2)*1.5))
            else
                return (8 + ((((dataFinal - dataInicial ) /3600)-2)*2))
        else
            if diaSemana >= 1 && diaSemana <= 5 then return 6 
            else return 8

    else if tipo vaga == "moto" then do
        if read(show(dataFinal - dataInicial))  > 7200 then do
            if diaSemana >= 1 && diaSemana <= 5 then do
                return (4 + (((dataFinal - dataInicial) /3600)-2))
            else
                return (6 + ((((dataFinal - dataInicial) /3600)-2)*1.5))
        else
            if diaSemana >= 1 && diaSemana <= 5 then return 4 
            else return 6

    else
        if read(show(dataFinal - dataInicial))  > 7200 then do
            if diaSemana >= 1 && diaSemana <= 5 then do
                return (8 + ((((dataFinal - dataInicial)/3600)-2)*2))
            else
                return (10 + ((((dataFinal - dataInicial) /3600)-2)*2.5))
        else
            if diaSemana >= 1 && diaSemana <= 5 then return 8 
            else return 10