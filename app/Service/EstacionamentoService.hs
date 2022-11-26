{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Service.EstacionamentoService where

import Control.Monad ()
import Model.Vaga (tipo, tempoInicial, Vaga())
import Util.DatabaseManager ()
import Data.Time.Clock.POSIX ( getPOSIXTime )


date :: IO  Integer
date = round `fmap` getPOSIXTime

horas :: Float -> Float -> IO Integer
horas dataFinal dataInicial = do
    let date1 = round dataFinal
    let date2 =  round dataInicial
    return (date1 - date2)

taxaPagamento :: Vaga -> Bool -> IO Double
taxaPagamento vaga iSDiaSemana = do
    current <- date
    let dataFinal = fromIntegral (current)
    let dataInicial = fromIntegral (tempoInicial vaga) 
    diferenca <- horas dataFinal dataInicial
    let d = fromIntegral(diferenca)
    if tipo vaga == "carro" then do
        if (dataFinal - dataInicial) > 7200 then do
            if iSDiaSemana then do
                return (6 + (((d / 3600)-2)*1.5))
            else
                return (8 + (((d /3600)-2)*2))
        else
            if iSDiaSemana then return 6 
            else return 8

    else if tipo vaga == "moto" then do
        if (dataFinal - dataInicial)  > 7200 then do
            if iSDiaSemana then do
                return (4 + ((d /3600)-2))
            else
                return (6 + (((d /3600)-2)*1.5))
        else
            if iSDiaSemana then return 4 
            else return 6

    else
        if (dataFinal - dataInicial)  > 7200 then do
            if iSDiaSemana then do
                return (8 + (((d/3600)-2)*2))
            else
                return (10 + (((d /3600)-2)*2.5))
        else
            if iSDiaSemana then return 8 
            else return 10