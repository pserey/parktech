module Service.EstacionamentoService where

import Control.Monad ()
import Model.Vaga (tipo, tempoInicial, Vaga())
import Util.DatabaseManager

taxaPagamento :: Vaga -> Int -> Integer ->  Float
taxaPagamento vaga diaSemana dataFinal = do
    if tipo vaga == "carro" then do
        if ((read (show (dataFinal - tempoInicial vaga)) :: Float) > 7200) then do
            if diaSemana >= 1 && diaSemana <= 5 then do
                6 + ((((read (show (dataFinal - tempoInicial vaga)))/3600)-2)*1.5)
            else
                8 + ((((read (show (dataFinal - tempoInicial vaga)))/3600)-2)*2)
        else
            if diaSemana >= 1 && diaSemana <= 5 then 6 
            else 8

    else if tipo vaga == "moto" then do
        if ((read (show(dataFinal - tempoInicial vaga)) :: Float) > 7200) then do
            if diaSemana >= 1 && diaSemana <= 5 then do
                4 + ((((read (show (dataFinal - tempoInicial vaga)))/3600)-2)*1)
            else
                6 + ((((read (show (dataFinal - tempoInicial vaga)))/3600)-2)*1.5)
        else
            if diaSemana >= 1 && diaSemana <= 5 then 4 
            else 6

    else
        if ((read (show (dataFinal - tempoInicial vaga)) :: Float) > 7200) then do
            if diaSemana >= 1 && diaSemana <= 5 then do
                8 + ((((read (show (dataFinal - tempoInicial vaga)))/3600)-2)*2)
            else
                10 + ((((read (show (dataFinal - tempoInicial vaga)))/3600)-2)*2.5)
        else
            if diaSemana >= 1 && diaSemana <= 5 then 8 
            else 10