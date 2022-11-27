module Service.EstacionamentoService where

import Control.Monad ()
import Model.Vaga
import Util.DatabaseManager 
import Service.VagaService hiding (vagasArq)
import Data.Time.Clock.POSIX ( getPOSIXTime )

vagasArq :: String
vagasArq = "app/db/vaga.txt"

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

arredonda :: Double -> Double
arredonda x = (fromIntegral (floor (x * (10 ^ 2)))) / (10 ^ 2)

convertBool:: String -> Bool
convertBool s
    |s == "S" = True
    |s == "N" = False
    |otherwise = error "valor passado invalido"

pagaEstacionamento :: IO ()
pagaEstacionamento = do
  putStrLn "--- PAGAMENTO ---"

  putStrLn "Digite o numero da sua vaga: "
  numeroVaga <- readLn :: IO Int
  putStrLn "Hoje é dia comercial?(S/N) "
  isDiaSemana <- getLine
  vaga <- getVagaByNumero numeroVaga
  taxa <- taxaPagamento vaga (convertBool isDiaSemana)

  vagasString <- readArquivo vagasArq
  if isOcupada vaga
    then do
      print ("O preco final eh R$ " ++ show(arredonda taxa))
      putStrLn "Faça seu pagamento: "
      valorPago <- readLn :: IO Double
      if arredonda taxa == valorPago
        then do
          let novaLinha = replace (show vaga) (show (isOcupada vaga)) "False"
          let listaVaga = replace vagasString [show vaga] [novaLinha]
          let vagas = map (read :: String -> Vaga) listaVaga
          updateByContent vagasArq vagas
          print "Estacionamento pago com sucesso"
        else
          if arredonda taxa < valorPago
            then do
              let novaLinha = replace (show vaga) (show (isOcupada vaga)) "False"
              let listaVaga = replace vagasString [show vaga] [novaLinha]
              let vagas = map (read :: String -> Vaga) listaVaga
              updateByContent vagasArq vagas
              print ("Estacionamento pago com sucesso, seu troco eh de " ++ show (valorPago - arredonda (taxa)) ++ "reais")
            else print ("Estacionamento nao foi pago. Valor da taxa (" ++ show (arredonda taxa) ++ ") eh maior que o valor pago")
    else print "A vaga nao esta ocupada, falha ao realizar o pagamento"