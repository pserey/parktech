module Service.EstacionamentoService where

import Control.Monad ()
import Model.Vaga as V
import Model.Veiculo as Ve
import Util.DatabaseManager 
import Service.VagaService
import Service.ClienteService
import Service.VeiculoService 
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
    let dataFinal = fromIntegral current
    let dataInicial = fromIntegral (tempoInicial vaga) 
    diferenca <- horas dataFinal dataInicial
    let d = fromIntegral diferenca
    if V.tipo vaga == "carro" then do
        if (dataFinal - dataInicial) > 7200 then do
            if iSDiaSemana then do
                return (6 + (((d / 3600)-2)*1.5))
            else
                return (8 + (((d /3600)-2)*2))
        else
            if iSDiaSemana then return 6 
            else return 8

    else if V.tipo vaga == "moto" then do
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
arredonda x = fromIntegral (floor (x * (10 ^ 2))) / (10 ^ 2)

convertBool:: String -> Bool
convertBool s
    |s == "S" = True
    |s == "N" = False
    |otherwise = error "valor passado invalido"

pagaEstacionamento :: IO ()
pagaEstacionamento = do
  putStrLn "--- PAGAMENTO ---"

  putStrLn "Digite o numero do andar: "
  numeroAndar <- readLn :: IO Int
  putStrLn "Digite o numero da sua vaga: "
  numeroVaga <- readLn :: IO Int
  putStrLn "Hoje é dia comercial?(S/N) "
  isDiaSemana <- getLine
  vaga <- getVagaByNumero numeroVaga numeroAndar
  taxa <- taxaPagamento vaga (convertBool isDiaSemana)

  vagasString <- readArquivo vagasArq
  if isOcupada vaga
    then do
      print ("O preco final eh R$ " ++ show (arredonda taxa))
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
              print $ "Estacionamento pago com sucesso, seu troco eh de " ++ show (valorPago - arredonda taxa) ++ "reais"
            else print $ "Estacionamento nao foi pago. Valor da taxa (" ++ show (arredonda taxa) ++ ") eh maior que o valor pago"
    else print "A vaga nao esta ocupada, falha ao realizar o pagamento"

estacionaVeiculo :: IO()
estacionaVeiculo = do
    putStrLn "--- ESTACIONAR ---"
    putStrLn "Insira seu CPF: "
    cpfCliente <- getLine
    
    -- recomenda vaga
    
    statusCadastroCliente <- verificaCliente cpfCliente
    if statusCadastroCliente then do
        verificaCadastroVeiculo cpfCliente
    else do
        print "criar cliente" -- criar função
        verificaCadastroVeiculo cpfCliente

verificaCadastroVeiculo :: String -> IO()
verificaCadastroVeiculo cpfCliente = do    
    putStrLn "Insira a placa do veiculo: "
    placaVeiculo <- getLine
    veiculoClienteList <- getVeiculo placaVeiculo
    if not (null veiculoClienteList) then do
        let veiculoCliente = head veiculoClienteList
        verificaDisponibilidadeVaga veiculoCliente cpfCliente
    else do
        print "criaVeiculo" -- criar função 
        let veiculoCliente = head veiculoClienteList
        verificaDisponibilidadeVaga veiculoCliente cpfCliente

verificaDisponibilidadeVaga :: Veiculo -> String -> IO()
verificaDisponibilidadeVaga veiculoCliente cpfCliente = do
    putStrLn "Insira o andar que você deseja estacionar:"
    andarEstacionamento <- readLn :: IO Int
    putStrLn "Insira a vaga que você deseja estacionar:"
    vagaEstacionamento <- readLn :: IO Int

    vagaEscolhida <- getVagaByNumero vagaEstacionamento andarEstacionamento
    
    if Ve.tipo veiculoCliente == V.tipo vagaEscolhida then do
        if not $ isOcupada vagaEscolhida then do
            estaciona cpfCliente (placa veiculoCliente) vagaEstacionamento andarEstacionamento
            print "Veiculo estacionado"
        else do
            vagasString <- readArquivo vagasArq
            let vagas = map (read :: String -> Vaga) vagasString
            let vagasLivres = vagasStatus vagas (Ve.tipo veiculoCliente)
            if not (null vagasLivres) then do
                print $ "A vaga escolhida nao esta disponivel, mas voce pode estacionar o veiculo na vaga numero "
                         ++ show (numero $ head vagasLivres) ++ " no andar " ++ show (andar $ head vagasLivres)
                print "Deseja estacionar nessa vaga? (s/n)"
                opcao <- getLine
                if opcao == "s" then do
                    estaciona cpfCliente (placa veiculoCliente) vagaEstacionamento andarEstacionamento
                    print "Veiculo estacionado"
                else verificaDisponibilidadeVaga veiculoCliente cpfCliente  
            else 
                print "Nao ha vagas livres que comportem esse tipo de veiculo"
    else do 
        print "Seu veiculo nao pode estacionar nessa vaga, porque ela nao comporta veiculos desse tipo"
        verificaDisponibilidadeVaga veiculoCliente cpfCliente  

-- estaciona :: cpfcliente -> placaVeiculo -> numeroVaga -> numeroAndar
-- alterar vaga --- isOcupada = true, placaVeiculo = pv, tempoInicial = posix
estaciona :: String -> String -> Int -> Int -> IO()
estaciona cpfCliente placaVeiculo numeroVaga numeroAndar = do
    print "deu certo!!!!!!!!!!!!!!" 