module Service.VagaService where

import Control.Monad ()
import Data.Time.Clock.POSIX (getPOSIXTime)
import Model.Vaga
import Data.List
import Util.DatabaseManager
import Prelude hiding (id)

vagasArq :: String
vagasArq = "app/db/vaga.txt"

-- Função que apresenta menu para adicionar vaga e adiciona em arquivo do andar especificado
adicionaVaga :: IO ()
adicionaVaga = do
  putStrLn "--- ADICIONAR VAGA ---"
  putStrLn "Dê os dados da vaga a ser adicionada: "
  putStrLn "Tipo de veículo: "
  tipoVeiculo <- getLine
  putStrLn "Andar: "
  andarInput <- getLine

  vagasAndar <- calculaVagaAndar (read andarInput :: Int)
  if vagasAndar < 20 then do
    num <- proxNumVaga (read andarInput :: Int)
    let vId = show (num + 1) ++ "-" ++ tipoVeiculo ++ "-" ++ andarInput
    now <- round `fmap` getPOSIXTime
    -- placa veiculo é adicionada como string sem significado, mas não vazia para facilitar a substituição posteriormente
    let placaVeiculoVaga = "---"

    let vaga = Vaga False (num + 1) (read andarInput :: Int) tipoVeiculo now vId placaVeiculoVaga
    addLinha (show vaga) vagasArq
    putStrLn "Vaga adicionada com sucesso!"
  else
    putStrLn "Numero máximo de vagas atingido"

-- print $ show vaga

-- Função que retorna o próximo número de vaga achando a última vaga no andar especificado
proxNumVaga :: Int -> IO Int
proxNumVaga andarCheck = do
  vagasString <- readArquivo vagasArq -- tratar exception de andar não existir
  if null vagasString
    then return 0
    else do
      let vagas = map (read :: String -> Vaga) vagasString
      let vagasAndar = [s | s <- reverse vagas, andar s == andarCheck]
      if null vagasAndar
        then return 0
        else return (numero $ head vagasAndar)

-- Função que contabiliza a quantidade de vagas disponíveis
vagasDisponiveis :: IO ()
vagasDisponiveis = do
  vagasString <- readArquivo vagasArq
  let vagas = map (read :: String -> Vaga) vagasString
  putStrLn $ show (length (vagasStatus vagas "carro")) ++ " vaga(s) disponivel(is) para carros"
  putStrLn $ show (length (vagasStatus vagas "moto")) ++ " vaga(s) disponivel(is) para motos"
  putStrLn $ show (length (vagasStatus vagas "van")) ++ " vaga(s) disponivel(is) para vans"

-- Função que retorna uma lista de vagas disponíveis para um tipo específico de veículo
vagasStatus :: [Vaga] -> String -> [Vaga]
vagasStatus vagas tipoVeiculo = [s | s <- reverse vagas, not (isOcupada s) && tipo s == tipoVeiculo]

calculaVagaAndar :: Int -> IO Int
calculaVagaAndar numAndar = do
  vagasString <- readArquivo vagasArq
  let vagas = map (read :: String -> Vaga) vagasString
  return (length [s | s <- vagas, andar s == numAndar])


-- Função que contabiliza a quantidade de vagas disponíveis por andar
vagasDisponiveisAndar :: IO ()
vagasDisponiveisAndar = do
  putStrLn "Indique o andar que deseja consultar a disponíbilidade de vagas: "
  a <- readLn :: IO Int

  vagasString <- readArquivo vagasArq
  let vagas = map (read :: String -> Vaga) vagasString
  putStrLn $ show (length ([s | s <- reverse (vagasStatus vagas "carro"), andar s == a])) ++ " vaga(s) disponivel(is) para carros no andar " ++ show (a)
  putStrLn $ show (length ([s | s <- reverse (vagasStatus vagas "moto"), andar s == a])) ++ " vaga(s) disponivel(is) para motos no andar " ++ show (a)
  putStrLn $ show (length ([s | s <- reverse (vagasStatus vagas "van"), andar s == a])) ++ " vaga(s) disponivel(is) para vans no andar " ++ show (a)

-- Função que modifica o tempo inicial de uma vaga
setTempoVagaTeste :: IO ()
setTempoVagaTeste = do
  putStrLn "--- FUNÇÃO PARA MODIFICAR O TEMPO DE UM VAGA PARA TESTES ---"
  putStrLn "Insira os dados da modificação"
  putStrLn "Numero da vaga: "
  numeroVaga <- readLn :: IO Int
  putStrLn "Numero do andar: "
  numeroAndar <- readLn :: IO Int
  putStrLn "Novo tempo: "
  novoTempoInicial <- readLn :: IO Int
  vagasString <- readArquivo vagasArq
  vaga <- getVagaByNumero numeroVaga numeroAndar
  let novaLinha = replace (show vaga) (show (tempoInicial vaga)) (show novoTempoInicial)
  let listaVaga = replace vagasString [show vaga] [novaLinha]
  let vagas = map (read :: String -> Vaga) listaVaga
  updateByContent vagasArq vagas


--- funcao que retorna uma vaga a partir do id
getVagaByNumero :: Int -> Int -> IO Vaga
getVagaByNumero numeroVaga numeroAndar = do
  vagasString <- readArquivo vagasArq
  let vagas = map (read :: String -> Vaga) vagasString
  let vaga = [s | s <- reverse vagas, numero s == numeroVaga && andar s == numeroAndar]
  if null vaga
    then error "A vaga buscada nao foi encontrada"
    else return $ head vaga

getVagaById :: String -> IO Vaga
getVagaById idVagaSearch = do
  vagasString <- readArquivo vagasArq
  let vagas = map (read :: String -> Vaga) vagasString
  let vaga = [s | s <- reverse vagas, idVaga s == idVagaSearch]
  if null vaga
    then error "A vaga buscada nao foi encontrada"
    else return $ head vaga

    --- adiciona vaga de acordo com o tipo do veiculo, sendo ao todo 7 vagas para carro, 2 para motos e 1 para van.
adicionaVagasAndarPorTipoVeiculo :: Int -> Int -> IO()
adicionaVagasAndarPorTipoVeiculo numeroAndar cont = do
  num <- proxNumVaga numeroAndar
  let vId = show (num + 1) ++ "-" ++ buscaTipoVeiculo cont ++ "-" ++ show numeroAndar
  now <- round `fmap` getPOSIXTime
  -- placa veiculo é adicionada como string sem significado, mas não vazia para facilitar a substituição posteriormente
  let placaVeiculoVaga = "---"
  let vaga = Vaga False (num + 1) numeroAndar (buscaTipoVeiculo cont) now vId placaVeiculoVaga
  addLinha (show vaga) vagasArq


  --- retorna o proximo tipo de vaga por tipo de veiculo
buscaTipoVeiculo :: Int -> String
buscaTipoVeiculo cont 
  |cont >= 3 = "carro"
  |cont >= 1 = "moto"
  |otherwise = "van"

-- Funcao para buscar o ultimo andar Cadastrado
buscaUltimoAndar :: IO Int
buscaUltimoAndar = do
  vagasString <- readArquivo vagasArq -- tratar exception de andar não existir
  if null vagasString
    then return 0
    else do
      let vagas = map (read :: String -> Vaga) vagasString
      let andarNumeros = map andar vagas
      if null andarNumeros
        then return 0
        else return $ maximum andarNumeros

-- Funcao Para adicionar um novo andar no estacionamento. Cada andar tem por padrão 10 vagas, que serão distribuidas
-- no metodo adcionasVagasPorTipoVeiculo
adicionaAndar :: IO ()
adicionaAndar = do
  putStrLn "Adicionando proximo andar"
  numAndar <- buscaUltimoAndar
  adicionaAndarRecursivo 10 (numAndar + 1)

--Função recursiva para adicionar as vagas no andar andar
adicionaAndarRecursivo :: Int -> Int -> IO ()
adicionaAndarRecursivo cont numeroAndar =
  if cont >= 1 then do
    adicionaVagasAndarPorTipoVeiculo numeroAndar (cont - 1)
    adicionaAndarRecursivo (cont - 1) numeroAndar
  else
    putStrLn "Andar cadastrado com sucesso"