module Service.VagaService where

import Util.DatabaseManager
import Model.Vaga
import Control.Monad ()
import Data.Time.Clock.POSIX ( getPOSIXTime )
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

    num <- proxNumVaga (read andarInput :: Int)
    let vId = show (num + 1) ++ "-" ++ tipoVeiculo ++ "-" ++ andarInput
    now <- round `fmap` getPOSIXTime

    let vaga = Vaga False (num + 1) (read andarInput :: Int) tipoVeiculo now vId
    addLinha (show vaga) vagasArq
    -- print $ show vaga

-- Função que retorna o próximo número de vaga achando a última vaga no andar especificado
proxNumVaga :: Int -> IO Int
proxNumVaga andarCheck = do
    vagasString <- readArquivo vagasArq -- tratar exception de andar não existir
    if null vagasString then return 0
    else do
        let vagas = map (read :: String -> Vaga) vagasString
        let vagasAndar = [s | s <- reverse vagas, andar s == andarCheck]
        if null vagasAndar then return 0
        else return (numero $ head vagasAndar)

-- Função que contabiliza a quantidade de vagas disponíveis
vagasDisponiveis :: IO()
vagasDisponiveis = do
    vagasString <- readArquivo vagasArq
    let vagas = map (read :: String -> Vaga) vagasString
    putStrLn $ show(length (vagasStatus vagas "carro")) ++ " vaga(s) disponivel(is) para carros"
    putStrLn $ show(length (vagasStatus vagas "moto")) ++ " vaga(s) disponivel(is) para motos"
    putStrLn $ show(length (vagasStatus vagas "van")) ++ " vaga(s) disponivel(is) para vans"

-- Função que retorna uma lista de vagas disponíveis para um tipo específico de veículo
vagasStatus :: [Vaga] -> String -> [Vaga]
vagasStatus vagas tipoVeiculo = [s | s <- reverse vagas, not (isOcupada s) && tipo s == tipoVeiculo]
    
-- Função que contabiliza a quantidade de vagas disponíveis por andar
vagasDisponiveisAndar :: IO()
vagasDisponiveisAndar = do
    putStrLn "Indique o andar que deseja consultar a disponíbilidade de vagas: "
    a <- readLn :: IO Int

    vagasString <- readArquivo vagasArq
    let vagas = map (read :: String -> Vaga) vagasString
    print $ show(length([s | s <- reverse (vagasStatus vagas "carro"), andar s == a])) ++ " vaga(s) disponivel(is) para carros no andar " ++ show(a)
    print $ show(length([s | s <- reverse (vagasStatus vagas "moto"), andar s == a])) ++ " vaga(s) disponivel(is) para motos no andar " ++ show(a)
    print $ show(length([s | s <- reverse (vagasStatus vagas "van"), andar s == a])) ++ " vaga(s) disponivel(is) para vans no andar " ++ show(a)

    -- Função que modifica o tempo inicial de uma vaga
setTempoVagaTeste :: Int -> Int -> IO ()
setTempoVagaTeste numeroVaga novoTempoInicial = do
    vagasString <- readArquivo vagasArq
    vaga <- getVagaByNumero numeroVaga
    let vagaStr = show vaga
    let vagaStr2 = show vaga
    let tempoVaga = show (tempoInicial vaga)
    let novoTempo = show novoTempoInicial
    let novaLinha = replace vagaStr tempoVaga novoTempo
    let listaVaga = replace vagasString [vagaStr2] [novaLinha]
    let vagas = map (read :: String -> Vaga) listaVaga
    updateByContent vagasArq vagas
    
--- funcao que faz o replace 
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
    then repl ++ (replace (drop (length find) s) find repl)
    else [head s] ++ (replace (tail s) find repl)

getVagaByNumero :: Int -> IO Vaga
getVagaByNumero numeroVaga = do
    vagasString <- readArquivo vagasArq
    let vagas = map (read :: String -> Vaga) vagasString
    let vaga = [s | s <- reverse vagas, numero s == numeroVaga]
    return $ head vaga -- p precisa tratar essa exceção