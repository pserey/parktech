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
vagasStatus vagas tipoVeiculo = [s | s <- reverse vagas, isOcupada s == False && tipo s == tipoVeiculo]
    
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
