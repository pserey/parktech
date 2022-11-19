module Service.VagaService where

import Util.DatabaseManager ( addLinha, readArquivo, getValor )
import Model.Vaga
import Control.Monad ()
import Data.Time.Clock.POSIX ( getPOSIXTime )

andarDir = "app/db/andar"

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
    let id = num ++ "-" ++ tipoVeiculo ++ "-" ++ andarInput
    now <- round `fmap` getPOSIXTime

    let vagasArq = andarDir ++ andarInput ++ ".txt"

    let vaga = Vaga False (read num + 1) (read andarInput :: Int) tipoVeiculo now id
    addLinha (show vaga) vagasArq

-- Função que retorna o próximo número de vaga achando a última vaga no andar especificado
proxNumVaga :: Int -> IO String
proxNumVaga andar = do
    -- abre arquivo do andar especifico
    let vagasArq = andarDir ++ show andar ++ ".txt"
    vagas <- readArquivo vagasArq -- tratar exception de andar não existir

    getValor "numero" $ last vagas -- tratar exception com arquivo vazio OU garantir que se andar existir, vaga 1 existe