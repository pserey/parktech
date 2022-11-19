module Service.VagaService ( adicionaVaga ) where

import Util.DatabaseManager
import Model.Vaga as V
import Control.Monad ()
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Prelude hiding (id)

vagas :: String
vagas = "app/db/vaga"

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
    let vId = show num ++ "-" ++ tipoVeiculo ++ "-" ++ andarInput
    now <- round `fmap` getPOSIXTime

    let vagasArq = vagas ++ andarInput ++ ".txt"

    let vaga = Vaga False (num + 1) (read andarInput :: Int) tipoVeiculo now vId
    addLinha (show vaga) vagasArq

-- Função que retorna o próximo número de vaga achando a última vaga no andar especificado
proxNumVaga :: Int -> IO Int
proxNumVaga andar = do
    -- abre arquivo do andar especifico
    let vagasArq = vagas ++ show andar ++ ".txt"
    vagasString <- readArquivo vagasArq -- tratar exception de andar não existir
    let vagasList = map (read :: String -> Vaga) vagasString
    let vagasAndar = [s | s <- reverse vagasList, V.andar s == andar]
    return $ V.numero (head vagasAndar)