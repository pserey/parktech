module Service.VeiculoService where

import Control.Monad ()
import Model.Veiculo
import Util.DatabaseManager 

veiculoArq :: String
veiculoArq = "app/db/veiculo.txt"

criaVeiculo :: IO()
criaVeiculo = do
    putStrLn "--- CRIA VEICULO ---"
    putStrLn "Dê os dados do veículo a ser adicionado: "
    putStrLn "Tipo de veículo: "
    tipo <- getLine
    putStrLn "placa do veículo: "
    placa <- getLine
    putStrLn "cor: "
    cor <- getLine

    let veiculo = Veiculo tipo placa cor
    addLinha (show veiculo) veiculoArq
    putStrLn "Veiculo cadastrado com sucesso!"


getVeiculo :: String -> IO [Veiculo]
getVeiculo placaCliente = do
    veiculosString <- readArquivo veiculoArq
    let veiculos = map (read :: String -> Veiculo) veiculosString
    let veiculoPlaca = [s | s <- veiculos, placa s == placaCliente]
    return veiculoPlaca

