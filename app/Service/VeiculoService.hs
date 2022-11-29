module Service.VeiculoService where

import Control.Monad ()
import Model.Veiculo
import Util.DatabaseManager 

veiculoArq :: String
veiculoArq = "app/db/veiculo.txt"

getVeiculo :: String -> IO [Veiculo]
getVeiculo placaCliente = do
    veiculosString <- readArquivo veiculoArq
    let veiculos = map (read :: String -> Veiculo) veiculosString
    let veiculoPlaca = [s | s <- veiculos, placa s == placaCliente]
    return veiculoPlaca


