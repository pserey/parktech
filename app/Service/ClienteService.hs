module Service.ClienteService where

import Control.Monad ()
import Model.Cliente
import Model.Historico
import Util.DatabaseManager 

clientesArq :: String
clientesArq = "app/db/cliente.txt"

historicosArq :: String
historicosArq = "app/db/historico.txt"

verificaCliente :: String -> IO Bool
verificaCliente cpfCliente = do
    clientesString <- readArquivo clientesArq
    let clientes = map (read :: String -> Cliente) clientesString
    let clienteCpf = [s | s <- clientes, cpf s == cpfCliente]
    return $ not (null clienteCpf)


getClienteByCpf :: String -> IO Cliente
getClienteByCpf cpfCliente = do
    clientesString <- readArquivo clientesArq
    let clientes = map (read :: String -> Cliente) clientesString
    let clienteCpf = [s | s <- clientes, cpf s == cpfCliente]
    return $ head clienteCpf

getHistoricoByCpf :: String -> IO [Historico]
getHistoricoByCpf cpfCliente = do
    historicosString <- readArquivo historicosArq
    let historicos = map (read :: String -> Historico) historicosString
    return [s | s <- historicos, clienteCpf s == cpfCliente]