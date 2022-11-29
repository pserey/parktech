module Service.ClienteService where

import Control.Monad ()
import Model.Cliente
import Util.DatabaseManager 

clientesArq :: String
clientesArq = "app/db/cliente.txt"

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