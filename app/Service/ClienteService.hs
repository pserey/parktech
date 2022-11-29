module Service.ClienteService where

import Control.Monad ()
import Model.Cliente
import Util.DatabaseManager 

clienteArq :: String
clienteArq = "app/db/cliente.txt"

verificaCliente :: String -> IO Bool
verificaCliente cpfCliente = do
    clientesString <- readArquivo clienteArq
    let clientes = map (read :: String -> Cliente) clientesString
    let clienteCPF = [s | s <- clientes, cpf s == cpfCliente]
    return $ not (null clienteCPF)
