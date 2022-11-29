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

criaCliente :: IO()
criaCliente = do
    putStrLn "--- CRIA CLIENTE ---"
    putStrLn "Dê os dados do cliente a ser adicionado: "
    putStrLn "cpf: "
    cpf <- getLine
    putStrLn "nome "
    nome <- getLine

    let cliente = Cliente cpf nome
    addLinha (show cliente) clienteArq
