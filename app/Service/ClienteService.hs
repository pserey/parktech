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
    putStrLn "nome: "
    nome <- getLine
    putStrLn "Gostaria que seu historico ficasse salvo? (s/n) "
    aceita <- getLine

    if aceita == "s"  then do
        let cliente = Cliente cpf nome True
        addLinha (show cliente) clienteArq
    else if aceita == "n" then do
        let cliente = Cliente cpf nome False
        addLinha (show cliente) clienteArq
    else do
        putStrLn "Escolha uma das duas opções que foi fornecida!!"
        criaCliente


