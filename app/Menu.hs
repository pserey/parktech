module Menu ( menuInicial, exit ) where

import Service.VagaService
import Service.EstacionamentoService

menuInicial :: IO()
menuInicial = do
    putStrLn "\n--- FAÇA LOGIN ---"
    putStrLn "\n1 - Sou cliente"
    putStrLn "\n2 - Sou administrador"
    putStrLn "\n3 - Sair do sistema"

    opcao <- getLine
    putStr "\n"
    opcaomenuInicial opcao

    if  opcao /= "3" then menuInicial
    else exit


menuAdministrador :: IO()
menuAdministrador = do
    putStrLn "\n--- BEM VINDO! ---"
    putStrLn "\nEscolha o que você quer fazer: "
    putStrLn "\n1 - Adcionar vaga"
    putStrLn "\n2 - Adcionar andar"
    putStrLn "\n3 - Adcionar tempo em vaga"
    putStrLn "\n4 - Voltar para o menu inicial"

    opcao <- getLine
    putStr "\n"
    opcaomenuAdministrador opcao

menuCliente :: IO()
menuCliente = do
    putStrLn "\n--- BEM VINDO! ---"
    putStrLn "\nO estacionamento está funcionando! Escolha o que você quer fazer: "
    putStrLn "\n1 - Estacionar veiculo"
    putStrLn "\n2 - Pagar estacionamento"
    putStrLn "\n3 - Ver vagas disponiveis"
    putStrLn "\n4 - Ver vagas disponiveis por andar"
    putStrLn "\n5 - Voltar para o menu inicial"

    opcao <- getLine
    putStr "\n"
    opcaomenuCliente opcao


opcaomenuInicial :: String -> IO()
opcaomenuInicial opcao
    | opcao == "1" = menuCliente
    | opcao == "2" = menuAdministrador
    | opcao == "3" = putStrLn "Sistema fechando"
    | otherwise = do putStrLn "Insira um valor válido!\n"


opcaomenuAdministrador :: String -> IO()
opcaomenuAdministrador opcao
    |opcao == "1" = adicionaVaga
    |opcao == "2" = putStrLn "adicionaAndar"
    |opcao == "3" = putStrLn "adicionaTempoVaga" -- TODO: falta fazer o menu na função
    |opcao == "4" = menuInicial
    |otherwise = do 
        putStrLn "Insira um valor válido!\n"
        menuAdministrador

opcaomenuCliente :: String -> IO()
opcaomenuCliente opcao
    |opcao == "1" = estacionaVeiculo
    |opcao == "2" = pagaEstacionamento
    |opcao == "3" = vagasDisponiveis
    |opcao == "4" = vagasDisponiveisAndar
    |opcao == "5" = menuInicial
    |otherwise = do 
        putStrLn "Insira um valor válido!\n"
        menuCliente
    
exit :: IO()
exit = do
    putStrLn "Encerrando sessão"