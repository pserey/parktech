module Menu where

menuInicial :: IO()
menuInicial = do
putStrLn "\n--- FAÇA LOGIN ---"
putStrLn "\n1 - Sou cliente"
putStrLn "\n2 - Sou administrador"
putStrLn "\n3 - Sair do sistema"

opcao <- getLine
putStr "\n"
opcaomenuInicial opcao


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
    putStrLn "\n4 - Voltar para o menu inicial"

    opcao <- getLine
    putStr "\n"
    opcaomenuCliente opcao


opcaomenuInicial :: String -> IO()
opcaomenuInicial opcao
| opcao == "1" = menuCliente
| opcao == "2" = menuAdministrador
| opcao == "3" = exit
| otherwise = do putStrLn "Insira um valor válido!\n"


opcaomenuAdministrador :: String -> IO()
opcaomenuAdministrador opcao
    |opcao == "1" = adicionaVaga
    |opcao == "2" = adicionaAndar
    |opcao == "3" = adicionaTempoVaga
    |opcao == "4" = menuInicial
    |otherwise = do putStrLn "Insira um valor válido!\n"

opcaomenuCliente :: String -> IO()
opcaomenuCliente opcao
    |opcao == "1" = estacionaVeiculo
    |opcao == "2" = pagaEstacionamento
    |opcao == "3" = listaVagas
    |opcao == "4" = menuInicial
    |otherwise = do putStrLn "Insira um valor válido!\n"
    
exit :: IO()
exit = do
    putStrLn "Encerrando sessão"