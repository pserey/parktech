module Service.EstacionamentoService where

import Control.Monad ()
import Model.Vaga as Va
import Model.Veiculo as Ve
import Model.Cliente as C
import Model.Historico as H
import Util.DatabaseManager 
import Service.VagaService
import Service.ClienteService
import Service.VeiculoService 
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.List
import Data.Maybe

date :: IO  Integer
date = round `fmap` getPOSIXTime

horas :: Float -> Float -> IO Integer
horas dataFinal dataInicial = do
    let date1 = round dataFinal
    let date2 =  round dataInicial
    return (date1 - date2)

retornaHoras :: IO Integer -> IO Double
retornaHoras tempoVaga = do
    retorno <- tempoVaga
    let hora = fromIntegral retorno / 3600
    return hora

transforma :: IO()
transforma = do
    putStrLn "----- TEMPO DA VAGA -----"
    putStrLn "Diga o numero da sua vaga: "
    vagaInput <- readLn
    putStrLn "Diga o numero do andar: "
    andar <- readLn
    vaga <- getVagaByNumero vagaInput andar
    current <- date
    let dataFinal = fromIntegral current
    let dataInicial = fromIntegral (tempoInicial vaga) 

    final <- retornaHoras(horas dataFinal dataInicial)
    let retorno = arredonda final
    putStrLn $ show retorno ++ " horas"


taxaPagamento :: Vaga -> Bool -> IO Double
taxaPagamento vaga iSDiaSemana = do
    current <- date
    let dataFinal = fromIntegral current
    let dataInicial = fromIntegral (tempoInicial vaga) 
    diferenca <- horas dataFinal dataInicial
    let d = fromIntegral diferenca
    if Va.tipo vaga == "carro" then do
        if (dataFinal - dataInicial) > 7200 then do
            if iSDiaSemana then do
                return (6 + (((d / 3600)-2)*1.5))
            else
                return (8 + (((d /3600)-2)*2))
        else
            if iSDiaSemana then return 6 
            else return 8

    else if Va.tipo vaga == "moto" then do
        if (dataFinal - dataInicial)  > 7200 then do
            if iSDiaSemana then do
                return (4 + ((d /3600)-2))
            else
                return (6 + (((d /3600)-2)*1.5))
        else
            if iSDiaSemana then return 4 
            else return 6

    else
        if (dataFinal - dataInicial)  > 7200 then do
            if iSDiaSemana then do
                return (8 + (((d/3600)-2)*2))
            else
                return (10 + (((d /3600)-2)*2.5))
        else
            if iSDiaSemana then return 8 
            else return 10

arredonda :: Double -> Double
arredonda x = fromIntegral (floor (x * (10 ^ 2))) / (10 ^ 2)

convertBool:: String -> Bool
convertBool s
    |s == "S" = True
    |s == "N" = False
    |otherwise = error "valor passado invalido"

pagaEstacionamento :: IO ()
pagaEstacionamento = do
  putStrLn "--- PAGAMENTO ---"

  putStrLn "Digite o numero do andar: "
  numeroAndar <- readLn :: IO Int
  putStrLn "Digite o numero da sua vaga: "
  numeroVaga <- readLn :: IO Int
  putStrLn "Hoje é dia comercial?(S/N) "
  isDiaSemana <- getLine
  vaga <- getVagaByNumero numeroVaga numeroAndar
  taxa <- taxaPagamento vaga (convertBool isDiaSemana)

  vagasString <- readArquivo vagasArq
  if isOcupada vaga
    then do
      putStrLn ("O preco final eh R$ " ++ show (arredonda taxa))
      putStrLn "Faça seu pagamento: "
      valorPago <- readLn :: IO Double
      if arredonda taxa == valorPago
        then do
          let novaLinha = replace (show vaga) (show (isOcupada vaga)) "False"
          let listaVaga = replace vagasString [show vaga] [novaLinha]
          let vagas = map (read :: String -> Vaga) listaVaga
          updateByContent vagasArq vagas
          putStrLn "Estacionamento pago com sucesso"
        else
          if arredonda taxa < valorPago
            then do
              let novaLinha = replace (show vaga) (show (isOcupada vaga)) "False"
              let listaVaga = replace vagasString [show vaga] [novaLinha]
              let vagas = map (read :: String -> Vaga) listaVaga
              updateByContent vagasArq vagas
              putStrLn $ "Estacionamento pago com sucesso, seu troco eh de " ++ show (valorPago - arredonda taxa) ++ "reais"
            else putStrLn $ "Estacionamento nao foi pago. Valor da taxa (" ++ show (arredonda taxa) ++ ") eh maior que o valor pago"
    else putStrLn "A vaga nao esta ocupada, falha ao realizar o pagamento"

estacionaVeiculo :: IO()
estacionaVeiculo = do
    putStrLn "--- ESTACIONAR ---"
    putStrLn "Insira seu CPF: "
    cpfCliente <- getLine
    
    statusCadastroCliente <- verificaCliente cpfCliente
    if statusCadastroCliente then do
        verificaCadastroVeiculo cpfCliente
    else do
        criaCliente
        verificaCadastroVeiculo cpfCliente

verificaCadastroVeiculo :: String -> IO()
verificaCadastroVeiculo cpfCliente = do    
    putStrLn "Insira a placa do veiculo: "
    placaVeiculo <- getLine
    veiculoClienteList <- getVeiculo placaVeiculo
    if not (null veiculoClienteList) then do
        let veiculoCliente = head veiculoClienteList
        verificaDisponibilidadeVaga veiculoCliente cpfCliente
    else do
        criaVeiculo
        veiculoCriadoList <- getVeiculo placaVeiculo
        let veiculoCliente = head veiculoCriadoList
        verificaDisponibilidadeVaga veiculoCliente cpfCliente

verificaDisponibilidadeVaga :: Veiculo -> String -> IO()
verificaDisponibilidadeVaga veiculoCliente cpfCliente = do
    vagaRecomendada <- recomendaVaga cpfCliente veiculoCliente
    
    putStrLn "--- VAGA RECOMENDADA ---"
    if isJust vagaRecomendada then
        putStrLn $ "A vaga recomendada para você é a vaga de número " ++ show (Va.numero (fromJust vagaRecomendada)) ++ " no andar " ++ show (Va.andar (fromJust vagaRecomendada))
    else putStrLn "Nao ha vagas recomendadas para seu veiculo"
    putStrLn "------"

    putStrLn "Insira o andar que você deseja estacionar:"
    andarEstacionamento <- readLn :: IO Int
    putStrLn "Insira a vaga que você deseja estacionar:"
    vagaEstacionamento <- readLn :: IO Int

    vagaEscolhida <- getVagaByNumero vagaEstacionamento andarEstacionamento
    
    if Ve.tipo veiculoCliente == Va.tipo vagaEscolhida then do
        if not $ isOcupada vagaEscolhida then do
            estaciona cpfCliente (placa veiculoCliente) vagaEscolhida
            print "Veiculo estacionado"
        else do
            vagasString <- readArquivo vagasArq
            let vagas = map (read :: String -> Vaga) vagasString
            let vagasLivres = vagasStatus vagas (Ve.tipo veiculoCliente)
            if not (null vagasLivres) then do
                putStrLn $ "A vaga escolhida nao esta disponivel, mas voce pode estacionar o veiculo na vaga numero "
                         ++ show (numero $ head vagasLivres) ++ " no andar " ++ show (andar $ head vagasLivres)
                putStrLn "Deseja estacionar nessa vaga? (s/n)"
                opcao <- getLine
                if opcao == "s" then do
                    estaciona cpfCliente (placa veiculoCliente) (head vagasLivres)
                    print "Veiculo estacionado"
                else verificaDisponibilidadeVaga veiculoCliente cpfCliente  
            else 
                putStrLn "Nao ha vagas livres que comportem esse tipo de veiculo"
    else do 
        putStrLn "Seu veiculo nao pode estacionar nessa vaga, porque ela nao comporta veiculos desse tipo"
        verificaDisponibilidadeVaga veiculoCliente cpfCliente  

-- estaciona :: cpfcliente -> placaVeiculo -> numeroVaga -> numeroAndar
-- alterar vaga --- isOcupada = true, placaVeiculo = pv, tempoInicial = posix
estaciona :: String -> String -> Vaga -> IO()
estaciona cpfCliente placaVeiculoEstacionado vagaEscolhida = do
    -- adiciona placaVeiuclo em vaga no bd e seta tempo de agora
    now <- date
    -- atualiza status de ocupação de vaga
    updateDb (show vagaEscolhida) (show (Va.isOcupada vagaEscolhida)) (show True) vagasArq
    -- atualiza tempo inicial de vaga
    vagaAtualizadaStatus <- getVagaById (Va.idVaga vagaEscolhida)
    updateDb (show vagaAtualizadaStatus) (show (Va.tempoInicial vagaAtualizadaStatus)) (show now) vagasArq
    -- atualiza placa de veiculo de vaga
    vagaAtualizadaTempo <- getVagaByNumero (Va.numero vagaEscolhida) (Va.andar vagaEscolhida)
    updateDb (show vagaAtualizadaTempo) (show (Va.placaVeiculo vagaAtualizadaTempo)) (show placaVeiculoEstacionado) vagasArq
    -- adiciona vaga em histórico de cliente
    registraHistorico cpfCliente vagaEscolhida

registraHistorico :: String -> Vaga -> IO ()
registraHistorico cpfCliente vagaEscolhida = do
    historicos <- getHistoricoByCpf cpfCliente
    if null historicos then do -- cria histórico de cliente se não houver
        let historico = Historico cpfCliente [vagaEscolhida]
        addLinha (show historico) historicosArq
    else do -- senão, atualiza histórico
        historicosString <- readArquivo historicosArq
        let historicoCliente = head historicos
        let historicoAtualizado = Historico (H.clienteCpf historicoCliente) (H.numVagas historicoCliente ++ [vagaEscolhida])
        let historicosStringAtualizado = replace historicosString [show historicoCliente] [show historicoAtualizado]
        -- updateByContent historicosArq historicosAtualizados
        writeFileFromList historicosArq historicosStringAtualizado


recomendaVaga :: String -> Veiculo -> IO (Maybe Vaga)
recomendaVaga clienteCpf veiculoCliente = do
    historico <- getHistoricoByCpf clienteCpf
    if null historico then return Nothing
    else do
        let vagasHistorico = [Va.idVaga v | v <- H.numVagas $ head historico, Ve.tipo veiculoCliente == Va.tipo v]
        if null vagasHistorico then return Nothing
        else do
            vaga <- getVagaById $ elementoMaisComum vagasHistorico
            if Va.isOcupada vaga then return Nothing
            else return $ Just vaga


elementoMaisComum :: Ord a => [a] -> a
elementoMaisComum = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort