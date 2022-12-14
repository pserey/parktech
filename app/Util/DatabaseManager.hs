module Util.DatabaseManager where

import Data.List (isInfixOf)
import Data.Text (pack)
import Data.Text.Internal.Search (indices)
import Data.List (intercalate)
import System.IO
import System.IO.Strict as S
import Model.Vaga

-- Função que adiciona uma string como linha ao arquivo de nome especificado.
addLinha :: String -> String -> IO()
addLinha conteudo arquivo = do
    let conteudoFinal = conteudo ++ "\n"
    meuAppend arquivo conteudoFinal
    -- appendFile arquivo conteudoFinal


{- | Função que lê arquivo de nome especificado e retorna uma lista de
strings contendo as linhas do arquivo.
-}
readArquivo :: String -> IO [String]
readArquivo arquivoNome = do
    arquivo <- openFile arquivoNome ReadMode
    conteudo <- S.hGetContents arquivo
    return $ lines conteudo


-- Função que procura uma string em um arquivo.
findByStr :: String -> String -> IO String
findByStr str arquivoNome = do
    conteudo <- readArquivo arquivoNome
    findByRecursivo conteudo str


{- | Função recursiva que percorre uma lista de strings (normalmente linhas
do arquivo) e procura uma substring especificada.

Retorna erro caso não ache a substring em nenhuma linha do arquivo.
-}
findByRecursivo :: [String] -> String -> IO String
findByRecursivo [] str = error "Not found"
findByRecursivo (x:xs) str = do
    if str `isInfixOf` x then return x
    else findByRecursivo xs str


-- -- função que busca um valor em uma string em formato de Show Object
-- -- retorna o valor com "" se for string e sem se for inteiro
-- getValor :: String -> String -> IO String
-- getValor chave linha = do
--     -- acha indice do nome do objeto e remove nome da string
--     let idxObj = head $ indices (pack "{") (pack linha)
--     let mapa = drop idxObj linha

--     -- acha indice de chave e remove chave
--     let idxChave = head $ indices (pack chave) (pack mapa)
--     let mapaComecaChave = drop (idxChave + length chave + 3) mapa

--     -- acha indice final de valor
--     -- se tiver uma virgula na frente, o indice final é o da virgula
--     -- senão, o indice final é o da chave fechando
--     let idxFinal = if null $ indices (pack ",") (pack mapaComecaChave) then 
--                     head $ indices (pack "}") (pack mapaComecaChave)
--                     else head $ indices (pack ",") (pack mapaComecaChave)

--     -- remove string na frente do valor
--     -- acha valor
--     let valor = take idxFinal mapaComecaChave

--     -- let valor = if head valor
--     return valor


meuAppend :: String -> String -> IO ()
meuAppend arquivo conteudo = do
    arq <- S.readFile arquivo
    writeFile arquivo (arq ++ conteudo)


updateByContent :: Show t => String -> [t] -> IO ()
updateByContent arquivoNome modelUpdate = do
    fileHandle <- openFile arquivoNome WriteMode
    updateByContentRecursivo modelUpdate fileHandle
    hFlush fileHandle
    hClose fileHandle


updateByContentRecursivo :: Show t => [t] -> Handle -> IO ()
updateByContentRecursivo [] _ = return ()
updateByContentRecursivo (model:modelTail) fileHandle = do
    let linha = show model
    hPutStrLn fileHandle linha
    updateByContentRecursivo modelTail fileHandle


-- Função que checa se um objeto em string (show) está dentro de um arquivo especifico
checkIfExist :: String -> String -> IO Bool
checkIfExist line arq = do
  linhas <- S.readFile arq
  return $ line `isInfixOf` linhas


-- funcao que faz o replace
-- recebe: estrutura que será alterada -> valor existente -> valor novo
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
  if take (length find) s == find
    then repl ++ replace (drop (length find) s) find repl
    else head s : replace (tail s) find repl

-- função que atualiza arquivo com lista de objetos
-- converte objetos com show e cria um novo arquivo separado por linhas
writeFileFromList :: String -> [String] -> IO ()
writeFileFromList nomeArquivo conteudos = do
    let dbString = intercalate "\n" conteudos
    writeFile nomeArquivo (dbString ++ "\n")

-- função que substitui elemento em linha do db usando replace
updateDb :: String -> String -> String -> String -> IO ()
updateDb linha elemento elementoNovo nomeArq = do
    db <- S.readFile nomeArq

    -- atualiza elemento em linha
    let linhaAtualizada = replace linha elemento elementoNovo
    -- atualiza linha em db
    -- S.readFile retorna toda \ com \\ na frente, impossibilitando que read :: Vaga funcione, por isso se usa replace para isso também
    let dbAtualizado = replace db linha linhaAtualizada

    -- reescreve db
    writeFile nomeArq dbAtualizado
    -- writeFileFromList nomeArq dbAtualizado