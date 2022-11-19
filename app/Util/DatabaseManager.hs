module Util.DatabaseManager where

import System.IO ( hGetContents, openFile, IOMode(ReadMode, ReadWriteMode), hFlush, hClose, hPutStr )
-- import System.IO.Strict (appendFile)
import Data.List ( isInfixOf )
import Data.Text (pack)
import Data.Text.Internal.Search (indices)


-- Função que adiciona uma string como linha ao arquivo de nome especificado.
addLinha :: String -> String -> IO()
addLinha conteudo arquivo = do
    let conteudoFinal = conteudo ++ "\n"
    appendFile arquivo conteudoFinal


{- | Função que lê arquivo de nome especificado e retorna uma lista de
strings contendo as linhas do arquivo.
-}
readArquivo :: String -> IO [String]
readArquivo arquivoNome = do
    arquivo <- openFile arquivoNome ReadMode
    conteudo <- hGetContents arquivo
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


-- função que busca um valor em uma string em formato de Show Object
-- retorna o valor com "" se for string e sem se for inteiro
getValor :: String -> String -> IO String
getValor chave linha = do
    -- acha indice do nome do objeto e remove nome da string
    let idxObj = head $ indices (pack "{") (pack linha)
    let mapa = drop idxObj linha

    -- acha indice de chave e remove chave
    let idxChave = head $ indices (pack chave) (pack mapa)
    let mapaComecaChave = drop (idxChave + length chave + 3) mapa

    -- acha indice final de valor
    -- se tiver uma virgula na frente, o indice final é o da virgula
    -- senão, o indice final é o da chave fechando
    let idxFinal = if null $ indices (pack ",") (pack mapaComecaChave) then 
                    head $ indices (pack "}") (pack mapaComecaChave)
                    else head $ indices (pack ",") (pack mapaComecaChave)

    -- remove string na frente do valor
    -- acha valor
    let valor = take idxFinal mapaComecaChave

    -- let valor = if head valor
    return valor