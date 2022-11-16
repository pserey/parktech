module Database.DatabaseManager where
import System.IO
import Data.List


-- Função que adiciona uma string como linha ao arquivo de nome especificado.
addLinha :: String -> String -> IO()
addLinha conteudo arquivo = do
    let conteudoFinal = conteudo ++ "\n"
    appendFile arquivo conteudoFinal


-- Função que lê arquivo de nome especificado e retorna uma lista de
-- strings contendo as linhas do arquivo.
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


-- Função recursiva que percorre uma lista de strings (normalmente linhas
-- do arquivo) e procura uma substring especificada.
-- retorna erro caso não ache a substring em nenhuma linha do arquivo.
findByRecursivo:: [String] -> String -> IO String
findByRecursivo [] str = error "Not found"
findByRecursivo (x:xs) str = do
    if str `isInfixOf` x then return x
    else findByRecursivo xs str