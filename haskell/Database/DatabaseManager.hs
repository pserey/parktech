module Database.DatabaseManager where
import System.IO
import Data.List


addLinha :: String -> String -> IO()
addLinha conteudo arquivo = do
    let conteudoFinal = conteudo ++ "\n"
    appendFile arquivo conteudoFinal


readFile :: String -> IO [String]
readFile arquivoNome = do
    arquivo <- openFile arquivoNome ReadMode
    conteudo <- hGetContents arquivo
    return $ lines conteudo


findBy :: String -> String -> IO String
findBy id arquivoNome = do
    arquivo <- openFile arquivoNome ReadMode
    conteudo <- hGetContents arquivo
    let lista = lines conteudo
    findByRecursivo lista id


findByRecursivo:: [String] -> String -> String
findByRecursivo (x:xs) conteudo = do
    if isInfixOf conteudo x then
        return x                                       -- TODO: verificar erro de tipo String/Char
    else do
        findByRecursivo xs conteudo
