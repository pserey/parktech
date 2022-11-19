module Model.Vaga ( Vaga(Vaga) ) where
    data Vaga = Vaga {
        isOcupada :: Bool,
        numero :: Int,
        andar :: Int,
        tipo :: String,
        tempoInicial :: Integer,
        id :: String
    } deriving (Show, Read)