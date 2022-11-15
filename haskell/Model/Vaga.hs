module Model.Vaga where
    data Vaga = Vaga {
        isOcupada :: Bool,
        numero :: Int,
        andar :: Int,
        tipo :: String,
        tempoInicial :: Int,
        id :: String
    } deriving (Show, Read)