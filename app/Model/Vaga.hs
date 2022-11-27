module Model.Vaga where
    data Vaga = Vaga {
        isOcupada :: Bool,
        numero :: Int,
        andar :: Int,
        tipo :: String,
        tempoInicial :: Integer,
        idVaga :: String,
        placaVeiculo :: String
    } deriving (Show, Read)