module Model.Vaga where
    data Vaga = Vaga {
        id :: String,
        isOcupada :: Boolean,
        numero :: Int,
        andar :: Int,
        tempoInicial :: Int
    } deriving (Show, Read)