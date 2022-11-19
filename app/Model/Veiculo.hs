module Model.Veiculo ( Veiculo(Veiculo) ) where
    data Veiculo = Veiculo {
        tipo :: String,
        placa :: String,
        cor :: String
    } deriving (Show, Read)