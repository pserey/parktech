module Model.Cliente where
    data Cliente = Cliente {
        cpf :: String,
        nome :: String,
        aceitaHistorico :: Bool
    } deriving (Show, Read)