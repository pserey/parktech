module Model.Cliente ( Cliente(Cliente) ) where
    data Cliente = Cliente {
        cpf :: String,
        nome :: String
    } deriving (Show, Read)