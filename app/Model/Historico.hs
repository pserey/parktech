module Model.Historico where 
    import Model.Vaga
    data Historico = Historico {
        clienteCpf :: String,
        numVagas :: [Vaga]
    } deriving (Show, Read)