# Parktech
Sistema de gerenciamento de estacionamento.

## Pre-requisitos

O projeto foi gerenciado com Stack, é importante que `stack` esteja instalado em sua máquina. Além disso, o GHC na versão 9.2.4 garante que as dependências de pacotes funcionem sem erros.

A ferramenta `make` também é necessária caso o uso do Makefile seja feito. Senão, `stack build parktech` e `stack exec parktech-exe` devem funcionar perfeitamente para a execução do código.

## Rodando
Para rodar o sistema, um *Makefile* foi criado.

```
# para fazer o build e rodar o código
make run

# para apenas fazer o build
make build
```

## Banco de dados
O banco de dados do projeto é baseado em arquivos `.txt` e pode ser acessado no diretório `app/db/` do mesmo. Os banco de dados já foram preenchidos durante a testagem do código e foram comitados preenchidos. Entretanto, é importante que caso modificações do bd sejam feitas editando os arquivos, esquecer da quebra de linha (`"\n"`) no final do arquivo pode causar com que o código quebre com erros de parse.