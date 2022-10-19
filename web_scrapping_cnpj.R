# install.packages("remotes")
# remotes::install_github("georgevbsantiago/qsacnpj")

# Instalando e carregando os pacotes necessários -------------------------------
pacotes <- c("tidyverse","qsacnpj", "data.table")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Preparando o ambiente --------------------------------------------------------
options(scipen=999)
set.seed(1)

# Importando as bases de dados das doações para a AMA --------------------------
path <- paste0(getwd(),"/base_ama")
lista_arquivos <- list.files(path = path,
                             recursive = TRUE,
                             pattern = "\\.csv",
                             full.names = TRUE)

base_ama <- readr::read_delim(lista_arquivos)

# Data wrangling - 'base_ama' --------------------------------------------------

# Ajustando os nomes das variáveis
names(base_ama) <- c("cnpj", "emitente", "num", "dt_emissao", "valor_nf", 
                 "dt_registro", "credito", "sit_credito")

# Ajustando o formato das datas
base_ama$dt_emissao <- as.Date(base_ama$dt_emissao, format = "%d/%m/%Y")
base_ama$dt_registro <- as.Date(base_ama$dt_registro, format = "%d/%m/%Y")

# Alterando o sepador de decimal e transformando as variáveis 'valor_nf' e 
# 'credito' em numeric
base_ama$valor_nf <-  gsub(",", ".", base_ama$valor_nf) %>%
        as.numeric()
base_ama$credito <-  gsub(",", ".", base_ama$credito) %>%
        as.numeric()

# Web scrapping de dados públicos de CNPJ. Fonte: Receita Federal --------------
# https://www.gov.br/receitafederal/pt-br/assuntos/orientacao-tributaria/
# cadastros/consultas/dados-publicos-cnpj

# Criando uma função para automatizar o processo de armazenagem
scrapping_func <- function(url, file){
     path_destino <- paste0(getwd(),"/bases_rf/", file)
     utils::download.file(url, path_destino)
}

# Dados sobre referente aos estabelecimentos (principalmente informações de 
# endereço)

scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos0.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 01.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos1.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 02.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos2.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 03.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos3.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 04.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos4.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 05.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos5.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 06.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos6.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 07.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos7.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 08.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos8.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 09.zip")
scrapping_func("http://200.152.38.155/CNPJ/Estabelecimentos9.zip", 
               "Dados Abertos CNPJ ESTABELECIMENTO 10.zip")

# Tabela de atributos referente ao CNAE
scrapping_func("http://200.152.38.155/CNPJ/Cnaes.zip", 
               "Tabela de atributo CNAE.zip")

# Tabela de atributos referente ao código dos Municípios
scrapping_func("http://200.152.38.155/CNPJ/Municipios.zip", 
               "Tabela de atributo Município.zip")

# Data wrangling - base de dados publica dos estabelecimentos ------------------

# Após descompactar os arquivos e separá-los em pastas, seguimos com a 
# manipulação dos dados

# Importando para o R a base de estabelecimentos
path_estabelecimento <- paste0(getwd(),"/bases_rf_estabelecimento")

lista_arquivos_estabelecimento <- list.files(path = path_estabelecimento,
                                     recursive = TRUE,
                                     pattern = "\\.ESTABELE",
                                     full.names = TRUE)

# Função que realiza a leitura dos arquivos referente aos dados dos 
# estabelecimentos
ler_bd_estabelecimento <- function(base,n){

   base <- readr::read_delim(paste0(path_estabelecimento, "/",  base),
                        delim = ";", escape_double = FALSE, col_names = FALSE,
                        trim_ws = TRUE)

   names(base) <- c("CNPJ BÁSICO","CNPJ ORDEM",
                    "CNPJ DV","IDENTIFICADOR MATRIZ/FILIAL",
                    "NOME FANTASIA","SITUAÇÃO CADASTRAL",
                    "DATA SITUAÇÃO CADASTRAL","MOTIVO SITUAÇÃO CADASTRAL",
                    "NOME DA CIDADE NO EXTERIOR","PAIS",
                    "DATA DE INÍCIO ATIVIDADE","CNAE FISCAL PRINCIPAL",
                    "CNAE FISCAL SECUNDÁRIA","TIPO DE LOGRADOURO",
                    "LOGRADOURO","NÚMERO",
                    "COMPLEMENTO",
                    "BAIRRO","CEP",
                    "UF","MUNICÍPIO",
                    "DDD 1","TELEFONE 1",
                    "DDD 2","TELEFONE 2",
                    "DDD DO FAX","FAX",
                    "CORREIO ELETRÔNICO","SITUAÇÃO ESPECIAL",
                    "DATA DA SITUAÇÃO ESPECIAL")

   # Aplicando a descrição das variáveis apresentada no layout disponibilizado
   # pela Receita Federal
   base$`IDENTIFICADOR MATRIZ/FILIAL` <- 
     ifelse(base$`IDENTIFICADOR MATRIZ/FILIAL` == 1, "MATRIZ",
            ifelse(base$`IDENTIFICADOR MATRIZ/FILIAL` == 2, "FILIAL",NA))

   base$`SITUAÇÃO CADASTRAL` <- 
     ifelse(base$`SITUAÇÃO CADASTRAL` == "01", "NULA",
            ifelse(base$`SITUAÇÃO CADASTRAL` == "02", "ATIVA",
                   ifelse(base$`SITUAÇÃO CADASTRAL` == "03", "SUSPENSA",
                          ifelse(base$`SITUAÇÃO CADASTRAL` == "04", "INAPTA",
                                 ifelse(base$`SITUAÇÃO CADASTRAL` == "08", 
                                        "BAIXADA",NA)))))
   
   # Salvando a base de dados pré-processada
   write.csv(base, paste0("bases_rf_prontas/base_estabelecimento", n,".csv"), 
             row.names = F)
}

# Lendo os banco de dados dos estabelecimentos
ler_bd_estabelecimento("K3241.K03200Y0.D20514.ESTABELE", 0)
ler_bd_estabelecimento("K3241.K03200Y1.D20514.ESTABELE", 1)
ler_bd_estabelecimento("K3241.K03200Y2.D20514.ESTABELE", 2)
ler_bd_estabelecimento("K3241.K03200Y3.D20514.ESTABELE", 3)
ler_bd_estabelecimento("K3241.K03200Y4.D20514.ESTABELE", 4)
ler_bd_estabelecimento("K3241.K03200Y5.D20514.ESTABELE", 5)
ler_bd_estabelecimento("K3241.K03200Y6.D20514.ESTABELE", 6)
ler_bd_estabelecimento("K3241.K03200Y7.D20514.ESTABELE", 7)
ler_bd_estabelecimento("K3241.K03200Y8.D20514.ESTABELE", 8)
ler_bd_estabelecimento("K3241.K03200Y9.D20514.ESTABELE", 9)

# Padronizando a variável CNPJ, para posterior merge entre a base de doações
# com os dados dos estabelecimentos
base_ama$cnpj <- gsub("-", "", base_ama$cnpj)
base_ama$cnpj <- gsub("/", "", base_ama$cnpj)
base_ama$cnpj <- gsub("\\.", "", base_ama$cnpj)
cnpj_ama <- base_ama$cnpj %>% unique() %>% as.character()
min(base_ama$dt_emissao)

# Criando função para preparar os banco de dados dos estabelecimentos para o 
# futuro 'merge' com o dataset de doações
ajustar_bd_estabelecimento <- function(base, cnpj_ama){
        base <- data.table::fread(paste0("bases_rf_prontas/", base),
                                  nThread = 4, colClasses = 'character')
        base$`DATA SITUAÇÃO CADASTRAL` <- 
          as.Date(base$`DATA SITUAÇÃO CADASTRAL`, "%Y%m%d")
        base$`DATA DE INÍCIO ATIVIDADE` <-
          as.Date(base$`DATA DE INÍCIO ATIVIDADE`, "%Y%m%d")
        base <- subset(base, UF == "SP")
        base <- subset(base,
                       !(`SITUAÇÃO CADASTRAL` != "ATIVA" & `DATA SITUAÇÃO CADASTRAL` <= "2017-01-01"))
        base$cnpj <- paste0(base$`CNPJ BÁSICO`, base$`CNPJ ORDEM`,
                            base$`CNPJ DV`)
        base_ama_estab <- subset(base, cnpj %in% cnpj_ama)
        base_n_ama <- subset(base, !(cnpj %in% cnpj_ama))
        base <- NULL
        return(list(base_ama_estab,base_n_ama))
}

# Aplicando a função 'ajustar_bd_estabelecimento'
estabelecimento_0 <- ajustar_bd_estabelecimento("base_estabelecimento0.csv", cnpj_ama)
gc()
estabelecimento_1 <- ajustar_bd_estabelecimento("base_estabelecimento1.csv", cnpj_ama)
gc()
estabelecimento_2 <- ajustar_bd_estabelecimento("base_estabelecimento2.csv", cnpj_ama)
gc()
estabelecimento_3 <- ajustar_bd_estabelecimento("base_estabelecimento3.csv", cnpj_ama)
gc()
estabelecimento_4 <- ajustar_bd_estabelecimento("base_estabelecimento4.csv", cnpj_ama)
gc()
estabelecimento_5 <- ajustar_bd_estabelecimento("base_estabelecimento5.csv", cnpj_ama)
gc()
estabelecimento_6 <- ajustar_bd_estabelecimento("base_estabelecimento6.csv", cnpj_ama)
gc()
estabelecimento_7 <- ajustar_bd_estabelecimento("base_estabelecimento7.csv", cnpj_ama)
gc()
estabelecimento_8 <- ajustar_bd_estabelecimento("base_estabelecimento8.csv", cnpj_ama)
gc()
estabelecimento_9 <- ajustar_bd_estabelecimento("base_estabelecimento9.csv", cnpj_ama)

# A partir desse ponto do código irei separar os datasets em 2 categorias:
# ama: é composta pelos estabelecimentos onde houve doação para a AMA.
# n_ama: todo o restante

estabelecimento_ama <- do.call(rbind, list(estabelecimento_0[[1]],
                                 estabelecimento_1[[1]],
                                 estabelecimento_2[[1]],
                                 estabelecimento_3[[1]],
                                 estabelecimento_4[[1]],
                                 estabelecimento_5[[1]],
                                 estabelecimento_6[[1]],
                                 estabelecimento_7[[1]],
                                 estabelecimento_8[[1]],
                                 estabelecimento_9[[1]]))

estabelecimento_n_ama <- do.call(rbind, list(estabelecimento_0[[2]],
                                   estabelecimento_1[[2]],
                                   estabelecimento_2[[2]],
                                   estabelecimento_3[[2]],
                                   estabelecimento_4[[2]],
                                   estabelecimento_5[[2]],
                                   estabelecimento_6[[2]],
                                   estabelecimento_7[[2]],
                                   estabelecimento_8[[2]],
                                   estabelecimento_9[[2]]))

estabelecimento_0 <- NULL
estabelecimento_1 <- NULL
estabelecimento_2 <- NULL
estabelecimento_3 <- NULL
estabelecimento_4 <- NULL
estabelecimento_5 <- NULL
estabelecimento_6 <- NULL
estabelecimento_7 <- NULL
estabelecimento_8 <- NULL
estabelecimento_9 <- NULL

base_ama_estab <- merge(base_ama, estabelecimento_ama, by = "cnpj", all = T)

base_ama_estab <- base_ama_estab %>%
        dplyr::select("cnpj",
                      "emitente",
                      "num",
                      "dt_emissao",
                      "valor_nf",
                      "dt_registro",
                      "credito",
                      "sit_credito",
                      "identificador" = "IDENTIFICADOR MATRIZ/FILIAL",
                      "situacao_cadastral" = "SITUAÇÃO CADASTRAL",
                      "dt_situacao_cadastral" = "DATA SITUAÇÃO CADASTRAL",
                      "dt_inicio_atividades" = "DATA DE INÍCIO ATIVIDADE",
                      "cnae_principal" = "CNAE FISCAL PRINCIPAL",
                      "cnae_secundaria" = "CNAE FISCAL SECUNDÁRIA",
                      "tipo_logradouro" = "TIPO DE LOGRADOURO",
                      "logradouro" = "LOGRADOURO",
                      "num_logradouro" = "NÚMERO",
                      "complemento" = "COMPLEMENTO",
                      "bairro" = "BAIRRO",
                      "cep" = "CEP",
                      "uf" = "UF",
                      "municipio" = "MUNICÍPIO"
        )

base_n_ama <- estab_n_ama %>%
        dplyr::select("cnpj",
                      "identificador" = "IDENTIFICADOR MATRIZ/FILIAL",
                      "situacao_cadastral" = "SITUAÇÃO CADASTRAL",
                      "dt_situacao_cadastral" = "DATA SITUAÇÃO CADASTRAL",
                      "dt_inicio_atividades" = "DATA DE INÍCIO ATIVIDADE",
                      "cnae_principal" = "CNAE FISCAL PRINCIPAL",
                      "cnae_secundaria" = "CNAE FISCAL SECUNDÁRIA",
                      "tipo_logradouro" = "TIPO DE LOGRADOURO",
                      "logradouro" = "LOGRADOURO",
                      "num_logradouro" = "NÚMERO",
                      "complemento" = "COMPLEMENTO",
                      "bairro" = "BAIRRO",
                      "cep" = "CEP",
                      "uf" = "UF",
                      "municipio" = "MUNICÍPIO"
        )

write.csv(base_ama_estab, "base_ama_estab.csv", row.names = F)
write.csv(base_n_ama, "base_n_ama.csv", row.names = F)