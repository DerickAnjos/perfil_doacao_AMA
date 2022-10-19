# Instalando e carregando os pacotes necessários -------------------------------
pacotes <- c("osmdata","dplyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Lendo a base de dados de doações já com as informações dos estabelecimentos
base_ama <- read.csv('base_ama_estab.csv', row.names = NULL, sep = ",")

# Lendo a tabela de atributos com os códigos dos municípios
cod_municipio <- read.csv('cod_municipio.csv',  row.names = NULL, sep = ";")
cod_municipio <- cod_municipio[-2]
names(cod_municipio) <- c('nome_municipio', 'municipio')

# Agregando o nome dos municípios ao dataset de doações
base_ama <- left_join(base_ama, cod_municipio, by = 'municipio')  

# Criando uma variável que armazena o endereço na granularidade 'bairro'
base_ama$endereco <- paste(base_ama$bairro, base_ama$nome_municipio,
                           sep = ' ')

# Com o intuito de poupar processamento, essa é uma etapa para preparar um 
# dataset de dados únicos dos estabelecimentos onde foram geradas doações para a
# AMA
base_endereco <- base_ama[, c('endereco', 'bairro', 'nome_municipio')]
endereco_long_lat <- unique(base_endereco)

# Aplicando a função 'getbb' para adquirir os valores correspondentes de 
# latitude e longitude dos endereços
for(i in 1:nrow(endereco_long_lat)){
  
  coord <- getbb(endereco_long_lat[i,1])
  
  if(is.na(coord)){
    coord <- getbb(endereco_long_lat$bairro[i])
    coord_mun <- getbb(endereco_long_lat$nome_municipio[i])
    
    if(sum(is.na(c(coord, coord_mun)))>=1) {
      
      next
      
    } else {
    
      if(coord[1] >= coord_mun[1] & coord[3] <= coord_mun[3] & 
         coord[2] >= coord_mun[2] & coord[4] <= coord_mun[4]) {
        
        endereco_long_lat$latitude[i] <- mean(c(coord[2],coord[4]))
        endereco_long_lat$longitude[i] <- mean(c(coord[1],coord[3]))
        
        next
      }
      
    }
    next
  }
  endereco_long_lat$latitude[i] <- mean(c(coord[2],coord[4]))
  endereco_long_lat$longitude[i] <- mean(c(coord[1],coord[3]))
  print(i)
}

# Selecionando as variáveis de interesse do dataset 'endereco_long_lat'
endereco_long_lat <- endereco_long_lat %>% 
  dplyr::select(endereco, bairro, nome_municipio, longitude, latitude)

# Unindo o dataset com as informações de geolocalização ao dataset de doações
base_ama <- left_join(base_ama, endereco_long_lat, by = 'endereco')

# Salvando os datasets outputs desse código, o dataset de endeços únicos e o 
# dataset principal, que combina as informações de doação, estabelecimento e 
# geolocalização
save(endereco_long_lat, file = 'endereco_long_lat.RData')
save(base_ama, file = 'base_ama_estab_long_lat.RData')