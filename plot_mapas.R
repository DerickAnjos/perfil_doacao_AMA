# Instalando e carregando os pacotes necessários -------------------------------
pacotes <- c("tidyverse","sf","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra", 'gganimate', 'paletteer', 'ggmap', 'gifski', 
             'ggplot2', 'lubridate')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando as bases de dados, sendo a primeira de endeços únicos e a outra, o
# dataset principal, que combina as informações de doação, estabelecimento e 
# geolocalização
load("base_ama_estab_long_lat.RData")
load("endereco_long_lat.RData")

# Análise espacial dos dados ---------------------------------------------------

# Importando um objeto do tipo 'sp' do Estado de SP
sao_paulo_rgdal <- readOGR(dsn="shapefile_sp", layer="estado_sp", 
                           verbose=FALSE, stringsAsFactors=FALSE, 
                           encoding = "UTF-8")

# Transformando em um objeto do tipo 'Data frame'
sao_paulo <- fortify(model=sao_paulo_rgdal, region="CD_GEOCMU")

# Preparando uma coluna para destacar a capital e a região metropolitana no 
# mapa de SP
regiao_metropolitana_sp <- c('3503901', '3505708', '3506607', '3509007', '3509205',
                             '3510609', '3513009', '3513801', '3515004', '3515103',
                             '3515707', '3516309', '3516408', '3518305', '3518800',
                             '3522208', '3522505', '3523107', '3525003', '3526209', 
                             '3528502', '3529401', '3530607', '3534401', '3539103',
                             '3539806', '3543303', '3544103', '3545001', '3546801',
                             '3547304', '3547809', '3548708', '3548708', '3549953',
                             '3552502', '3552809', '3556453')

sao_paulo$grande_sp <- 
  ifelse(sao_paulo$id %in% regiao_metropolitana_sp, "1", 
         ifelse(sao_paulo$id == '3550308', '2','0'))

# Plotando o mapa de SP, destacando a região metropolitana e a capital
ggplot(data = sao_paulo, mapping = aes(x = long, y = lat,  group = id)) +
  geom_polygon(aes(fill = grande_sp), color = "black") +
  scale_fill_manual(values = c('white', 'mediumseagreen', 'darkslategrey')) +
  coord_quickmap() +
  theme_void()+
  theme(legend.position = 'none')

# Criando uma Região de Interesse (ROI)
min_long <- min(sao_paulo$long)
max_long <- max(sao_paulo$long)
min_lat <- min(sao_paulo$lat)
max_lat <- max(sao_paulo$lat)

# ROI plotada sobre o mapa de SP
ggplot(data = sao_paulo, mapping = aes(x = long, y = lat,  group = id)) +
  geom_polygon(aes(fill = grande_sp), color = "black") +
  scale_fill_manual(values = c('white', 'mediumseagreen', 'darkslategrey')) +
  coord_quickmap() +
  geom_hline(yintercept = c(max_lat, min_lat), color = 'aquamarine4',
             size = 1.5, linetype = 'dotdash') +
  geom_vline(xintercept = c(max_long, min_long), color = 'aquamarine4', 
             size = 1.5, linetype = 'dotdash') +
  theme_void()+
  theme(legend.position = 'none')


# Aplicando a ROI criada ao banco de dados de doações, eliminando assim os 
# outliers (3.980 observações eliminadas)
base_ama <- base_ama[base_ama$latitude > min_lat  & 
                           base_ama$latitude < max_lat & 
                           base_ama$longitude > min_long  & 
                           base_ama$longitude < max_long, ]

# Agregando o banco de dados, utilizando o endereço como elemento de agregação
# e somando os valores dos créditos destinados à AMA
credito_localizacao <- aggregate(x = base_ama$credito, 
                                 by = list(base_ama$endereco), 
                                 FUN = sum)

# Alterando os nomes das variáveis
names(credito_localizacao) <- c('endereco', 'credito')

# Unindo os datasets com as informações dos créditos gerados por endereço e o 
# dataset com as geolocalizações
credito_localizacao <- left_join(credito_localizacao, endereco_long_lat, 
                                 by = 'endereco') 

# Gerando o primeiro mapa (camada base)
base_map <- ggplot(data = sao_paulo, mapping = aes(x = long, y = lat, 
                                                   group = id)) +
  geom_polygon(color = "black", fill = "white") +
  coord_quickmap() +
  theme_void() 

# Visualizando o camada base
base_map

# Adicionando o layer com as informações das doações
map_with_data <- base_map +
  geom_point(data = credito_localizacao, aes(x = longitude, y = latitude,
                                             color = credito, size = credito,
                                             group= c(credito)))+
  scale_color_viridis_b(alpha = 0.5) +
  labs(color =  'Crédito (R$)', size = "")+
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))

# Visualizando o mapa
map_with_data


# Sedes da AMA em SP -----------------------------------------------------------

# Unidade Cambuci
ama_cambuci <- c(-46.62172884973312, -23.561902060377005)

# Unidade Parelheiros
ama_parelheiros <- c(-46.712624157571234, -23.836588578265975)

# Unidade Parelheiros
ama_creapp <- c(-46.724056609582526, -23.486140368526332)

df_unidades_ama <- tibble(unidade = c('Cambuci', 'Parelheiros', 'Creapp'),
                          longitude = c(ama_cambuci[1], ama_parelheiros[1],
                                        ama_creapp[1]), 
                          latitude = c(ama_cambuci[2], ama_parelheiros[2],
                                       ama_creapp[2]))

# Adicionando o layer com as informações das doações e as sedes
map_with_data_sede <- map_with_data +
  geom_point(data = df_unidades_ama, aes(x = longitude, y = latitude,
                                         group = NULL), color = 'red')
# Visualizando o mapa
map_with_data_sede


# Análise temporal das doações -------------------------------------------------

# Agregando o banco de dados, utilizando o endereço e a data como elemento de 
# agregação, e somando os valores dos créditos destinados à AMA
credito_localizacao_data <- aggregate(x = base_ama$credito, 
                                 by = list(base_ama$endereco,
                                           base_ama$dt_emissao), 
                                 FUN = sum)

# Alterando os nomes das variáveis
names(credito_localizacao_data) <- c('endereco', 'data', 'credito')

# Ajustando o formato das data
credito_localizacao_data$year <- year(credito_localizacao_data$data)

credito_localizacao_data <- aggregate(x = credito_localizacao_data$credito, 
                                      by = list(credito_localizacao_data$endereco, 
                                                credito_localizacao_data$year),
                                      FUN = sum)

# Alterando os nomes das variáveis
names(credito_localizacao_data) <- c('endereco', 'data', 'credito')

# Unindo os datasets com as informações dos créditos gerados por endereço e o 
# dataset com as geolocalizações
credito_localizacao_data <- left_join(credito_localizacao_data, 
                                      endereco_long_lat, by = 'endereco') 

map_with_data_temp <- base_map +
  geom_point(data = credito_localizacao_data[credito_localizacao_data$data == 
                                               2021,],
             aes(x = longitude, y = latitude, color = credito, size = credito,
                 group= c(credito)))+
  scale_color_viridis_b(alpha = 0.5) +
  labs(color =  'Crédito (R$)', size = "")+
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))

map_with_data_temp

?'theme'
# Criando um vídeo (gif) com a progressão das doações ao longo do período de 
# análise
map_with_animation <- map_with_data +
  transition_time(data) +
  ggtitle('Data: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')

# Definindo o número de frames
num_months <- 5*12 # 5 anos

# Gerando a animação
map_with_shadow <- map_with_animation +
  shadow_mark()

# Visualizando a animação
animate(map_with_shadow, nframes = num_months, fps = 2)

# Salvando a animação
anim_save("credito_sp.gif", map_with_shadow, nframes = num_months, fps = 3)