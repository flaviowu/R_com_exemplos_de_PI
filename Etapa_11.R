# Mapas Dinamicos - Emprestimo BNDES e Cooperacao Brasileira - Reservas e Producao de Petroleo

# Instalacao de alguns pacotes - nem todos serao usados, mas esses pacotes oferecem muitas opcoes para explorar

# install.packages(c("raster", "dplyr", "spData", "spDataLarge", "sf"))
# install.packages(c("leaflet", "shiny"))
# install.packages("readxl")
# install.packages("htmlwidgets")
# install.packages("magick")
# install.packages("here")
# install.packages("tmap")
# install.packages("ggplot2")
# install.packages("gifski")

# Importacao dos pacotes
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)
library(readxl)
library(htmlwidgets)
library(magick)
library(here)
library(leaflet) 
library(ggplot2) 
library(shiny)   
library(sf)
library(raster)
library(gifski)

# Importa o arquivo mapa com dados do BNDES e Cooperacao Brasileira - vamos chamar o objeto de br

br <- read_excel("./R_com_exemplos_de_PI/Bases_de_Dados/mapa.xls", col_types = c("text","text","numeric","numeric"))

# importa o mapa mundi
data("World")

# Ordena pelo nome do pais nas duas bases
World <- World[order(World$iso_a3),]
br <- br[order(br$iso_a3),]

#Junta as bases de dados criando o objeto br_2
br_2 <-merge(World, br)

# Cria o mapa com duas variaveis: loans_2 (emprestimos do BNDES) e "coop_2" (cooperacao bilateral do Brasil)#

tm_shape(br_2) +
  tm_polygons("loans_2", style = "fixed",breaks = c(1, 309, 2800, 7127, 15783, 24360, 34811), textNA = "No Loans", colorNA = "white", title = "BNDES Loans(100 thousands R$)") +
  tm_symbols(col = "black",  style="jenks", border.col = "white", size = "coop_2", scale=1.0, title.size = "Bilateral Cooperation(100thousands R$)")+
  tm_layout(legend.outside = TRUE)

# Importa a base reservas em excel que possui a produção e reservas para quatro décadas#

reservas <- read_excel("./R_com_exemplos_de_PI/Bases_de_Dados/reservas.xls", col_types = c("numeric","numeric","numeric","numeric","text", "numeric","numeric","numeric","numeric"))

# Checa se os nomes dos paises na base em excel e no mapa World sao iguais
identical(World$name,reservas$name)

#Ordena as duas bases pelo nome do pais 
World <- World[order(World$name),]
reservas <- reservas[order(reservas$name),]

# Junta as bases de dados criando o objeto map
map <-merge(World, reservas)

# Salva mapa dinamico em html
petro=tm_shape(map) +
  tm_polygons("p_2016", title = "Producao Petroleo") +
  tm_symbols(col = "black", border.col = "white", size = "r_2016", scale=1.2)
tmap_save(tm = petro, filename = "./petroleo.html", width = 1200, height = 1000)


# Criacao de GIF com a evolucao da producao e reservas de petroleo por decada (1986-2016)
# Novo banco de dados denominado reservas_din

reservas_gif <- read_excel("./R_com_exemplos_de_PI/Bases_de_Dados/reservas_din.xls", col_types = c("text", "numeric","numeric","numeric"))

# Mesmos comandos
identical(World$name,reservas_gif$name)
World <- World[order(World$name),]
reservas_gif <- reservas_gif[order(reservas_gif$name),]
map2 <-merge(World, reservas_gif)

# Cria quatro mapas, um para cada decada
petroleo_gif = tm_shape(map2) + tm_polygons("producao") + tm_dots(size = "reservas") +
  tm_facets(along = "year", free.coords = FALSE, ncol = 1, nrow=1, as.layers = TRUE, free.scales.symbol.size = FALSE)

# Gera o arquivo de animacao em gif. Note que e preciso ter um app instalado para o R fazer a animacao: https://imagemagick.org/script/download.php#windows#
tmap_animation(petroleo_gif, filename = "./R_com_exemplos_de_PI/Bases_de_Dados/petroleo_2.gif", delay = 100, width = 1800, height = 1000)

# Mapa estatico com as 4 decadas
petroleo_decada = tm_shape(map2) + tm_polygons("producao") + tm_dots(size = "reservas") +
  tm_facets(by = "year", free.coords = FALSE, ncol = 2, nrow=2, as.layers = FALSE, free.scales.symbol.size = FALSE)
petroleo_decada