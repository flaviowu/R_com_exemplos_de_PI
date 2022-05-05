#Comando para buscar músicas#
install.packages("chorrrds")
install.packages("tidyverse")
library(chorrrds)
library(tidyverse)
mutantes<-"mutantes" %>% chorrrds::get_songs() %>% dplyr::sample_n(10)
beyonce<-"beyonce" %>% chorrrds::get_songs() %>% dplyr::sample_n(10)
TomJobim<-"tom-jobim" %>% chorrrds::get_songs() %>% dplyr::sample_n(10)
mutantes
beyonce
TomJobim
