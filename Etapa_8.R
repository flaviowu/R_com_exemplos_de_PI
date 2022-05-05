# install.packages("ggplot2")
library(ggplot2)
library(readxl)

vetos_CSONU <- read_excel("R_com_exemplos_de PI/Bases_de_Dados/vetos_CSONU.xls")

ggplot(vetos_CSONU, aes(fill=anos, y=pais, x=vetos)) + geom_bar(position="stack", stat="identity")+ 
  labs(title = "Vetos das Potências no CSONU ",
       subtitle = "Fonte: Dyson (2013)",
       x = "Numero de Vetos",
       y = "Potencias Poder de Veto")
