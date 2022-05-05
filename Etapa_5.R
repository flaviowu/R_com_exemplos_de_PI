library(readxl)

guerras <- read_excel("R_com_exemplos_de PI/Bases_de_Dados/guerras.xlsx")
View(guerras)

attach(guerras)
tapply(mortes, pais, sum)
str(guerras)
library(summarytools)

freq(pais, cumul = TRUE, totals = FALSE, order = "freq")
descr(guerras[,6:7], style='rmarkdown')
descr(guerras[,6:7], style='grid')

brazil<- subset(guerras, pais=="Brazil")
descr(brazil[,6:7], style='rmarkdown')
