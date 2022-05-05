library(readxl)

tropas_ONU <- read_excel("R_com_exemplos_de PI/Bases_de_Dados/tropas_ONU.xls")

attach(tropas_ONU)
plot(ano, tropas,type = "o", col = "cadetblue", xlab = "Anos", ylab = "Tropas",main = "Tropas na ONU por ano")
plot(ano, tropas,type = "o", col = "aquamarine4", xlab = "Anos", ylab = "Tropas",main = "Tropas na ONU por ano", lwd=2)
     