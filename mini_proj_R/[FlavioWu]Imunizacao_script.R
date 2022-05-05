library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)

# importando os dados do excel. Fonte: Banco Mundial
# Indicador: Immunization, measles (% of children ages 12-23 months) 
# trad: Imunização, sarampo (% de crianças de 12-23 meses de idade)
data <- read_excel("./API_SH.IMM.MEAS_DS2_en_excel_v2_3935140.xls", skip=3)

# selecionando os dados:
data<- select(data, -c(seq(2, 33)))   # excluindo as colunas "Country code", "Indicator Name", "Indicator Code" e os anos 1960 a 1988
                                      # 1989 foi escolhido para ser o primeiro ano de comparação por ser o primeiro ano da CF/1988 em vigor

data <- data[1:length(data)-1]  # a coluna de 2020 está vazia, portanto, vamos joogar fora também

# guardando os anos em um data frame separado
anos <- tibble(colnames(data))
anos <- anos[-c(1),]
names(anos)[1] <- "Anos"

# selecionando os países Brasil, china e Estados Unidos para comparação
sarampo <- data[data$"Country Name" %in% c("Brazil", 'China', "United States"), ]
sarampo <- data.frame(t(sarampo))

# renomeando as colunas
names(sarampo)[1] <- "Brasil"
names(sarampo)[2] <- "China"
names(sarampo)[3] <- "Estados_Unidos"

sarampo <- sarampo[-c(1), ]   # removendo a primeira linha que contém o nome dos países

rownames(sarampo) <- 1:nrow(sarampo)    # resetando os índices

sarampo <- bind_cols(anos, sarampo)   #  juntando os dados dos anos

# montando um dataframe para o ggplot:
data2plot <- data.frame(year = sarampo$Anos,
                        percentage = c(sarampo$Brasil, sarampo$China, sarampo$Estados_Unidos),
                        country = c(rep("Brasil", nrow(sarampo)),
                                    rep("China", nrow(sarampo)),
                                    rep("Estados_Unidos", nrow(sarampo))))

# graficos:
data2plot$year <- as.numeric(data2plot$year)    #  mudando o tipo das variaveis ano de char para numeric
data2plot$percentage <- as.numeric(data2plot$percentage)    #  mudando o tipo das variaveis percentage de char para numeric

ggp <- ggplot(data2plot, aes(x=year, y=percentage, col=country)) + 
        geom_line(linetype="dashed") + 
        geom_point() + 
        labs(title="Imunização, sarampo (% crianças, 12-23 meses de idade)", subtitle = "Fonte: Banco Mundial") +
        xlab("Ano") + 
        ylab("Porcentagem %")
ggp 
ggp + facet_grid(country ~ .) + labs(title="Imunização, sarampo (% crianças, 12-23 meses de idade)", subtitle = "Fonte: Banco Mundial") + xlab("Ano") + ylab("Porcentagem %")
