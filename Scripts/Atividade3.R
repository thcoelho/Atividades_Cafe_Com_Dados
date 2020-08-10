# Bibliotecas
library(betareg)
library(ggplot2)
library(ggrepel)

# Ler csv
df <- read.csv("../Dados/GINI_Por_Estado.csv", sep = ";")

# Resolver compatibilidade trocando "," com ;
df$Domicilios <-
    as.numeric(sub(",", ".", df$Domicilios, fixed = TRUE))
df$GINI <- as.numeric(sub(",", ".", df$GINI, fixed = TRUE))


# Beta Regression
reg <- betareg(Domicilios ~ GINI, data = df)

# Sumário

summary(reg)

# Gráfico
ggplot(df, aes(GINI, Domicilios, label = Estado, color = Regiao)) +
    geom_point(size = 2.5) +
        geom_text_repel(color = "black") +
        geom_line(aes(GINI, predict(reg)), color = "black") +
    labs(title = "Desigualdade nos estados e porcentagem de 
domicílios com auxílio emergencial",
     x = "Coeficiente de GINI",
     y = "Domicílios com auxílio emergencial (%)")
        