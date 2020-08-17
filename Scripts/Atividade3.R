# Bibliotecas
library(betareg)
library(ggplot2)
library(ggrepel)

# Ler csv
df <- read.csv("../Dados/Atividade3.csv", sep = ";")

# Resolver compatibilidade trocando "," com ;
df$Domicílios <-
    as.numeric(sub(",", ".", df$Domicílios, fixed = TRUE))
df$GINI <- as.numeric(sub(",", ".", df$GINI, fixed = TRUE))

# Beta Regression
reg <- betareg(Domicílios ~ GINI, data = df)

# Reg linear
reglm <- lm(Domicílios ~  GINI, data = df)

# Comparar resultados
sum(residuals(reg)^ 2)  < sum(residuals(reglm)^ 2) 

# Sumário
summary(reglm)

# Gráfico
ggplot(df, aes(GINI, Domicílios, label = Estado, color = Região)) +
    geom_point(size = 2.5) +
    # geom_text(data=df[19,], color = "black",
    #           nudge_y = -0.02) +
    geom_text_repel() +
    geom_line(aes(GINI, predict(reglm)), color = "black") +
    labs(
        title = "Desigualdade nos estados e porcentagem de
domicílios com auxílio emergencial",
        x = "Coeficiente de GINI",
        y = "Domicílios com auxílio emergencial (%)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))

# Salvar gráfico
ggsave("../Graficos/Atividade3.png")
