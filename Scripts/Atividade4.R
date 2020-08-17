# Bibliotecas
library(rio)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Importar os dados e salvar numa variável
df <- import("../Dados/Atividade4.1.xls", sheet = "Junho")

# Sumário das colunas
summary(df)

# Colocar coluna em porcentagem
df <- df %>% mutate(`Últimos 12 meses` = `Últimos 12 meses` / 100)


# Modelo
modelo_linear <-  lm(df$`Últimos 12 meses` ~ Isolamento, data=df)

# Sumário do modelo
summary(modelo_linear)

# Gráfico
ggplot(df, aes(Isolamento, `Últimos 12 meses`, label=UF, color=Regiao)) +
    geom_point() +
    geom_text_repel()

ggsave("../Graficos/Atividade4.1.png")
