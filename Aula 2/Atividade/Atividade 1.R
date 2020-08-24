########## Carregando pacotes ------------------------
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

######### Carregando dataset -------------------------

PNAD <- read_excel("C:\\Users\\Rodrigo\\OneDrive\\EGC - NEREUS\\Aula 2\\Atividade\\Emprego_UF_2015.xlsx", sheet = 1, range = "B5:AC23")

RAIS <- read_excel("C:\\Users\\Rodrigo\\OneDrive\\EGC - NEREUS\\Aula 2\\Atividade\\Emprego_UF_2015.xlsx", sheet = 2, range = "B5:AC72")

######### Calculando o Quociente Locacional (QL)  da PNAD ------------------------------

# Filtrando apenas os números da PNAD

PNAD <- PNAD[,2:28]

# calculando os totais por estado (soma das linhas) e dos setores (soma das colunas)

total_estado_PNAD <- apply(PNAD, 2, sum)

total_setor_PNAD <- data.frame(apply(PNAD, 1, sum))

# Calculando o QL - PNAD
Particip_setorial_estados <- sweep(PNAD, 2, total_estado_PNAD, "/")
Particip_setorial_total <- total_setor_PNAD/sum(total_setor_PNAD)

QL_PNAD <- data.frame(apply(Particip_setorial_estados, 2, "/", Particip_setorial_total))

names(QL_PNAD) <- names(PNAD)

########## Calculando o Quociente Locacional (QL) da RAIS -----------------------------

# filtrando apenas números da RAIS

RAIS <- RAIS[,2:28]

# calculando os totais por estado (soma das linhas) e dos setores (soma das colunas)

total_estado_RAIS <- apply(RAIS, 2, sum)

total_setor_RAIS <- apply(RAIS, 1, sum)

# calculando o QL - RAIS 

Particip_setorial_estados_RAIS <- sweep(RAIS, 2, total_estado_RAIS, "/")
Particip_setorial_total_RAIS <- total_setor_RAIS/sum(total_setor_RAIS)

QL_RAIS <- data.frame(apply(Particip_setorial_estados_RAIS, 2, "/", Particip_setorial_total_RAIS))

########### MULTIPLICADOR DE BASE ECONÔMICA

## calculando os multiplicadores para PNAD 

# função para filtro dos valores maiores que 1
maior_q_1 <- function(x) {
  if (x>1) {
    x*1
  } else {
    x*0
  }
}

diff_setorial_total_PNAD <- as.matrix(data.frame(apply(Particip_setorial_estados, 2, "-", Particip_setorial_total)))
diff_produto_PNAD <- t(apply(diff_setorial_total_PNAD, 1, "*", total_estado_PNAD))
matriz_maior_q_1 <- apply(diff_produto_PNAD, MARGIN =c(1,2), maior_q_1)

# Empregos da Base Econômica voltada para exportação
Eb_PNAD <- diff_produto_PNAD * matriz_maior_q_1

# Multiplicador PNAD

Multiplicador_PNAD <- data.frame(total_estado_PNAD / apply(Eb_PNAD, 2, sum))

## Calculando os multiplicadores para RAIS

diff_setorial_total_RAIS <- as.matrix(data.frame(apply(Particip_setorial_estados_RAIS, 2, "-", Particip_setorial_total_RAIS)))
diff_produto_RAIS <- t(apply(diff_setorial_total_RAIS, 1, "*", total_estado_RAIS))
matriz_maior_q_1_RAIS <- apply(diff_produto_RAIS, MARGIN = c(1,2), maior_q_1)

# Empregos da Base Econômica voltada para exportação

Eb_RAIS <- diff_produto_RAIS * matriz_maior_q_1_RAIS

# Calculo do Multiplicador para RAIS

Multiplicador_RAIS <- data.frame(total_estado_RAIS / apply(Eb_RAIS, 2, sum))

######### Gráfico ----------------------------------------------

dados_grafico <- data.frame(cbind(rownames(Multiplicador_PNAD), Multiplicador_PNAD, Multiplicador_RAIS), row.names = NULL)
names(dados_grafico) = c("UF","PNAD", "RAIS")

dados_grafico <- dados_grafico %>% gather("dados", "multiplicador", -UF)

grafico <- ggplot(dados_grafico, aes(x = UF, y = multiplicador, fill = dados)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grafico
