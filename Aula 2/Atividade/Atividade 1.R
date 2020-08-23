########## Carregando pacotes ------------------------
library(readxl)

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

## PNAD 

maior_q_1 <- function(x) {
  if (x>1) {
    x*1
  } else {
    x*0
  }
}

diff_setorial_total_PNAD <- as.matrix(data.frame(apply(Particip_setorial_estados, 2, "-", Particip_setorial_total)))
diff_produto_PNAD <- t(apply(setorial_menos_total_PNAD, 1, "*", total_estado_PNAD))
matriz_maior_q_1 <- apply(teste2, MARGIN =c(1,2), maior_q_1)

Multiplicador_PNAD <- diff_produto_PNAD * matriz_maior_q_1

