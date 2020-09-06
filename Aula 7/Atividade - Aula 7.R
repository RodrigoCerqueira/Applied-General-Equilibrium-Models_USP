####### Carregando pactoes necessários ---------------------------------------------------------

library(readxl)
library(dplyr)

####### Carregando datasets ----------------------------------------------------------------------

setwd("C:/Users/Rodrigo/OneDrive/EGC - NEREUS/Repositório GitHub/Applied-General-Equilibrium-Models_USP/Aula 7")

#matriz inversa de Leontief
B <- as.matrix(read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 6, range = "D4:CM91", col_names = FALSE, col_types = "numeric"))

#dados da demanda final por região

#investimento por região de origem
invest_R1 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CN4:CN91", col_names = FALSE, col_types = "numeric")
invest_R2 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CO4:CO91", col_names = FALSE, col_types = "numeric")
invest_R3 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CP4:CP91", col_names = FALSE, col_types = "numeric")
invest_R4 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CQ4:Cq91", col_names = FALSE, col_types = "numeric")

#consumo das famílias por região de origem
consumo_R1 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CR4:CR91", col_names = FALSE, col_types = "numeric")
consumo_R2 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CS4:CS91", col_names = FALSE, col_types = "numeric")
consumo_R3 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CT4:CT91", col_names = FALSE, col_types = "numeric")
consumo_R4 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CU4:CU91", col_names = FALSE, col_types = "numeric")

#gastos do governo por região de origem
G_R1 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CV4:CV91", col_names = FALSE, col_types = "numeric")
G_R2 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CW4:CW91", col_names = FALSE, col_types = "numeric")
G_R3 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CX4:CX91", col_names = FALSE, col_types = "numeric")
G_R4 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "CY4:CY91", col_names = FALSE, col_types = "numeric")

# consumo ISFLSF por região de origem
ISFLSF_R1 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "Cz4:Cz91", col_names = FALSE, col_types = "numeric")
ISFLSF_R2 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "DA4:DA91", col_names = FALSE, col_types = "numeric")
ISFLSF_R3 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "DB4:DB91", col_names = FALSE, col_types = "numeric")
ISFLSF_R4 <- read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "DC4:DC91", col_names = FALSE, col_types = "numeric")

#exportações para outros países
Ex <- as.matrix(read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "DD4:DD91", col_names = FALSE, col_types = "numeric"))

#discrepância estatística
rest <- as.matrix(read_excel("IIOAS_APSAL_22SETORES_2015.xlsx", sheet = 5, range = "DE4:DE91", col_names = FALSE, col_types = "numeric"))

######## Juntando as demandas finais por Região de origem
DF_R1 = as.matrix(invest_R1 + consumo_R1 + G_R1 + ISFLSF_R1)
DF_R2 = as.matrix(invest_R2 + consumo_R2 + G_R2 + ISFLSF_R2)
DF_R3 = as.matrix(invest_R3 + consumo_R3 + G_R3 + ISFLSF_R3)
DF_R4 = as.matrix(invest_R4 + consumo_R4 + G_R4 + ISFLSF_R4)

####### Calculando o VBP com base na origem da demanda

#vetor de VBP por origem de demanda
VBP_DR1 <- B %*% DF_R1
VBP_DR2 <- B %*% DF_R2
VBP_DR3 <- B %*% DF_R3
VBP_DR4 <- B %*% DF_R4
VBP_Exp <- B %*% Ex
VBP_rest <- B %*% rest

#Separando o VBP originado da Demanda final da Região 1, por origem do produto
VBP_R1_DR1 <- VBP_DR1[1:22,]
VBP_R2_DR1 <- VBP_DR1[23:44,]
VBP_R3_DR1 <- VBP_DR1[45:66,]
VBP_R4_DR1 <- VBP_DR1[67:88,]

#separando VBP originado da Demanda final da Região 2, por origem do produto
VBP_R1_DR2 <- VBP_DR2[1:22,]
VBP_R2_DR2 <- VBP_DR2[23:44,]
VBP_R3_DR2 <- VBP_DR2[45:66,]
VBP_R4_DR2 <- VBP_DR2[67:88,]

#separando VBP originado da Demanda final da Região 3, por origem do produto
VBP_R1_DR3 <- VBP_DR3[1:22,]
VBP_R2_DR3 <- VBP_DR3[23:44,]
VBP_R3_DR3 <- VBP_DR3[45:66,]
VBP_R4_DR3 <- VBP_DR3[67:88,]

#separando VBP originado da Demanda final da Região 4, por origem do produto
VBP_R1_DR4 <- VBP_DR4[1:22,]
VBP_R2_DR4 <- VBP_DR4[23:44,]
VBP_R3_DR4 <- VBP_DR4[45:66,]
VBP_R4_DR4 <- VBP_DR4[67:88,]

#Separando VBP originado das Exportações, por origem do produto
VBP_R1_Exp <- VBP_Exp[1:22,]
VBP_R2_Exp <- VBP_Exp[23:44,]
VBP_R3_Exp <- VBP_Exp[45:66,]
VBP_R4_Exp <- VBP_Exp[67:88,]

#criando matriz de resultados - colunas
R1 <- c(sum(VBP_R1_DR1), sum(VBP_R2_DR1), sum(VBP_R3_DR1), sum(VBP_R4_DR1))
R2 <- c(sum(VBP_R1_DR2), sum(VBP_R2_DR2), sum(VBP_R3_DR2), sum(VBP_R4_DR2))
R3 <- c(sum(VBP_R1_DR3), sum(VBP_R2_DR3), sum(VBP_R3_DR3), sum(VBP_R4_DR3))
R4 <- c(sum(VBP_R1_DR4), sum(VBP_R2_DR4), sum(VBP_R3_DR4), sum(VBP_R4_DR4))
Exp <- c(sum(VBP_R1_Exp), sum(VBP_R2_Exp), sum(VBP_R3_Exp), sum(VBP_R4_Exp))
Total <- as.matrix(c((R1+R2+R3+R4+Exp)))

decomposicao <- data.frame(R1, R2, R3, R4, Exp, Total)

decomposicao <- rbind(decomposicao, apply(decomposicao, 2, sum))

row.names(decomposicao) <-  c("R1", "R2", "R3", "R4", "Total")

decomposicao_linha <- round(as.matrix(apply(decomposicao, 2, "/", decomposicao$Total/100)),2)

decomposicao_coluna <- round(t(as.matrix(apply(decomposicao, 1, "/", t(decomposicao[5,]/100)))),2)

colnames(decomposicao_coluna) <- c("R1", "R2", "R3", "R4", "Exp", "Total")
                                 