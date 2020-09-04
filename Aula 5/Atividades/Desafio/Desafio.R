######### pacotes
library(readxl)
setwd("C:\\Users\\Rodrigo\\OneDrive\\EGC - NEREUS\\Repositório GitHub\\Applied-General-Equilibrium-Models_USP\\Aula 5\\Atividades\\Desafio")

# Carregando os dados de 1985

FBCF_1985 <- as.matrix(read_excel("TABELA03_1985.xls", sheet =1, range = "AV6:AV85", col_names = FALSE))
FBCF_1985_TOTAL <- 261333 
PO_1985 <- as.matrix(read_excel("Tabela01_1985.xls", sheet =4, range = "B21:AQ21", col_names = FALSE))
VBP_1985 <- as.matrix(read_excel("TABELA02_1985.xls", sheet =1, range = "CE6:CE47", col_names = FALSE))
DE_1985 <- as.matrix(read_excel("TABELA19_1985.xls", sheet =1, range = "C6:CD47", col_names = FALSE)) 
B_1985 <- as.matrix(read_excel("TABELA21_1985.xls", sheet =1, range = "C6:AR47", col_names = FALSE))

# Carregando os dados de 1995

FBCF_1995 <- as.matrix(read_excel("Tabela03_1995.xls", sheet =1, range = "AV7:AV86", col_names = FALSE))
FBCF_1995_TOTAL <- 132753432 
PO_1995 <- as.matrix(read_excel("Tabela02_1995.xls", sheet =3, range = "B19:AQ19", col_names = FALSE))
VBP_1995 <- as.matrix(read_excel("Tabela01_1995.xls", sheet = 2, range = "c87:AR87", col_names = F))
DE_1995 <- as.matrix(read_excel("Tabela18_1995.xls", sheet =1, range = "C6:CD47", col_names = FALSE))
B_1995 <- as.matrix(read_excel("Tabela20_1995.xls", sheet =1, range = "C6:AR47", col_names = FALSE))

# Carregando os dados de 2005

FBCF_2005 <- as.matrix(read_excel("tab03_2005.xls", sheet =1, range = "U6:U17", col_names = FALSE))
FBCF_2005_TOTAL <-   342237
VBP_2005 <- as.matrix(read_excel("tab01_2005.xls", sheet = 1, range = "L20:W20", col_names = FALSE))
PO_2005 <- as.matrix(read_excel("tab02_2005.xls", sheet =1, range = "C38:N38", col_names = FALSE))
DE_2005 <- as.matrix(read_excel("tab07_2005.xls", sheet =1, range = "C6:N17", col_names = FALSE))
B_2005 <- as.matrix(read_excel("tab09_2005.xlsx", sheet =1, range = "C6:N17", col_names = FALSE))

# Carregando os dados de 2015

FBCF_2015 <- as.matrix(read_excel("MIP_2015.xls", sheet =3, range = "U6:U17", col_names = FALSE))
FBCF_2015_TOTAL <-  1069397
PO_2015 <- as.matrix(read_excel("tab2_2015.xls", sheet = 3, range = "B19:M19", col_names = FALSE))
VBP_2015 <- as.matrix(read_excel("MIP_2015.xls", sheet = 1, range = "H19:S19", col_names = FALSE))
DE_2015 <- as.matrix(read_excel("MIP_2015.xls", sheet =13, range = "C6:N17", col_names = FALSE))
B_2015 <- as.matrix(read_excel("MIP_2015.xls", sheet =15, range = "C6:N17", col_names = FALSE))

options(scipen=999)

#Encontrando investimento por setor
FBCF_1985_setor <- DE_1985 %*% FBCF_1985
FBCF_1995_setor <- DE_1995 %*% FBCF_1995
FBCF_2005_setor <- DE_2005 %*% FBCF_2005
FBCF_2015_setor <- DE_2015 %*% FBCF_2015

#unidade padrão de investimento
c_FBCF_1985 <- as.matrix(apply(FBCF_1985_setor, 1, "/", FBCF_1985_TOTAL))
c_FBCF_1995 <- as.matrix(apply(FBCF_1995_setor, 1, "/", FBCF_1995_TOTAL))
c_FBCF_2005 <- as.matrix(apply(FBCF_2005_setor, 1, "/", FBCF_2005_TOTAL))
c_FBCF_2015 <- as.matrix(apply(FBCF_2015_setor, 1, "/", FBCF_2015_TOTAL))
