######### pacotes
library(readxl)
library(dplyr)
library(ggplot2)

######### leitura dos arquivos
MIP_2017 <- read_excel("MIP_2017.xlsx", sheet =2, range = "A3:CB97")

#selecão da matriz e vetores iniciais apra os cálculos
Z <- MIP_2017[2:69,4:71] 

VBP <- MIP_2017[93,4:71]

salarios <- MIP_2017[81,4:71]

c_familias <- MIP_2017[2:69,76]

c_total <- MIP_2017[79,76]

VA <- MIP_2017[89,4:71]

imp_ICMS <- MIP_2017[73,4:71]
  
imp_IPI <- MIP_2017[75,4:71]
  
imp_outros <- MIP_2017[77,4:71]

imp <- imp_ICMS + imp_IPI + imp_outros
  
emprego <- MIP_2017[94,4:71]

######## calculo das MRI

#matriz A (modelo aberto)

A <- sweep(Z, 2, t(VBP), "/") %>% unname() %>% as.matrix()

# matriz A_barra (modelo fechado)              

hc <- t(data.frame(apply(c_familias, 1, "/", c_total))) %>% unname() %>% as.matrix()

hr <- sweep(salarios, 2, t(VBP), "/") %>% cbind(0) %>% unname() %>% as.matrix()         

A_barra <- cbind(A, hc) %>% rbind(hr)         

#inversa de leontief - modelo aberto

I <- diag(nrow(A))

B <- solve(I - A)

#inversa de leontief - modelo fechado

I_ <- diag(nrow(A_barra))

B_barra <- solve(I_ - A_barra)

###### multiplicadores de produção

#modelo aberto

MP <- data.frame(apply(B, 2, sum))

#modelo fechado

MP_barra <- data.frame(apply(B_barra[-nrow(B_barra),-ncol(B_barra)], 2, sum))

####### decomposição dos efeitos --------------- 

efeito_renda <- MP_barra - MP

efeito_direto <- data.frame(apply(A, 2, sum))

efeito_total <- MP_barra

efeito_inicial <- data.frame(c(rep(1,nrow(A))))

efeito_indireto <- MP - efeito_inicial - efeito_direto

nomes <- data.frame(colnames(Z))

efeitos <- data.frame(nomes, as.vector(c(1:68)), efeito_total, efeito_renda, efeito_indireto, efeito_direto, efeito_inicial)
colnames(efeitos) <- c("setor","cod", "efeito total", "efeito renda", "efeito indireto", "efeito direto", "efeito inicial")

######### gráficos ------------

graf_renda <- efeitos %>% 
  arrange(`efeito renda`) %>% 
  mutate(setor = factor(setor, levels = setor)) %>% 
  ggplot() +
  geom_bar(aes(x = setor, y = `efeito renda`), stat = "identity") +
  coord_flip()

graf_renda


graf_indireto <- efeitos %>% 
  arrange(`efeito indireto`) %>% 
  mutate(setor = factor(setor, levels = setor)) %>% 
  ggplot()+
  geom_bar(aes(x = setor, y = `efeito indireto`), stat = "identity")+
  coord_flip()

graf_indireto

######## multiplicadores de VA, impostos e empregos Tipo I (modelo aberto)

#coeficientes

coef_VA <- VA/VBP
coef_imp <- imp/VBP
coef_emprego <- emprego/VBP

#geradores

g_VA <- data.frame(apply(sweep(B, 1, t(coef_VA), "*"),2,sum))

g_imp <- data.frame(apply(sweep(B, 1, t(coef_imp), "*"),2,sum))

g_emprego <- data.frame(apply(sweep(B, 1, t(coef_emprego), "*"),2,sum))

#multiplicadores

m_VA_I <- g_VA/t(coef_VA)

m_imp_I <- g_imp/t(coef_imp)

m_emprego_I <- g_emprego/t(coef_emprego)


######## multiplicadores de VA, impostos e empregos Tipo II (modelo fechado)

#geradores

g_VA_ <- data.frame(apply(sweep(B_barra[-nrow(B_barra),-ncol(B_barra)], 1, t(coef_VA), "*"),2,sum))

g_imp_ <- data.frame(apply(sweep(B_barra[-nrow(B_barra),-ncol(B_barra)], 1, t(coef_imp), "*"),2,sum))

g_emprego_ <- data.frame(apply(sweep(B_barra[-nrow(B_barra),-ncol(B_barra)], 1, t(coef_emprego), "*"),2,sum))

#multiplicadores

m_VA_II <- g_VA_/t(coef_VA)

m_imp_II <- g_imp_/t(coef_imp)

m_emprego_II <- g_emprego_/t(coef_emprego)

#unificando

multiplicadores <- data.frame(nomes, m_VA_I, m_imp_I, m_emprego_I, m_VA_II, m_imp_II, m_emprego_II)
names(multiplicadores) <- c("setor", "VA_I", "impostos_I", "empregos_I", "VA_II", "impostos_II", "empregos_II")

