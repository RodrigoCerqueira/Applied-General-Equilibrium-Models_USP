######### pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)

######### leitura dos arquivos
MIP_2017 <- read_excel("MIP_2017.xlsx", sheet =2, range = "A3:CB97")

#selecão da matrizes e vetores iniciais para os cálculos
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

efeitos <- data.frame(nomes, as.vector(c(1:nrow(B))), efeito_total, efeito_renda, efeito_indireto, efeito_direto, efeito_inicial)
colnames(efeitos) <- c("setor","cod", "efeito total", "efeito renda", "efeito indireto", "efeito direto", "efeito inicial")

######### gráficos ------------


graf_renda <- efeitos %>% 
  arrange(-`efeito renda`) %>% 
  mutate(setor = factor(setor, levels = setor)) 
 
graf_renda$pos <- NA
 
graf_renda[1:5,]$pos <- "top"

graf_renda <- graf_renda %>% 
  arrange(-cod) %>% 
  mutate(setor = factor(setor, levels = setor)) %>% 
  ggplot() +
  geom_bar(aes(x = setor, y = `efeito renda`, fill = pos), stat = "identity") +
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")

graf_renda


graf_indireto <- efeitos %>% 
  arrange(-`efeito indireto`) %>% 
  mutate(setor = factor(setor, levels = setor))

graf_indireto$pos <- NA

graf_indireto[1:5,]$pos <- "top"

graf_indireto <- graf_indireto %>%
  arrange(-cod) %>% 
  mutate(setor = factor(setor, levels = setor)) %>% 
  ggplot() +
  geom_bar(aes(x = setor, y = `efeito indireto`, fill = pos), stat = "identity") +
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")

graf_indireto

library(ggpubr)
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

#diferenças

dif_VA <- m_VA_II - m_VA_I

razao_VA <- m_VA_II/m_VA_I


#unificando

multiplicadores <- data.frame(nomes, m_VA_I, m_imp_I, m_emprego_I, m_VA_II, m_imp_II, m_emprego_II)
names(multiplicadores) <- c("setor", "VA_I", "impostos_I", "empregos_I", "VA_II", "impostos_II", "empregos_II")

########## Coeficiente de Rasmussen-Hirschman de Ligação para frente Uio e para Trás Uoj  - modelo aberto -----

Uio = as.matrix(apply(B, 1, mean))/mean(B)

Uoj = as.matrix(apply(B, 2, mean))/mean(B)

indices_ligacao  <- data.frame(nomes, Uio, Uoj)
names(indices_ligacao) <- c("setor", "Uio", "Uoj")

indices_ligacao$chave <- ifelse(indices_ligacao$Uio > 1 & indices_ligacao$Uoj > 1, "Setor chave", "-")

# gráfico


dispersão_indices <- ggplot(indices_ligacao, aes(label =setor))+
  geom_point(aes(x = Uio, y = Uoj, colour = chave), size = 2) +
  theme_bw() +
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 1)+
  scale_color_manual(values = c("#393b44", "#318fb5")) +
  theme(legend.position = "none") +
  ylab("Encadeamento para trás (Uj)") +
  xlab("Encadeamento para frente (Ui)")+
  ggtitle("Índices de ligação de Rasmussen-Hirschman, Brasil - 2017")+
  geom_text_repel(data = indices_ligacao[indices_ligacao$chave == "Setor chave",], 
                  aes(x = Uio, y = Uoj, label = setor),
                  size = 2.5)+
  annotate(geom = "text", x = max(Uio)+0.1, y = max(Uoj)+0.1, label = "Setores-chave\n(Ui > 1 e Uj > 1)")+
  annotate(geom = "text", x = 0.15, y = min(Uoj)+0.02, label = "não fortemente \nconectado com \noutros setores \n(Uj < 1 e Ui < 1)")+
  annotate(geom = "text", x = 0.15, y = max(Uoj)+0.08, label = "Dependente da\noferta intersetorial\nUj > 1")+
  annotate(geom = "text", x = max(Uio)+0.05, y = min(Uoj)+0.01, label = "Dependente da\nodemanda intersetorial\nUi > 1")

dispersão_indices

dispersão_indices_html <- ggplotly(dispersão_indices, tooltip = c("x", "y", "label"))

dispersão_indices_html

htmlwidgets::saveWidget(dispersão_indices_html,"dispersão_indices.html")
