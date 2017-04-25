library(data.table)
library(gridExtra)
library(moments)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Rmisc)
library(devtools)

# separando os dados da tabela que me interessam 
casa = dados_domiciliosCEA15P02['consumo_per_capita'][dados_domiciliosCEA15P02['TIPODEDOMICILIO']==1]
ap = dados_domiciliosCEA15P02['consumo_per_capita'][dados_domiciliosCEA15P02['TIPODEDOMICILIO']==2]
comodo = dados_domiciliosCEA15P02['consumo_per_capita'][dados_domiciliosCEA15P02['TIPODEDOMICILIO']==3]

# como cada uma dessas variáveis tem tamanhos diferentes
# vamos encontrar a que tem maior comprimento 
max_len = max(length(casa), length(ap), length(comodo))

# preencher os valores faltantes nas outras colunas
casa = c(casa, rep(NA, max_len - length(casa)))
ap = c(ap, rep(NA, max_len - length(ap)))
comodo = c(comodo, rep(NA, max_len - length(comodo)))

# criar o dataframe com essas três listas
df = data.frame(casa=casa, ap=ap, comodo=comodo)

# a)
boxplot(df, main='Consumo de Gás Per Capita por tipo Domicílio',
        col=c('red', 'blue', 'green'), ylab='kg')

n_casa = length(casa)
n_ap = length(ap)
n_com = length(comodo)
n_total = length(casa)+length(ap)+length(comodo)

soma_var = n_casa*var(casa) + n_ap*var(ap) + n_com*var(comodo)

var_barra = soma_var/n_total

var_total = var(dados_domiciliosCEA15P02$consumo_per_capita)
R_quadrado = (var_total - var_barra)/var_total

# b)

# dividindo as rendas em salários mínimos conforme pede exercício
casa_ate_um= dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 937
                                                                 & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 1]

casa_um_dois = dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 937 
                                                              & dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 937*2
                                                              & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 1]

casa_dois_quatro = dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 2*937 
                                                                  & dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 4*937
                                                                  & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 1]

casa_mais_que_quatro =  dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 4*937
                                                                       & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 2] 


ap_ate_um= dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 937
                                                                 & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 2]

ap_um_dois = dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 937 
                                                                   & dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 937*2
                                                                   & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 2]

ap_dois_quatro = dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 2*937 
                                                                       & dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 4*937
                                                                       & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 2]

ap_mais_que_quatro =  dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 4*937
                                                                            & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 2] 


comodo_ate_um= dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 937
                                                                 & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 3]

comodo_um_dois = dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 937 
                                                                   & dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 937*2
                                                                   & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 3]

comodo_casa_dois_quatro = dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 2*937 
                                                                       & dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] < 4*937
                                                                       & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 3]

comodo_casa_mais_que_quatro =  dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'][dados_domiciliosCEA15P02['RENDA_PER_CAPITA_MENSAL'] >= 4*937
                                                                            & dados_domiciliosCEA15P02['TIPODEDOMICILIO'] == 3] 


ate_um = c(length(casa_ate_um), length(ap_ate_um), length(comodo_ate_um))
um_dois = c(length(casa_um_dois), length(ap_um_dois), length(comodo_um_dois))
dois_quatro = c(length(casa_dois_quatro), length(ap_dois_quatro), 
                length(comodo_casa_dois_quatro))
mais_que_quatro = c(length(casa_mais_que_quatro), length(ap_mais_que_quatro),
                    length(comodo_casa_mais_que_quatro))

df2 = data.frame(ate_um, um_dois, dois_quatro, mais_que_quatro)
colnames(df2) = c("< 1s.m", "1-2s.m", "2-4s.m", ">= 4 s.m")

(setattr(df2, "row.names", c("casa", "apartamento", "cômodo")))
# imprimir imagem de tabela com grades
gridExtra::grid.table(df2)

# chi-quadrado
chisq.test(df2)
chi = 1923.1
# Coeficiente de Contigência
C = sqrt(chi/(chi + 12))
# Coeficiente T
t = sqrt((chi)/(chi + 12*2*3))


# 5)
# a)


media_rc = c(round(mean(malaria$peso, na.rm = TRUE), 1))
mediana_rc = c(round(median(malaria$peso, na.rm = TRUE), 1))
minimo_rc = c(min(malaria$peso, na.rm = TRUE))
maximo_rc = c(max(malaria$peso, na.rm = TRUE))
q1_rc = c(quantile(malaria$peso, .25, na.rm = TRUE))
q3_rc = c(quantile(malaria$peso, .75, na.rm = TRUE))
desvio_padrao_rc = c(round(sd(malaria$peso, na.rm = TRUE), 1))

# grupo  Grupo #	(0-controle; 1-vivax; 2-falciparum; 3-mista)

# Controle
media_controle = c(round(mean(malaria$peso[malaria['grupo']==0], na.rm = TRUE), 1))
mediana_controle = c(round(median(malaria$peso[malaria['grupo']==0], na.rm = TRUE), 1))
minimo_controle = c(min(malaria$peso[malaria['grupo']==0], na.rm = TRUE))
maximo_controle = c(max(malaria$peso[malaria['grupo']==0], na.rm = TRUE))
q1_controle = c(quantile(malaria$peso[malaria['grupo']==0], .25, na.rm = TRUE))
q3_controle = c(quantile(malaria$peso[malaria['grupo']==0], .75, na.rm = TRUE))
desvio_padrao_controle = c(round(sd(malaria$peso[malaria['grupo']==0], na.rm = TRUE), 1))

length(malaria$peso[malaria['grupo']==0], na.rm = TRUE)

# vivax

media_vivax = c(round(mean(malaria$peso[malaria['grupo']==1], na.rm = TRUE), 1))
mediana_vivax = c(round(median(malaria$peso[malaria['grupo']==1], na.rm = TRUE), 1))
minimo_vivax = c(min(malaria$peso[malaria['grupo']==1], na.rm = TRUE))
maximo_vivax = c(max(malaria$peso[malaria['grupo']==1], na.rm = TRUE))
q1_vivax = c(quantile(malaria$peso[malaria['grupo']==1], .25, na.rm = TRUE))
q3_vivax = c(quantile(malaria$peso[malaria['grupo']==1], .75, na.rm = TRUE))
desvio_padrao_vivax = c(round(sd(malaria$peso[malaria['grupo']==1], na.rm = TRUE), 1))

# 2-falciparum

media_fal = c(round(mean(malaria$peso[malaria['grupo']==2], na.rm = TRUE), 1))
mediana_fal = c(round(median(malaria$peso[malaria['grupo']==2], na.rm = TRUE), 1))
minimo_fal = c(min(malaria$peso[malaria['grupo']==2], na.rm = TRUE))
maximo_fal = c(max(malaria$peso[malaria['grupo']==2], na.rm = TRUE))
q1_fal = c(quantile(malaria$peso[malaria['grupo']==2], .25, na.rm = TRUE))
q3_fal = c(quantile(malaria$peso[malaria['grupo']==2], .75, na.rm = TRUE))
desvio_padrao_fal = c(round(sd(malaria$peso[malaria['grupo']==2], na.rm = TRUE), 1))



# 3-mista

media_mista = c(round(mean(malaria$peso[malaria['grupo']==3], na.rm = TRUE), 1))
mediana_mista = c(round(median(malaria$peso[malaria['grupo']==3], na.rm = TRUE), 1))
minimo_mista = c(min(malaria$peso[malaria['grupo']==3], na.rm = TRUE))
maximo_mista = c(max(malaria$peso[malaria['grupo']==3], na.rm = TRUE))
q1_mista = c(quantile(malaria$peso[malaria['grupo']==3], .25, na.rm = TRUE))
q3_mista = c(quantile(malaria$peso[malaria['grupo']==3], .75, na.rm = TRUE))
desvio_padrao_mista = c(round(sd(malaria$peso[malaria['grupo']==3], na.rm = TRUE), 1))

media = c(media_rc, media_controle, media_vivax, media_fal, media_mista)
mediana = c(mediana_rc, mediana_controle, mediana_vivax, mediana_fal, mediana_mista)
minimo = c(minimo_rc, minimo_controle, minimo_vivax, minimo_fal, minimo_mista)
maximo = c(maximo_rc, maximo_controle, maximo_vivax, maximo_fal, maximo_mista)
q1 = c(q1_rc, q1_controle, q1_vivax, q1_fal, q1_mista)
q3 = c(q3_rc, q3_controle, q3_vivax, q3_fal, q3_mista)
desvio_padrao = c(desvio_padrao_rc, desvio_padrao_controle,
                  desvio_padrao_vivax, desvio_padrao_fal,
                  desvio_padrao_mista)


# (0-controle; 1-vivax; 2-falciparum; 3-mista)
df = data.frame(media, mediana, minimo, maximo, q1, q3, desvio_padrao)
names = c("global", 'controle', 'vivax', 'falciparum', 'mista')
df = data.frame(grupo =names, df)
#(setattr(df, "row.names", c("global", 'controle', 'vivax', 'falciparum', 'mista')))
# imprimir imagem de tabela com grades
gridExtra::grid.table(df)

# peso por sexo

# Feminino
media_femi = c(round(mean(malaria$peso[malaria['sexorn']==2], na.rm = TRUE), 1))
mediana_femi = c(round(median(malaria$peso[malaria['sexorn']==2], na.rm = TRUE), 1))
minimo_femi = c(min(malaria$peso[malaria['sexorn']==2], na.rm = TRUE))
maximo_femi = c(max(malaria$peso[malaria['sexorn']==2], na.rm = TRUE))
q1_femi = c(quantile(malaria$peso[malaria['sexorn']==2], .25, na.rm = TRUE))
q3_femi = c(quantile(malaria$peso[malaria['sexorn']==2], .75, na.rm = TRUE))
desvio_padrao_femi = c(round(sd(malaria$peso[malaria['sexorn']==2], na.rm = TRUE), 1))

# Masculino

media_masc = c(round(mean(malaria$peso[malaria['sexorn']==1], na.rm = TRUE), 1))
mediana_masc = c(round(median(malaria$peso[malaria['sexorn']==1], na.rm = TRUE), 1))
minimo_masc = c(min(malaria$peso[malaria['sexorn']==1], na.rm = TRUE))
maximo_masc = c(max(malaria$peso[malaria['sexorn']==1], na.rm = TRUE))
q1_masc = c(quantile(malaria$peso[malaria['sexorn']==1], .25, na.rm = TRUE))
q3_masc = c(quantile(malaria$peso[malaria['sexorn']==1], .75, na.rm = TRUE))
desvio_padrao_masc = c(round(sd(malaria$peso[malaria['sexorn']==1], na.rm = TRUE), 1))


media = c(media_masc, media_femi)
mediana = c(mediana_masc, mediana_femi)
minimo = c(minimo_masc, minimo_femi)
maximo = c(maximo_masc, maximo_femi)
q1 = c(q1_masc, q1_femi)
q3 = c(q3_masc, q3_femi)
desvio_padrao = c(desvio_padrao_masc, desvio_padrao_femi)

df3 = data.frame(media, mediana, minimo, maximo, q1, q3, desvio_padrao)
names = c("masculino", 'feminino')
df3 = data.frame(sexo =names, df3)
#(setattr(df, "row.names", c("global", 'controle', 'vivax', 'falciparum', 'mista')))
# imprimir imagem de tabela com grades
gridExtra::grid.table(df3)



# prematrn    Prematuro   #	(0-não; 1-sim)
# Peso por indicador de prematuridade

# Não prematuro
media_nao = c(round(mean(malaria$peso[malaria['prematrn']==0], na.rm = TRUE), 1))
mediana_nao = c(round(median(malaria$peso[malaria['prematrn']==0], na.rm = TRUE), 1))
minimo_nao = c(min(malaria$peso[malaria['prematrn']==0], na.rm = TRUE))
maximo_nao = c(max(malaria$peso[malaria['prematrn']==0], na.rm = TRUE))
q1_nao = c(quantile(malaria$peso[malaria['prematrn']==0], .25, na.rm = TRUE))
q3_nao = c(quantile(malaria$peso[malaria['prematrn']==0], .75, na.rm = TRUE))
desvio_padrao_nao = c(round(sd(malaria$peso[malaria['prematrn']==0], na.rm = TRUE), 1))

# Prematuro
media_sim = c(round(mean(malaria$peso[malaria['prematrn']==1], na.rm = TRUE), 1))
mediana_sim = c(round(median(malaria$peso[malaria['prematrn']==1], na.rm = TRUE), 1))
minimo_sim = c(min(malaria$peso[malaria['prematrn']==1], na.rm = TRUE))
maximo_sim = c(max(malaria$peso[malaria['prematrn']==1], na.rm = TRUE))
q1_sim = c(quantile(malaria$peso[malaria['prematrn']==1], .25, na.rm = TRUE))
q3_sim = c(quantile(malaria$peso[malaria['prematrn']==1], .75, na.rm = TRUE))
desvio_padrao_sim = c(round(sd(malaria$peso[malaria['prematrn']==1], na.rm = TRUE), 1))

media = c(media_nao, media_sim)
mediana = c(mediana_nao, mediana_sim)
minimo = c(minimo_nao, minimo_sim)
maximo = c(maximo_nao, maximo_sim)
q1 = c(q1_nao, q1_sim)
q3 = c(q3_nao, q3_sim)
desvio_padrao = c(desvio_padrao_nao, desvio_padrao_sim)

df4 = data.frame(media, mediana, minimo, maximo, q1, q3, desvio_padrao)
names = c("Não", 'Sim')
df4 = data.frame(Prematuro=names, df4)
#(setattr(df, "row.names", c("global", 'controle', 'vivax', 'falciparum', 'mista')))
# imprimir imagem de tabela com grades
gridExtra::grid.table(df4)

# b)
# (0-controle; 1-vivax; 2-falciparum; 3-mista)
controle = malaria$peso[malaria['grupo']==0]
vivax = malaria$peso[malaria['grupo']==1]
falciparum = malaria$peso[malaria['grupo']==2]
mista = malaria$peso[malaria['grupo']==3]

length( malaria$peso[malaria['grupo']==2])

boxplot(controle, vivax, falciparum, mista, las = 1 , 
        names = c('controle', 'vivax', 'falciparum', 'mista'), 
        main='Peso(g) por grupo de infecção', xlab='Grupos', ylab='Peso(g)',
        col=c('blue', 'red', 'green', 'yellow'))

# C 

n_controle = length(controle)
n_vivax = length(vivax)
n_fal = length(falciparum)
n_mista = length(mista)
n_total = n_controle + n_vivax + n_fal + n_mista

soma_var = (n_controle*var(controle, na.rm = TRUE) + n_vivax*var(vivax, na.rm = TRUE)
            + n_fal*var(falciparum, na.rm = TRUE) + n_mista*var(mista, na.rm = TRUE))

var_barra = soma_var/n_total
var_total = var(malaria$peso, na.rm = TRUE)
R_quadrado = (var_total - var_barra)/var_total
R_quadrado*100


# peso e sexo
# (1-masculino; 2-feminino)
mas = malaria$peso[malaria['sexorn']==1]
femi = malaria$peso[malaria['sexorn']==2]


boxplot(mas, femi ,las = 1 , 
        names = c('masculino', 'feminino'), 
        main='Peso(g) por sexo do recém nascido', xlab='Sexo', ylab='Peso(g)',
        col=c('blue', 'red'))


# c) gráfico perfis

sexo.plot <- summarySE(malaria, measurevar="peso", groupvars="sexorn", na.rm=TRUE)
sexo.plot <- sexo.plot[-c(3), ]
sexo.plot$sexorn <- factor(c("Masculino", "Feminino"), 
                             levels=c("Masculino", "Feminino"))
sexo.plot

ggplot(sexo.plot, aes(x=sexorn, y=peso, group=1)) + 
  geom_errorbar(aes(ymin=peso-se, ymax=peso+se), width=.1) +
  geom_line() +
  geom_point() +
  labs(x="Sexo", y="Peso (g)") +
  ggtitle("Gráfico de Perfis de Média") +
  theme(plot.title = element_text(hjust = 0.5))

# perfis segundo parazita
prematuro.plot <- summarySE(malaria, measurevar="peso", groupvars="prematrn", na.rm=TRUE)
prematuro.plot <- parasita.plot[-c(3), ]
prematuro.plot$prematrn <- factor(c("Não", "Sim"), 
                           levels=c("Não", "Sim"))
prematuro.plot

ggplot(prematuro.plot, aes(x=prematrn, y=peso, group=1)) + 
  geom_errorbar(aes(ymin=peso-sd, ymax=peso+sd), width=.1) +
  geom_line() +
  geom_point() +
  labs(x="Prematuro?", y="Peso (g)") +
  ggtitle("Gráfico de Perfis de Média") +
  theme(plot.title = element_text(hjust = 0.5))

                                                                                                              
# c

soma_var = (length(mas)*var(mas, na.rm = TRUE) + length(femi)*var(femi, na.rm = TRUE))
var_barra = soma_var/(length(mas) + length(femi))
R_quadrado = (var_total - var_barra)/var_total
R_quadrado

# peso e prematuridade
nao = malaria$peso[malaria['prematrn']==0]
sim = malaria$peso[malaria['prematrn']==1]

soma_var = length(nao)*var(nao, na.rm = TRUE) + length(sim)*var(sim, na.rm = TRUE)
var_barra = soma_var/(length(nao)+length(sim))
R_quadrado = (var_total - var_barra)/var_total
R_quadrado
R_quadrado*100

boxplot(sim, nao ,las = 1 , 
        names = c('Sim', 'Não'), 
        main='Peso(g) por indicador de prematuridade', xlab='Prematuro?', ylab='Peso(g)',
        col=c('red', 'green'))

# d)

malaria.plot <- summarySE(malaria, measurevar="peso", groupvars="grupo", na.rm=TRUE)
malaria.plot$grupo <- factor(c("Controle", "Vivax", "Falciparum", "Mista"), 
                             levels=c("Controle", "Vivax", "Falciparum", "Mista"))
malaria.plot
ggplot(malaria.plot, aes(x=grupo, y=peso, group=1)) + 
  geom_errorbar(aes(ymin=peso-se, ymax=peso+se), width=.1) +
  geom_line() +
  geom_point() +
  labs(x="Grupo", y="Peso (g)") +
  ggtitle("Gráfico de Perfis de Média") +
  theme(plot.title = element_text(hjust = 0.5))






# 6) 
# a)

normal = rnorm(200, mean = 0, sd = 1)
plot(density(normal), main = 'Distribuição amostral Normal (n=200)',
     xlab = 'valores amostrais', ylab = 'Densidade')


qui = rchisq(200, df = 3)
plot(density(qui), main = 'Distribuição amostral Qui-Quadrado (n=200)',
     xlab = 'valores amostrais', ylab = 'Densidade')

t = rt(200, df=2)
plot(density(t), main = 'Distribuição amostral T-Student (n=200)',
     xlab = 'valores amostrais', ylab = 'Densidade')

