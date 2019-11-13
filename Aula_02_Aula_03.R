#'========================================================================================#
#' Curso R 2019 - Novembro
#' Autor  : Germano
#' Versao : 1.0 (6/11/19)
#'========================================================================================#
# Graficos e tabelas (estatistica descritiva) ----
#'========================================================================================#

# chamando pacotes (noob mode)
require(plyr)
require(reshape2)
require(ggplot2)
require(ggpubr)

# chamando pacotes (pro mode)
pkg = c("plyr","reshape2","ggplot2","ggpubr")

# criando
inst.package <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


inst.package(pkg);

# Mais facil ainda: crie um repositorio no seu github para poder salvar funcoes
source("https://raw.githubusercontent.com/gcostaneto/Funcoes_naive/master/instpackage.R");
inst.package(pkg);

#'----------------------------------------------------------------------------------------
# Parte 1: Leitura e diagnostico de estruturas de datasets                           -----
#'----------------------------------------------------------------------------------------
ls()
#rm(list=ls())
home.dir = getwd(); # diretorio home
data.dir = "C:/Users/germano/Documents/Curso R 2019/"; # diretorio dos datasets

setwd() # direciona para diretorio desejado
setwd(dir = data.dir);

ls()
list.files() # lista files no diretorio atual ou diretorio especifico (ver path)
list.files(pattern = "pira"); # lista com base num padrao
list.files(pattern = ".csv")[1];

data = read.csv(file = "piraclim.csv",header = T);

# via listagem de files (util para loops)
data = read.csv(file = list.files(pattern = ".csv")[1],header = T);

# 1.1 str() nos retorna diagnostico de estruturas                                    -----
str(data)
# 1.2 class() nos retorna em qual classe o objeto importado se enquadra              -----
class(data)
#data=as.matrix(data)
#data = as.data.frame(data)

# 1.3 levels(): aplicados a vetores de fatores                                       -----   
data[,2]
data$MES
levels(data$MES)
length(levels(data$MES))
nlevels(data$MES)

levels(data$MES)[c(2,4,6,8,10,12,14,16,18,20,22,24)] = levels(data$MES)[c(1,3,5,7,9,11,13,15,17,19,21,23)]
levels(data$MES)

# levels(data$MES)[seq(1,24,by=2)]
# seq(2,25,by=2)

#'----------------------------------------------------------------------------------------
# Parte 2: estatisticas basicas e uso do plyr  -----
#'----------------------------------------------------------------------------------------

# 2.1 Funcao summary nos retorna um diagnostico de cada coluna                       ----- 

summary(data)
summary(data$SRAD)
summary(data$DIA)
summary(data$MES)

# 2.2 Funcao table() faz contagens de elementos                                      ----- 

table(data$MES);
table(data$SRAD);
table(data$MES,data$SRAD);

# 2.3 Outras funcoes                                                                 ----- 

contagem = table(data$MES,data$SRAD)
class(contagem)
str(contagem)
dim(contagem)

apply(X = contagem,MARGIN = 1,FUN = sum);
apply(X = contagem,MARGIN = 1,FUN = mean);
apply(X = contagem,MARGIN = 1,FUN = var);

head(data)
head(data[,-c(1:3)]);
dim(data)
apply(data[,-c(1:3)],2,sum);
apply(data[,-c(1:3)],1,sum)


colSums(data[,-c(1:3)]);
rowSums(data[,-c(1:3)]);

# criando uma funcao
soma.colunas = function(dados,margin){
  somas=apply(dados,margin,sum)
  return(somas)
}

soma.colunas(data[,-c(1:3)],margin = 2);


# 2.4 Funcao cut() gera discretizacoes de variaveis continuas                        ----- 

# breaks nos diz os limites das variaveis

summary(data$SRAD);

cut(data$SRAD, breaks = c(0,10, 20, 30, 40));

table(cut(data$SRAD, breaks = c(0,20, 40)));

# 2.5 Uso do plyr: gerando medias gerais e por grupo                                 ----- 

# Estrutura: (dataset, .(variaveis indicadoras), funcao, respostas)

# familia de plyrs:

# from data frame to data frame : ddply
# from lists      to data.frame : ldply
# from data frame to lists      : dlply
# running functions             : adply

# primos pobres:
# apply, lapply, tapply, do.call()

# exemplo: medias de SRAD por mes:
ddply(data, .(MES), summarise, MediaSRAD=mean(SRAD));

# exemplo: arredondando para 2 casas decimais via round(x, 2)
ddply(data, .(MES), summarise, MediaSRAD = round(mean(SRAD),2));

resultados=ddply(data, .(ANO,MES), summarise, 
      MediaSRAD = round(mean(SRAD,na.rm = T),2),
      VarSRAD = var(SRAD,na.rm = T),
      MedSRAD = median(SRAD,na.rm = T),
      MedWIND = median(WIND,na.rm = T));

#cor()
ddply(data,.(ANO),function(x) cor(x$TMAX,x$SRAD))

# usando o primo pobre
tapply(X = data$SRAD,INDEX = data$MES,FUN = mean);

# lidando com vetores
data$SRAD[data$SRAD > 20]
data$SRAD[data$SRAD > 30]
data$SRAD[data$DATA %in% "10-11-2019"]

indice = data$WIND < 0
data$WIND[indice];
data$WIND[indice] = NA
data$WIND[data$WIND %in% "morreu"]

#'----------------------------------------------------------------------------------------
# 2.6 Criando seu proprio summary  via plyr usando aas funcoes (exercicio)                                        ----
sd()        # desvio padrao
median()    # mediana
quantile()  # quantis
max()       # maximo vlaor
min()       # minimo valor
range()     # maximo e minimo valores
var()       # variancia

#'----------------------------------------------------------------------------------------
# Parte 3: Pacotes ggplot e ggpubr -----
#'----------------------------------------------------------------------------------------

# vejam! (salvem isso no fundo do coracao <3)
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://beanumber.github.io/sds192/lab-ggplot2.html
# https://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization


# Estrutura: ggplot() + geom_algumacoisa() + theme()

# 3.1 Descritores de distribuicao de frequencias

ggplot(data, aes(x=reorder(MES,-SRAD),y=SRAD))+geom_boxplot()

data$ANO = as.factor(data$ANO)

str(data)
ggplot(data, aes(x=ANO,y=SRAD))+geom_boxplot()

ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+geom_boxplot()
ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+geom_boxplot()


ggplot(data, aes(x=ANO,y=SRAD,fill=ANO))+geom_boxplot()+facet_grid(~MES)
ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+geom_boxplot()+facet_wrap(~MES)

ggplot(data, aes(x=ANO,y=SRAD))+ geom_violin();

ggplot(data, aes(x=ANO,y=SRAD))+ geom_bar(stat="identity");
ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+geom_bar(stat="identity", position=position_dodge());
ggplot(data, aes(x=ANO,y=SRAD,fill=ANO))+ geom_bar(stat="identity");

ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+ geom_bar(stat="identity",position = "fill");
ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+ geom_bar(stat="identity",position = "fill");

#'========================================================================================#
# Graficos e Variancia-Covariancia  ----
#'========================================================================================#
# Parte 4: reshape2: derretendo planilhas  (adicional) -----
#'---------------------------------------------------------------------------------------- 

head(data)

names(data)
names(data)[1] # nome da primeira coluna
names(data)[1:5] # nome das coluans de 1 a 5
names(data)[c(5,3,1,2)] # nome das colunas 5,3,1 e 2

# measure.vars = nome das colunas a serem mensuradas (derretidas)
data2 = melt(data,measure.vars = names(data)[c(4:11)]);
head(data2)

# id.vars = nome das colunas a nao serem derretidas, isto e, sao as identificacoes
data2 = melt(data,id.vars =  names(data)[c(1:3)]);
head(data2)

# variable.name = nomeando ID output
data2 = melt(data,id.vars =  names(data)[c(1:3)],variable.name = "Clima");
head(data2);

# value.name = nomeando o valor do output
data2 = melt(data,
             id.vars =  names(data)[c(1:3)],
             variable.name = "Clima",
             value.name = "valor");
head(data2)

data2 = melt(data,id.vars =  names(data)[c(1:3)]);
head(data2);


#'---------------------------------------------------------------------------------------- 
# Parte 5: aplicacoes fazendo figuras -----
#'---------------------------------------------------------------------------------------- 

ggplot(data2, aes(x=ANO,y=value,fill=variable))+ 
  geom_bar(stat="identity", position=position_dodge())+
  facet_grid(~variable)


ggplot(data2, aes(x=ANO,y=value,fill=variable))+ 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(~variable)


ggplot(data2, aes(x=ANO,y=value,fill=variable))+ 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(~variable,scales = "free")

ggplot(data2, aes(x=MES,y=value,fill=variable))+ 
  facet_wrap(~variable,scales = "free")+geom_boxplot()

# usando ggpubr
ggboxplot(data2, x = "MES", y = "value",
          fill = "variable")+
  facet_wrap(~variable,scales = "free")

# editando a figura...
ggboxplot(data2, x = "MES", y = "value",
          fill = "variable")+
  facet_wrap(~variable,scales = "free")+
  theme(axis.text.x = element_text(angle=90));


# fazendo nossa funcao summary ------

summary.nosso = function(x) #

#'---------------------------------------------------------------------------------------- 
# Parte 6: Medidas de associacao entre variaveis     -----
#'---------------------------------------------------------------------------------------- 


# 6.0 Variancia-Covariancia ------
cov(x = data$SRAD,y = data$TMAX)

var(data$SRAD)
var(data$TMAX)

cov(data[,4:5])

# 6.1 Correlacao par a par ------
# correlacoes sao covariancias normalizadas

cor(x = data$SRAD,y = data$TMAX)

cor(data$SRAD,data$TMAX)

cor(data$SRAD,data$TMAX,use = "complete.obs");
cor(data$SRAD,data$TMAX,method = "Spearman");
cor(data$SRAD,data$TMAX,method = "spearman");

# pearson : correlacao quantitativa
# kendall e spearman: correlacao entre ranks (qualitativa)

# 6.2 Matriz de correlacoes -----
cor(data[,c(4:5)])

names(data)
cor(data[,c(4:11)])
cor(data[,c(4:11)],use = "complete.obs")

round(cor(data[,c(4:11)],use = "complete.obs"),2)


#'---------------------------------------------------------------------------------------- 
# Parte 7: fazendo correlacoes pro ano ------
#'---------------------------------------------------------------------------------------- 

ddply(data,.(ANO), summarise, cor(x = SRAD,y = TMAX))

ddply(data,.(ANO), summarise, correlacao = cor(x = SRAD,y = TMAX))

#'---------------------------------------------------------------------------------------- 
# Algebra Matricial ------
#'---------------------------------------------------------------------------------------- 

# %*%  =  produto de matrizes
# *    =  produto de haddamard (elemento a elemento)
# %x%  =  produto de kronecker (elemento por matriz)

# teste 1: sem escalonar

X = as.matrix(data[,4:5])
dim(X)

(XtX=t(X)%*%X/nrow(X))  # X'X = coluna por coluna;  XX' = linha por linha (obs : nrow X = ncol X)
dim(XtX)

round(XtX,3)

# teste 2: centrando na media de cada variavel

X = scale(as.matrix(data[,4:5]),center = T,scale = F)
dim(X)

(XtX=t(X)%*%X/nrow(X))

round(XtX,3)

# teste 3: centrando na media e normalizando

X = scale(as.matrix(data[,4:5]),center = T,scale = T)
dim(X)

(XtX=t(X)%*%X/nrow(X))

round(XtX,3);

#'---------------------------------------------------------------------------------------- 
# Parte 8: Moddelos lineares------
#'---------------------------------------------------------------------------------------- 

# 8.1 Introducao  ---------------
# linear model (lm)

lm(TMAX~1+SRAD,data)

model=lm(TMAX~1+SRAD,data)

summary(model)
coef(model)
anova(model)
residuals(model)
predict(model)

cor(data$TMAX,predict(model))
