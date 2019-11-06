#'========================================================================================#
#' Curso R 2019 - Novembro
#' Autor  : Germano
#' Versao : 1.0 (6/11/19)
#'========================================================================================#
# Graficos e tabelas (estatistica descritiva)
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

home.dir = getwd(); # diretorio home
data.dir = "C:/Users/germano/Documents/Curso R 2019/"; # diretorio dos datasets

setwd() # direciona para diretorio desejado
setwd(dir = data.dir);

list.files() # lista files no diretorio atual ou diretorio especifico (ver path)
list.files(pattern = ".csv"); # lista com base num padrao
list.files(pattern = ".csv")[1];

data = read.csv(file = "piraclim.csv",header = T);

# via listagem de files (util para loops)
data = read.csv(file = list.files(pattern = ".csv")[1],header = T);

# 1.1 str() nos retorna diagnostico de estruturas                                    -----
str(data)
# 1.2 class() nos retorna em qual classe o objeto importado se enquadra              -----
class(data)
# 1.3 levels(): aplicados a vetores de fatores                                       -----   
levels(data$MES)
length(levels(data$MES))
nlevels(data$MES)

levels(data$MES)[c(2,4,6,8,10,12,14,16,18,20,22,24)] = levels(data$MES)[c(1,3,5,7,9,11,13,15,17,19,21,23)]
levels(data$MES)

# seq(1,24,by=2)
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

table(data$MES,data$SRAD);

# 2.3 Outras funcoes                                                                 ----- 

apply(X = table(data$MES,data$SRAD),MARGIN = 1,FUN = sum);
apply(data[,-c(1:3)],2,sum);
apply(data[,-c(1:3)],1,sum)


colSums(data[,-c(1:3)]);
rowSums(data[,-c(1:3)]);

# criando uma funcao
soma.colunas = function(dados){
  return(apply(dados,2,sum))
}

soma.colunas(data[,-c(1:3)]);


# 2.4 Funcao cut() gera discretizacoes de variaveis continuas                        ----- 

# breaks nos diz os limites das variaveis

summary(data$SRAD);
cut(data$SRAD, breaks = c(0,10, 20, 30, 40));

table(cut(data$SRAD, breaks = c(0,10, 20, 30, 40)));

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
ddply(data, .(MES), summarise, SRADmean = mean(SRAD));

# exemplo: arredondando para 2 casas decimais via round(x, 2)
ddply(data, .(MES), summarise, SRADmean = round(mean(SRAD),2));

# usando o primo pobre
tapply(X = data$SRAD,INDEX = data$MES,FUN = mean);

# lidando com vetores
data$SRAD[data$SRAD > 20]
data$SRAD[data$SRAD > 30]
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

ggplot(data, aes(x=ANO,y=SRAD))+geom_boxplot()

data$ANO = as.factor(data$ANO)

ggplot(data, aes(x=ANO,y=SRAD))+geom_boxplot()

ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+geom_boxplot()
ggplot(data, aes(x=MES,y=SRAD,fill=ANO))+geom_boxplot()


ggplot(data, aes(x=ANO,y=SRAD,fill=ANO))+geom_boxplot()+facet_grid(~MES)
ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+geom_boxplot()+facet_wrap(~MES)

ggplot(data, aes(x=ANO,y=SRAD))+ geom_violin()
ggplot(data, aes(x=ANO,y=SRAD))+ geom_bar(stat="identity")
ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+geom_bar(stat="identity", position=position_dodge());
ggplot(data, aes(x=ANO,y=SRAD,fill=ANO))+ geom_bar(stat="identity");
ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+ geom_bar(stat="identity",position = "fill");
ggplot(data, aes(x=ANO,y=SRAD,fill=MES))+ geom_bar(stat="identity",position = "fill");

#'----------------------------------------------------------------------------------------
# Parte 4: reshape2: derretendo planilhas  (adicional) -----
#'----------------------------------------------------------------------------------------

