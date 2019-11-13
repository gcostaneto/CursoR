#'========================================================================================#
#' Curso R 2019 - Novembro
#' Autor  : Germano
#' Versao : 1.0 (13/11/19)
#'========================================================================================#
# Tipos de graficos e modelos lineares ----
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
