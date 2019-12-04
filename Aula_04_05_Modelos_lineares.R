#'========================================================================================#
#' Curso R 2019 - Novembro
#' Autor  : Germano
#' Versao : 1.0 (27/11/19)
#'========================================================================================#
# Parte 8: Moddelos lineares------
#'---------------------------------------------------------------------------------------- 

# OBS: importem o dataset piraclim.csv!

piraclim = read.csv(file = "https://raw.githubusercontent.com/gcostaneto/CursoR/master/piraclim.csv")
str(piraclim)

piraclim$ANO = as.factor(piraclim$ANO)


source("https://raw.githubusercontent.com/gcostaneto/Funcoes_naive/master/instpackage.R");

pkg = c("plyr","reshape2","ggplot2","ggpubr","superheat",
        "emmeans","lme4","BGLR","stats","rcompanion","multcompView")

inst.package(pkg);

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

cov(data[,4:5])
cor(data[,4:5])
round(XtX,3);

crossprod(X,X)/nrow(X);

#' ---------------------------------------------------------------------------------------
# 8.1 Introducao                                                                   -------
#'---------------------------------------------------------------------------------------- 

# Modelos cujos efeitos sao LINEARES
# (temos modelos multivariados, nao-lineares)

# Modelos quantitativos x Modelos qualitativos

#### Conceitos chave (pra filosofar):
#'---------------------------------------------------------------------------------------- 

# (1) Coeficiente x Efeito
# (2) Residuo x Erro
# (3) Soma de quadrados x Quadrado Medio
# (4) Erro Padrao x Desvio Padrao
# (5) valor de F (ou t) x p-value
#'---------------------------------------------------------------------------------------- 
# (6) graus de liberdade
# (7) parametros do modelo
# (8) restricoes do modelo

#'---------------------------------------------------------------------------------------- 
# linear model (lm): o basico do R                                                   ------
#'---------------------------------------------------------------------------------------- 


# y~x   (outputs: intercept + error)
# X[x1 + x2 + x3 + ... xn]

# voltando ao exemplo maçante do Tmax vs SRAD

lm(TMAX~SRAD,data = data);   # idem ao primeiro caso

lm(TMAX~1,data);
mean(data$TMAX);

lm(TMAX~0+SRAD,data); # intercepto null

# o output do modelo é uma lista
# essa lista tem objetos e diagnosticos dos modelos
# podemos criar funcoes para extrair esses objetos

model = lm(TMAX~SRAD,data);

summary(model)  # sumario de ajuste do modelo
anova(model)    # analysis of variance

coef(model)      # coeficientes do modelo
residuals(model) # residuos
predict(model)   # predicao (yhat)


# e se for polinomial?
model2 = lm(TMAX~1+SRAD+I(SRAD^2),data);
model3 = lm(TMAX~1+SRAD+I(SRAD^2)+I(SRAD^3),data);


model2 = lm(TMAX~1+poly(SRAD,2),data);
summary(model2)
model3 = lm(TMAX~1+poly(SRAD,3),data);
summary(model3)

AIC(object = model);
AIC(object = model2);
AIC(object = model3);

AIC(model,model2,model3);

# Bozdongan. H. Model selection and Akaike's Information Criterion (AIC): The general theory and its analytical extensions. Psychometrika. v.52, n.3, 345-370, Sep. 1987.

BIC(object = model)
BIC(object = model2)
BIC(object = model3)

BIC(model,model2,model3)

myBIC = BIC(model,model2,model3);
rownames(myBIC)
which.min(myBIC$BIC)

rownames(myBIC)[which.min(myBIC$BIC)]

testa.meu.model = function(.myBIC){ # declara argumento
  
  # faz operacoes
  saida = rownames(.myBIC)[which.min(.myBIC$BIC)]

  # retorna saida
  return(saida)
}

testa.meu.model(BIC(model,model2,model3));



#'---------------------------------------------------------------------------------------- 
# O que roda por tras?
#'---------------------------------------------------------------------------------------- 

X = cbind(1,data$SRAD-mean(data$SRAD));

X = cbind(1,scale(data$SRAD,center=T));

y = scale(c(data$TMAX),center=T)

Xtx <- t(X)%*%X
XtX <- crossprod(X,X)
Xty <- crossprod(X,y)

BETA <- solve(XtX, Xty) # vetor de coeficientes (solucoes)
yhat <- X%*%BETA        # predicao do modelo
e    <- y - yhat        # erro (desvio da realidade!)
var(e)
var(yhat)
var(y)

anova(model)
#'---------------------------------------------------------------------------------------- 
# e se colocarmos mais uma variavel no modelo?
#'---------------------------------------------------------------------------------------- 

model=lm(TMAX~1+SRAD+RAIN+SUNH,data)
summary(model)

X = cbind(1,data$SRAD,data$RAIN,data$SUNH);
y = c(data$TMAX);


XtX <- crossprod(X,X)
Xty <- crossprod(X,y)


BETA <- solve(XtX, Xty) # vetor de coeficientes (solucoes)
yhat <- X%*%BETA # predicao do modelo
e    <- y - yhat    # erro

U <- chol(XtX)
L <- t(U)

## LU = XlX.
L%*%U

## EstimaÃ§Ã£o.
d <- t(X)%*%y # idem a Xty <- crossprod(X,y)
z <- forwardsolve(L, d)
betas <- backsolve(U, z)

coef(model) # extrait os coeficientes do modelo


summary(model)
coef(model)
anova(model)
residuals(model)
predict(model)
#'---------------------------------------------------------------------------------------- 
## Plotagem
#'---------------------------------------------------------------------------------------- 

# scatterplot, geom_point(), geom_lines, geom_jitter().....

# visualizacao grafica
# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
# https://rpkgs.datanovia.com/ggpubr/reference/ggscatter.html

ggplot(data, aes(x=SRAD, y=TMAX)) +
  geom_point()

ggplot(data, aes(x=SRAD, y=TMAX,colour=ANO)) +
  geom_point()


ggplot(data, aes(x=SRAD, y=TMAX,colour=ANO)) +
  geom_point()+facet_wrap(~ANO)

ggplot(data, aes(x=SRAD, y=TMAX,colour=MES)) +
  geom_point()+
  geom_abline(slope = 1)

ggplot(data, aes(x=SRAD, y=TMAX,colour=MES)) +
  geom_point()+
  xlim(0,50)+ ylim(0,50)+
  geom_abline(slope = 1)


ggplot(data, aes(x=SRAD, y=TMAX,colour=MES)) +
  geom_point()+
  xlim(0,50)+ ylim(0,50)+
  geom_abline(slope = 1,
              linetype="dotted",
              size=1,colour="red")


ggplot(data, aes(x=SRAD, y=TMAX,colour=MES)) +
 # geom_point()+
  xlim(0,50)+ ylim(0,50)+
  geom_abline(slope = 1,linetype="dashed")+
  geom_smooth()

ggplot(data, aes(x=SRAD, y=TMAX,colour=MES)) +
  geom_point()+
  xlim(0,50)+ ylim(0,50)+
  geom_abline(slope = 1,linetype="dashed")+
  geom_smooth(method="lm")+
  theme(legend.position = c(.9,.5))


ggplot(data, aes(x=SRAD, y=TMAX,colour=ANO)) +
  geom_point()+
  facet_wrap(~ANO)+
 # scale_color_brewer(value=rainbow(10))+
  stat_smooth(method="lm",formula = y~poly(x,2),se=F)



#'---------------------------------------------------------------------------------------- 
## Modelos Qualitativos
#'---------------------------------------------------------------------------------------- 

## pressupostos:

# 1) O modelo é aditivo (efeitos se somam)
# 2) Os erros sao independentes (Ok, podemos modelar isso hoje em dia!)
# 3) Os erros (e observacoes) tem distribuicao normal (Ok 2, também da pra contornar isso)

## tipos de efeitos:

# Media (intercepto fixo) e erro (aleatorio) sempre estao no modelo
# os demais efeitos (tratamentos + desenho experimental) sao assumidos como:
# Fixo (desvios em relacao a media)
# Aleatorio (efeitos oriundos de uma distribuicao de probabilidade)

# Teorema de Gauss-Markov (modelo de efeito fixo)
# ver http://www4.eco.unicamp.br/docentes/gori/images/arquivos/EconometriaI/Econometria_RevisaoRegressaoLinear.pdf

model = lm(TMAX~MES+ANO,data)
summary(model)

Xmes = model.matrix(~0+MES,data=data)
Xano = model.matrix(~0+ANO,data=data)

X = cbind(1,Xmes,Xano)
y = c(data$TMAX); y = (y-mean(y))/sd(y)

#X = X-mean(X)/sd(X)

XtX <- crossprod(X,X)
Xty <- crossprod(X,y)



X = model.matrix(~MES+ANO,data)
y = c(data$TMAX); y = (y-mean(y))/sd(y)

#X = X-mean(X)/sd(X)

XtX <- crossprod(X,X)
Xty <- crossprod(X,y)

BETA <- solve(XtX, Xty) # vetor de coeficientes (solucoes)

yhat <- X%*%BETA # predicao do modelo
e    <- y - yhat    # erro


#'---------------------------------------------------------------------------------------- 
## Erro Padrao (standard error) ------
#'---------------------------------------------------------------------------------------- 

x = piraclim$SRAD
hist(x)
boxplot(x)

n = length(x) # tamanho de x
deviation = sd(x)/sqrt(n)

c(mean(x)-2*deviation,mean(x)+2*deviation)
# lower limit; upper limit

std <- function(x) sd(x)/sqrt(n) # nossa funcao para erro padrao

std(x)

# associando uma distribuicao (t, por exemplo)

(quantil_T <- qt(0.975,df=n-1)) # (1 - 0.975 = 0.025)
(std.error <- quantil_T*std(x)) 

(quantil_T <- qt(0.95,df=n-1)) # (1 - 0.95 = 0.05)
(std.error <- quantil_T*std(x))

c(mean(x)-std.error,mean(x)+std.error)
# lower limit; upper limit


## via bootstrap (distribuicao empirica = gerada pelos proprios dados)
# https://rcompanion.org/handbook/C_03.html


# usando pacote rcompanion
library(rcompanion)

groupwiseMean(SRAD ~ 1,
              data   = data,
              conf   = 0.95,
              digits = 6)


groupwiseMean(SRAD ~ ANO,
              data   = data,
              conf   = 0.95,
              digits = 6)

groupwiseMean(SRAD ~ ANO+MES,
              data   = data,
              conf   = 0.95,
              digits = 6)


#'---------------------------------------------------------------------------------------- 
# Testes ad-hoc
#'---------------------------------------------------------------------------------------- 

###  teste t (t test)

# pareado (comparar medias pareadas)
# uma amostra (comparar se uma media varia de outra media esperada)

t.test(conf.level  = 0.95)


## teste Tukey (comparacao de multiplas medias)

# no R gallery https://www.r-graph-gallery.com/84-tukey-test.html

model <- lm(TMAX~MES+ANO,data)
ANOVA <- aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'MES', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")


#'---------------------------------------------------------------------------------------- 
#' Rodando varias analises
#'---------------------------------------------------------------------------------------- 

# montar na aula


#'---------------------------------------------------------------------------------------- 
# Extra: modelos bayesianos
#'---------------------------------------------------------------------------------------- 

ETA<-list(list(~factor(MES)+factor(ANO),
               data=data,model='FIXED'))

fm<-BGLR(y=y,ETA=ETA, nIter=1200, burnIn=20)

fm$yHat # valores preditos
fm$mu # intercepto
fm$ETA

# bayesian ridge regression
# (regressao de cumeeira bayesiana [regressao aleatoria])

ETA<-list(list(~factor(MES)+factor(ANO),
               data=data,model='BRR'))