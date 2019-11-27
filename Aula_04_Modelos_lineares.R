#'========================================================================================#
#' Curso R 2019 - Novembro
#' Autor  : Germano
#' Versao : 1.0 (27/11/19)
#'========================================================================================#
# Parte 8: Moddelos lineares------
#'---------------------------------------------------------------------------------------- 
source("https://raw.githubusercontent.com/gcostaneto/Funcoes_naive/master/instpackage.R");

pkg = c("plyr","reshape2","ggplot2","ggpubr","superheat",
        "emmeans","lme4","BGLR","stats")

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

round(XtX,3);

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
lm(TMAX~1+SRAD,data);
lm(TMAX~0+SRAD,data); # intercepto null
lm(TMAX~SRAD,data);   # idem ao primeiro caso
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
model3 = lm(TMAX~1+poly(SRAD,3),data);

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

X = cbind(1,data$SRAD);
y = c(data$TMAX);

XtX <- crossprod(X,X)
Xty <- crossprod(X,y)

BETA <- solve(XtX, Xty) # vetor de coeficientes (solucoes)
yhat <- X%*%BETA        # predicao do modelo
e    <- y - yhat        # erro (desvio da realidade!)

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
  geom_point()+facet_grid(ANO~.)

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
  geom_abline(slope = 1,linetype="dashed")


ggplot(data, aes(x=SRAD, y=TMAX,colour=MES)) +
  geom_point()+
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
 # geom_point()+
  stat_smooth(method="lm",formula = y~poly(x,2),se=F)



## Modelos Qualitativos

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



model=lm(TMAX~0+SRAD,data)
summary(model)
coef(model)
anova(model)
residuals(model)
predict(model)

cor(data$TMAX,predict(model))



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