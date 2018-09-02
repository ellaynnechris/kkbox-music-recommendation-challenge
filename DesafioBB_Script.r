###############################################################################################
#                                             DESAFIO BB                                      #
#                                                                                             #
# Autora: Ellaynne Christine R. de Moraes Sousa                                               #
# Data: 1 de setembro de 2018                                                                 #
# Objetivo: Desafio de implementa��o de modelo predito cujo objetivo � prev� se um individuo  #
#           ganha mais de US $50.000. O DataSet mostra a renda dos indiv�duos usando dados    #
#           coletados do Censo de 1994 nos EUA. Dever� ser escolhido o melhor algoritmo para  #
#           melhor modelar os dados.                                                          #
###############################################################################################


### Load packages

library('digest')
library('splitstackshape')
library("ggplot2")
library("corrplot")
library("plyr")


################ Diret�rio e importa��o do(s) arquivo(s) ################

diretorioPadrao <- "D:/TestesR/DesafioBB/"
arquivoTrain <- paste0(diretorioPadrao,"census.csv")

train <- NULL
train <- read.csv(arquivoTrain, sep = "," , na.strings = c('NA', '', 'NULL')) # valores passados para o par�metro na.strings devem ser interpretados como NA


################ An�lise dos dados (missing, outliers, ...) ################

summary(train)

boxplot(train)
boxplot(train$capital.gain) #possui outliers


levels(train$sex) # �nico valor "MAL" na vari�vel. Assumi que deveria ser "Male" e o corrigi na linha abaixo.
revalue(train$sex, c(" Mal" = " Male")) -> train$sex

train <- na.omit(train) #apenas uma linha com valores NA, ent�o essa linha foi omitida

#Convers�o de vari�veis factor em numeric para plotar a correla��o entre as vari�veis
train_numerics <- NULL
train_numerics$age <- train$age
train_numerics$workclass <- as.numeric(train$workclass)
train_numerics$education_level <- as.numeric(train$education_level)
train_numerics$education.num <- train$education.num
train_numerics$marital <- as.numeric(train$marital.status)
train_numerics$occupation <- as.numeric(train$occupation)
train_numerics$relationship <- as.numeric(train$relationship)
train_numerics$race <- as.numeric(train$race)
train_numerics$sex <- as.numeric(train$sex)
train_numerics$hours.work <- as.numeric(train$hours.per.week)
train_numerics$country <- as.numeric(train$native.country)
train_numerics$income <- as.numeric(train$income)

train_numerics <- na.omit(data.frame(train_numerics))

corrplot(cor(train_numerics), main="Correla��o entre vari�veis", method="circle", type="lower", order="hclust", addCoef.col = "black")



################ Transforma��o das vari�veis e cria��o de nova coluna ################

#Cria��o da vari�vel Target (como factor, j� que os modelos ser�o do tipo Supervisionado Classificat�rio) a partir da vari�vel Income

levels(train$income)

train$target <- NULL
train[train$income != ">50K", "target"] <- 0
train[train$income == ">50K", "target"] <- 1
train$target <- as.factor(train$target)
summary(train$target)


################ CRIA��O DOS MODELOS NO H2o ################


library("h2o")
h2o.init()

dataTrain <- NULL
dataTrain <- as.h2o(train)

#Divis�o do dataset em Treinamento, Valida��o e Teste (para os modelos que n�o ir�o utilizar o cross-validation)

options(OutDec= ".") 
data_split <- h2o.splitFrame(data = dataTrain, ratios = c(0.7,0.2), seed = 1234)

dados.treino <- data_split[[1]]
dados.validacao <- data_split[[2]]
dados.teste <- data_split[[3]]

# Coluna que se deseja prever
myY <- "target"

# Coluna que deve ser ignorada pelo modelo
ignored_columns <- c("target", "income", "workclass", "education_level", "marital.status",
                     "occupation", "relationship", "race", "capital.gain", "capital.loss",
                     "native.country" )


myX <- setdiff(setdiff(names(dataTrain), myY), ignored_columns)

## O GBM � um algoritmo de aprendizagem supervisionada baseado em �rvores de decis�o utilizado em problemas de classifica��o e regress�o
#GBM sem cross validation
gbm <- NULL
gbm <- h2o.gbm(x = myX,
               y = myY,
               model_id = "gbm",
               training_frame    = dados.treino,
               validation_frame  = dados.validacao,
               balance_classes = TRUE)


h2o.logloss(gbm)
plot(gbm@model$scoring_history$validation_logloss)

gbm@model$validation_metrics
h2o.auc(h2o.performance(gbm))



#GBM com cross validation = 5
#Parametriza��o do modelo para definir profundidade da �rvore (max_depth)
gbm_cross_validation <- NULL
gbm_cross_validation <- h2o.gbm(x = myX,
               y = myY,
               model_id = "gbm_cross_validation",
               training_frame    = dataTrain,
               nfolds            = 5,
               max_depth         = 10,
               balance_classes = TRUE)


h2o.logloss(gbm_cross_validation)
plot(gbm_cross_validation@model$scoring_history$training_logloss)

gbm_cross_validation@model$cross_validation_metrics_summary 
gbm_cross_validation@model$cross_validation_metrics@metrics$AUC 



# O Deep Learning � baseado em Redes Neurais Artificiais (RNA), que s�o algoritmos inspirados nas liga��es neurol�gicas que constituem o c�rebro dos seres vivos.

# DEEP LEARNING sem cross validation
deep_learning <- h2o.deeplearning(
  x = myX,
  y = myY,
  model_id = "deep_learning",
  training_frame = dados.treino,
  validation_frame = dados.validacao,
  epoch = 12, #itera��es de conex�es
  #activation  = "Rectifier",
  seed = 1234
)


h2o.logloss(deep_learning)
plot(deep_learning@model$scoring_history$validation_logloss)

deep_learning@model$validation_metrics
h2o.auc(h2o.performance(deep_learning))

#DEEP LEARNING com cross validation = 5
deep_learning_cross_validation <- h2o.deeplearning(
  x = myX,
  y = myY,
  model_id = "deep_learning_cross_validation",
  training_frame = dataTrain,
  nfolds = 5
  #epoch = 12,
  #hidden = c(5,5),
  #activation  = "Rectifier",
  #seed = 1234
)

h2o.logloss(deep_learning_cross_validation)
plot(deep_learning_cross_validation@model$scoring_history$training_logloss)

deep_learning_cross_validation@model$cross_validation_metrics_summary 


################ REALIZAR PREDI��O NO H20 ################


write.csv(testJoins, file = "D:/TestesR/Kaggle/Versao3/testJoins.csv", quote = FALSE, row.names = FALSE)


ResultadoPredicao <- NULL
ResultadoPredicao <- h2o.predict(gbm, dados.teste, type = "response")

h2o.exportFile(h2o.cbind(dados.teste$target, ResultadoPredicao$predict), paste0(diretorioPadrao,"predicao.csv"))


# --------------------------------------------------------------------------------------------------
#                                                Conclus�o

# Muitos testes foram realizados at� chegar nos algoritmos apresentados: foram passadas diferentes combina��es de vari�veis independentes a serem consideradas pelos algoritmos e diferentes valores de par�metros de configura��o para a constru��o dos modelos.
#
# Os modelos encontrados podem ser considerados bons, j� que as principais m�tricas de avalia��o para machine learning tiveram resultados satisfat�rios, tais como: 
#  Acur�cia, que calcula a proximidade dos valores obtidos experimentalmente e dos valores reais esperados; Sensibilidade, que � a capacidade de identificar corretamente as classes positivas; Especificidade, que identifica corretamente as classes negativas; Logarithmic loss, que consiste em punir classifica��es erradas e de alta confian�a feitas pelo modelo.
#
# Dentre as solu��es apresentadas, os modelos 1 (gbm) e 3 (deep_leaning) possuem os melhores valores (e bem pr�ximos) para as m�tricas que ser�o consideradas nesse trabalho (Acur�cia e Log Loss), sendo o primeiro (gbm) o que representar� a solu��o final escolhida para o desafio proposto.

