###############################################################################################
#                                             DESAFIO BB                                      #
#                                                                                             #
# Autora: Ellaynne Christine R. de Moraes Sousa                                               #
# Data: 1 de setembro de 2018                                                                 #
# Objetivo: Desafio de implementação de modelo predito cujo objetivo é prevê se um individuo  #
#           ganha mais de US $50.000. O DataSet mostra a renda dos indivíduos usando dados    #
#           coletados do Censo de 1994 nos EUA. Deverá ser escolhido o melhor algoritmo para  #
#           melhor modelar os dados.                                                          #
###############################################################################################


### Load packages

library('digest')
library('splitstackshape')
library("ggplot2")
library("corrplot")
library("plyr")


################ Diretório e importação do(s) arquivo(s) ################

diretorioPadrao <- "D:/TestesR/DesafioBB/"
arquivoTrain <- paste0(diretorioPadrao,"census.csv")

train <- NULL
train <- read.csv(arquivoTrain, sep = "," , na.strings = c('NA', '', 'NULL')) # valores passados para o parêmetro na.strings devem ser interpretados como NA


################ Análise dos dados (missing, outliers, ...) ################

summary(train)

boxplot(train)
boxplot(train$capital.gain) #possui outliers


levels(train$sex) # único valor "MAL" na variável. Assumi que deveria ser "Male" e o corrigi na linha abaixo.
revalue(train$sex, c(" Mal" = " Male")) -> train$sex

train <- na.omit(train) #apenas uma linha com valores NA, então essa linha foi omitida

#Conversão de variáveis factor em numeric para plotar a correlação entre as variáveis
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

corrplot(cor(train_numerics), main="Correlação entre variáveis", method="circle", type="lower", order="hclust", addCoef.col = "black")



################ Transformação das variáveis e criação de nova coluna ################

#Criação da variável Target (como factor, já que os modelos serão do tipo Supervisionado Classificatório) a partir da variável Income

levels(train$income)

train$target <- NULL
train[train$income != ">50K", "target"] <- 0
train[train$income == ">50K", "target"] <- 1
train$target <- as.factor(train$target)
summary(train$target)


################ CRIAÇÃO DOS MODELOS NO H2o ################


library("h2o")
h2o.init()

dataTrain <- NULL
dataTrain <- as.h2o(train)

#Divisão do dataset em Treinamento, Validação e Teste (para os modelos que não irão utilizar o cross-validation)

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

## O GBM é um algoritmo de aprendizagem supervisionada baseado em árvores de decisão utilizado em problemas de classificação e regressão
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
#Parametrização do modelo para definir profundidade da árvore (max_depth)
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



# O Deep Learning é baseado em Redes Neurais Artificiais (RNA), que são algoritmos inspirados nas ligações neurológicas que constituem o cérebro dos seres vivos.

# DEEP LEARNING sem cross validation
deep_learning <- h2o.deeplearning(
  x = myX,
  y = myY,
  model_id = "deep_learning",
  training_frame = dados.treino,
  validation_frame = dados.validacao,
  epoch = 12, #iterações de conexões
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


################ REALIZAR PREDIÇÃO NO H20 ################


write.csv(testJoins, file = "D:/TestesR/Kaggle/Versao3/testJoins.csv", quote = FALSE, row.names = FALSE)


ResultadoPredicao <- NULL
ResultadoPredicao <- h2o.predict(gbm, dados.teste, type = "response")

h2o.exportFile(h2o.cbind(dados.teste$target, ResultadoPredicao$predict), paste0(diretorioPadrao,"predicao.csv"))


# --------------------------------------------------------------------------------------------------
#                                                Conclusão

# Muitos testes foram realizados até chegar nos algoritmos apresentados: foram passadas diferentes combinações de variáveis independentes a serem consideradas pelos algoritmos e diferentes valores de parâmetros de configuração para a construção dos modelos.
#
# Os modelos encontrados podem ser considerados bons, já que as principais métricas de avaliação para machine learning tiveram resultados satisfatórios, tais como: 
#  Acurácia, que calcula a proximidade dos valores obtidos experimentalmente e dos valores reais esperados; Sensibilidade, que é a capacidade de identificar corretamente as classes positivas; Especificidade, que identifica corretamente as classes negativas; Logarithmic loss, que consiste em punir classificações erradas e de alta confiança feitas pelo modelo.
#
# Dentre as soluções apresentadas, os modelos 1 (gbm) e 3 (deep_leaning) possuem os melhores valores (e bem próximos) para as métricas que serão consideradas nesse trabalho (Acurácia e Log Loss), sendo o primeiro (gbm) o que representará a solução final escolhida para o desafio proposto.

