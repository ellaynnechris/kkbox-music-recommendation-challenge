# Load packages
library('digest')
library('splitstackshape')

################ Diretório dos arquivos ################

diretorioPadrao <- "D:/TestesR/Kaggle/"
arquivoTrain <- paste0(diretorioPadrao,"train.csv")
arquivoSongs <- paste0(diretorioPadrao,"songs.csv")
arquivoInfoExtraSongs <- paste0(diretorioPadrao,"song_extra_info.csv")
arquivoMembers <- paste0(diretorioPadrao,"members.csv")
arquivoTest <- paste0(diretorioPadrao,"test.csv")

################ Limpando as variáveis ################

train <- NULL
songs <- NULL
infoExtraSongs <- NULL
members <- NULL

################ Importação dos dados dos arquivos ################

train <- read.csv(arquivoTrain, sep = ",", na.strings = "")

songs <- read.csv(arquivoSongs, sep = ",", na.strings = "")

#artist_name_md5 <- NULL
#artist_name_md5 <-  digest(artist_name, "md5") # sapply(artist_name, digest, algo = 'md5')

#infoExtraSongs <- read.csv(arquivoInfoExtraSongs, sep = ",", na.strings = "")

members <- read.csv(arquivoMembers, sep = ",", na.strings = "")


################ Separação dos gêneros da música ################

songs_genre <- NULL

songs_genre <- songs[, c("song_id", "song_length", "genre_ids", "artist_name", "language")]

songs_genre <-cSplit (songs_genre, "genre_ids", "|", "long")

songs <- NULL


################ Merge dos demais arquivos com o TRAIN ################

trainJoins <- NULL
trainJoins <- train
trainJoins <- merge(x = trainJoins, y = songs_genre, by = "song_id", all.x = TRUE)
#trainJoins <- merge(x = trainJoins, y = infoExtraSongs, by = "song_id", all.x = TRUE)
trainJoins <- merge(x = trainJoins, y = members, by = "msno", all.x = TRUE)
str(trainJoins$bd)

trainJoins$target <- as.factor(trainJoins$target)
write.csv(trainJoins, "D:/TestesR/Kaggle/Versao1/trainJoins.csv", quote = FALSE, row.names = FALSE, na = 'NA')

#trainJoins2 <- NULL
#trainJoins2 <- trainJoins[, c("target", "genre_ids", "artist_name", "language", "city", "bd", "gender")]

train <- NULL
songs <- NULL
#infoExtraSongs <- NULL
members <- NULL
songs_genre <- NULL

################ CRIAÇÃO DOS MODELOS NO H20 ################


library("h2o")
h2o.init()

#trainJoins2 <- NULL
#trainJoins2 <- read.csv("D:/TestesR/Kaggle/Versão 1 -06.02.2018/trainJoins.csv", sep = ",")

dataTrain <- NULL
dataTrain <- as.h2o(trainJoins[,c("target",
                                  "genre_ids","language", "city", "bd",
                                  "gender")])
#trainJoins <- NULL
summary(dataTrain)

#options(OutDec= ".") 
#data.split <- h2o.splitFrame(data = data.h, ratios = c(0.7,0.2), seed = 1234)

# Coluna que se deseja prever
myY <- "target"

# Coluna que deve ser ignorada pelo modelo
ignored_columns <- c("target")


myX <- setdiff(setdiff(names(dataTrain), myY), ignored_columns)

gbm <- NULL
# GBM
gbm <- h2o.gbm(x = myX,
               y = myY,
               model_id = "gbm",
               training_frame    = dataTrain,
               nfolds            = 5,
               ntrees            = 200,
               max_depth         = 50,
               balance_classes = TRUE)

h2o.auc(h2o.performance(gbm)) #0.613
h2o.confusionMatrix(gbm@model$training_metrics)
h2o.confusionMatrix(gbm@model$cross_validation_metrics)
gbm@model$cross_validation_metrics@metrics$AUC #0.601
gbm@model$cross_validation_metrics@metrics$r2 #0.037


################ REALIZAR PREDIÇÃO NO H20 ################

#Leitura do arquivo de Teste e Join com demais arquivos
test <- NULL
test <- read.csv(arquivoTest, sep = ",",na.strings = "")

testJoins <- NULL
testIds <- NULL

testIds <-  data.frame("id"=test$id)
testJoins <- test
testJoins <- merge(x = testJoins, y = songs_genre, by = "song_id", all.x = TRUE)
#testJoins <- merge(x = testJoins, y = infoExtraSongs, by = "song_id", all.x = TRUE, sort = TRUE)
testJoins <- merge(x = testJoins, y = members, by = "msno", all.x = TRUE)
testJoins <- merge(x = testJoins, y = testIds, by = "id", all.x = TRUE)

train <- NULL
songs <- NULL
#infoExtraSongs <- NULL
members <- NULL
songs_genre <- NULL
test <- NULL
testIds <- NULL

testJoins2 <- testJoins[, c("id", "genre_ids","language", "city", "bd", "gender")]

write.csv(testJoins, file = "D:/TestesR/Kaggle/Versão 1 -06.02.2018/testJoins.csv", quote = FALSE, row.names = FALSE)

dataTest<-NULL
#predictTest <- h2o.importFile("D:/TestesR/Kaggle/testJoins3.csv")
dataTest <- as.h2o(testJoins2)

ResultadoPredicao <- NULL
ResultadoPredicao <- h2o.predict(gbm, dataTest, type = "response")
str(ResultadoPredicao)

h2o.exportFile(ResultadoPredicao, path="D:/TestesR/Kaggle/predicao.csv")

testJoins <- NULL
testJoins2 <- NULL
trainJoins <- NULL

################ Tirar a duplicidade dos ids ################


arquivo <- NULL
arquivoResultado <- data.frame("id"= as.numeric(dataTest$id), 
                               "target" = as.numeric(ResultadoPredicao$predict))

#Excluindo a última linha duplicada

predicaoJoins <- NULL
predicaoJoins <- arquivoResultado[!duplicated(arquivoResultado$id, fromLast = FALSE),]
write.csv(predicaoJoins, file = "D:/TestesR/Kaggle/Versão 1 -06.02.2018/resultado.csv", quote = FALSE, row.names = FALSE)

#Excluindo a primeira linha duplicada
predicaoJoins <- NULL
predicaoJoins <- arquivoResultado[!duplicated(arquivoResultado$id, fromLast = TRUE),]
write.csv(predicaoJoins, file = "D:/TestesR/Kaggle/Versão 1 -06.02.2018/resultado.csv2", quote = FALSE, row.names = FALSE)
