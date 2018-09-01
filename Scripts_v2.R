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

trainJoins$bd_factor <- NULL
trainJoins[trainJoins$bd < 15, "bd_factor"] <- "Menor que 15"
trainJoins[trainJoins$bd >= 15 & trainJoins$bd < 19, "bd_factor"] <- "Entre 15 e 18"
trainJoins[trainJoins$bd >= 19 & trainJoins$bd < 23, "bd_factor"] <- "Entre 19 e 22"
trainJoins[trainJoins$bd >= 23 & trainJoins$bd < 27, "bd_factor"] <- "Entre 23 e 26"
trainJoins[trainJoins$bd >= 27 & trainJoins$bd < 31, "bd_factor"] <- "Entre 27 e 30"
trainJoins[trainJoins$bd >= 31 & trainJoins$bd < 35, "bd_factor"] <- "Entre 31 e 34"
trainJoins[trainJoins$bd >= 35 & trainJoins$bd < 39, "bd_factor"] <- "Entre 35 e 38"
trainJoins[trainJoins$bd >= 39 & trainJoins$bd < 43, "bd_factor"] <- "Entre 39 e 42"
trainJoins[trainJoins$bd >= 43 & trainJoins$bd < 47, "bd_factor"] <- "Entre 43 e 46"
trainJoins[trainJoins$bd >= 47 & trainJoins$bd < 51, "bd_factor"] <- "Entre 47 e 50"
trainJoins[trainJoins$bd >= 51, "bd_factor"] <- "Maior que 51"

trainJoins$bd_factor <- as.factor(trainJoins$bd_factor)

trainJoins$target <- as.factor(trainJoins$target)
#write.csv(trainJoins, "D:/TestesR/Kaggle/Versao2/trainJoins.csv", quote = FALSE, row.names = FALSE, na = 'NA')

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

dataTrain <- NULL
dataTrain <- as.h2o(trainJoins[,c("target",
                                  "genre_ids","language", "city", "bd_factor",
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
               ntrees            = 150,
               max_depth         = 150,
               nfolds            = 5,
               balance_classes = TRUE)

h2o.auc(h2o.performance(gbm)) #0.671
h2o.confusionMatrix(gbm@model$training_metrics)
h2o.confusionMatrix(gbm@model$cross_validation_metrics)
gbm@model$cross_validation_metrics@metrics$AUC #0.671
gbm@model$cross_validation_metrics@metrics$r2 #0.089


################ REALIZAR PREDIÇÃO NO H20 ################

# Separação dos gêneros da música 
songs <- read.csv(arquivoSongs, sep = ",", na.strings = "")
songs_genre <- NULL

songs_genre <- songs[, c("song_id", "song_length", "genre_ids", "artist_name", "language")]

songs_genre <-cSplit (songs_genre, "genre_ids", "|", "long")
songs <- NULL

#Leitura do arquivo de Teste e Join com demais arquivos

members <- read.csv(arquivoMembers, sep = ",", na.strings = "")
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

test <- NULL
#infoExtraSongs <- NULL
members <- NULL
songs_genre <- NULL

testJoins$bd_factor <- NULL
testJoins[testJoins$bd < 15, "bd_factor"] <- "Menor que 15"
testJoins[testJoins$bd >= 15 & testJoins$bd < 19, "bd_factor"] <- "Entre 15 e 18"
testJoins[testJoins$bd >= 19 & testJoins$bd < 23, "bd_factor"] <- "Entre 19 e 22"
testJoins[testJoins$bd >= 23 & testJoins$bd < 27, "bd_factor"] <- "Entre 23 e 26"
testJoins[testJoins$bd >= 27 & testJoins$bd < 31, "bd_factor"] <- "Entre 27 e 30"
testJoins[testJoins$bd >= 31 & testJoins$bd < 35, "bd_factor"] <- "Entre 31 e 34"
testJoins[testJoins$bd >= 35 & testJoins$bd < 39, "bd_factor"] <- "Entre 35 e 38"
testJoins[testJoins$bd >= 39 & testJoins$bd < 43, "bd_factor"] <- "Entre 39 e 42"
testJoins[testJoins$bd >= 43 & testJoins$bd < 47, "bd_factor"] <- "Entre 43 e 46"
testJoins[testJoins$bd >= 47 & testJoins$bd < 51, "bd_factor"] <- "Entre 47 e 50"
testJoins[testJoins$bd >= 51, "bd_factor"] <- "Maior que 51"

testJoins$bd_factor <- as.factor(testJoins$bd_factor)

testJoins2 <- testJoins[, c("id", "genre_ids","language", "city", "bd_factor", "gender")]

write.csv(testJoins, file = "D:/TestesR/Kaggle/Versao2/testJoins.csv", quote = FALSE, row.names = FALSE)

dataTest<-NULL
#predictTest <- h2o.importFile("D:/TestesR/Kaggle/testJoins3.csv")
dataTest <- as.h2o(testJoins2)

ResultadoPredicao <- NULL
ResultadoPredicao <- h2o.predict(gbm, dataTest, type = "response")

testJoins <- NULL
testJoins2 <- NULL
trainJoins <- NULL

################ Tirar a duplicidade dos ids ################

h2o.exportFile(h2o.cbind(dataTest$id, ResultadoPredicao$predict), "D:/TestesR/Kaggle/Versao2/predicao.csv")

arquivoResultado <- NULL
arquivoResultado <- read.csv("D:/TestesR/Kaggle/Versao2/predicao.csv",sep = ",")

#Excluindo a última linha duplicada

predicaoJoins <- NULL
predicaoJoins <- arquivoResultado[!duplicated(arquivoResultado$id, fromLast = FALSE),]
write.csv(predicaoJoins, file = "D:/TestesR/Kaggle/versao2/resultado.csv", quote = FALSE, row.names = FALSE)

#Excluindo a primeira linha duplicada
predicaoJoins <- NULL
predicaoJoins <- arquivoResultado[!duplicated(arquivoResultado$id, fromLast = TRUE),]
write.csv(predicaoJoins, file = "D:/TestesR/Kaggle/versao2/resultado2.csv", quote = FALSE, row.names = FALSE)
