# Load packages
library('digest')
library('splitstackshape')
library("ggplot2")
library("corrplot")

################ DiretÃ³rio dos arquivos ################

diretorioPadrao <- "D:/TestesR/Kaggle/"
arquivoTrain <- paste0(diretorioPadrao,"train.csv")
arquivoSongs <- paste0(diretorioPadrao,"songs.csv")
arquivoInfoExtraSongs <- paste0(diretorioPadrao,"song_extra_info.csv")
arquivoMembers <- paste0(diretorioPadrao,"members.csv")
arquivoTest <- paste0(diretorioPadrao,"test.csv")

################ Limpando as variÃ¡veis ################

train <- NULL
songs <- NULL
infoExtraSongs <- NULL
members <- NULL

################ ImportaÃ§Ã£o dos dados dos arquivos ################

train <- read.csv(arquivoTrain, sep = ",", na.strings = "")

songs <- read.csv(arquivoSongs, sep = ",", na.strings = "")

#artist_name_md5 <- NULL
#artist_name_md5 <- sapply(trainJoins$artist_name, digest, algo = 'md5')

#infoExtraSongs <- read.csv(arquivoInfoExtraSongs, sep = ",", na.strings = "")

members <- read.csv(arquivoMembers, sep = ",", na.strings = "")


################ SeparaÃ§Ã£o dos gÃªneros da mÃºsica ################

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

#trainJoins$source_system_tab <- na.omit(trainJoins$source_system_tab)
#trainJoins$source_screen_name <- na.omit(trainJoins$source_screen_name)
#trainJoins$source_type <- na.omit(trainJoins$source_type)
#trainJoins$song_length <- na.omit(trainJoins$song_length)
#trainJoins$genre_ids <- na.omit(trainJoins$genre_ids)
#trainJoins$artist_name <- na.omit(trainJoins$artist_name)
#trainJoins$language <- na.omit(trainJoins$language)
#trainJoins$city <- na.omit(trainJoins$city)
#trainJoins$bd <- na.omit(trainJoins$bd)
#trainJoins$gender <- na.omit(trainJoins$gender)
#trainJoins$registered_via <- na.omit(trainJoins$registered_via)

trainJoins$source_system_tab <- as.numeric(trainJoins$source_system_tab)
trainJoins$source_screen_name <- as.numeric(trainJoins$source_screen_name)
trainJoins$source_type <- as.numeric(trainJoins$source_type)
#trainJoins$artist_name <- as.numeric(as.factor(trainJoins$artist_name))
trainJoins$gender <- as.numeric(trainJoins$gender)

trainJoins$bd_factor <- NULL
trainJoins[trainJoins$bd <= 0, "bd_factor"] <- "NA"
trainJoins[trainJoins$bd > 0 & trainJoins$bd < 15, "bd_factor"] <- "Entre 0 e 14"
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
trainJoins$bd_factor_numeric <- as.numeric(trainJoins$bd_factor)
trainJoins$target <- as.factor(trainJoins$target)
summary(train)

summary(trainJoins$bd)
summary(trainJoins$bd_factor)
summary(trainJoins$bd_factor_numeric)

#write.csv(trainJoins, "D:/TestesR/Kaggle/Versao2/trainJoins.csv", quote = FALSE, row.names = FALSE, na = 'NA')

#trainJoins2 <- NULL
#trainJoins2 <- trainJoins[, c("target", "genre_ids", "artist_name", "language", "city", "bd", "gender")]

train <- NULL
songs <- NULL
#infoExtraSongs <- NULL
members <- NULL
songs_genre <- NULL


#    Análise dos Dados
#Omitir valores NA
trainJoins <- na.omit(trainJoins)
trainJoins2 <- trainJoins[, c("source_system_tab", "source_screen_name", "source_type", "song_length",
                              "genre_ids", "artist_name", "language", "city", "bd_factor_numeric", "gender",
                              "target")]

corrplot(cor(trainJoins2), method="circle", type="lower", order="hclust", addCoef.col = "black")



################ CRIAÃÃO DOS MODELOS NO H20 ################


library("h2o")
h2o.init()

dataTrain <- NULL
dataTrain <- as.h2o(trainJoins2)
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
gbm <- h2o.gbm(x = myX,
               y = myY,
               model_id = "gbm",
               training_frame    = dataTrain,
               nfolds            = 5,
               #ntrees            = 200,
               #max_depth         = 50,
               balance_classes = TRUE)


randomForest <- NULL
# Random Forest
randomForest <- h2o.randomForest(x = myX,
                                 y = myY,
                                 model_id = "randomForest",
                                 training_frame    = dataTrain,
                                 nfolds            = 5,
                                 ntrees            = 50,
                                 max_depth         = 20,
                                 
                                 balance_classes = TRUE)

h2o.auc(h2o.performance(gbm)) #0.671
h2o.confusionMatrix(gbm@model$training_metrics)
h2o.confusionMatrix(gbm@model$cross_validation_metrics)
gbm@model$cross_validation_metrics@metrics$AUC #0.671
gbm@model$cross_validation_metrics@metrics$r2 #0.089

dlearning.model <- NULL
dlearning.model <- h2o.deeplearning(
              x = myX,
              y = myY,
              model_id = "deep_learning",
              training_frame = dataTrain,
              nfolds = 5
              #epoch = 100,
              #hidden = c(200,200),
              #activation  = "Rectifier",
              #seed = 1234
)
#h2o.accuracy(h2o.performance(dlearning.model)) 
h2o.auc(h2o.performance(dlearning.model)) #0.655
h2o.confusionMatrix(dlearning.model@model$training_metrics)
h2o.confusionMatrix(dlearning.model@model$cross_validation_metrics)
dlearning.model@model$cross_validation_metrics@metrics$AUC #0.66
dlearning.model@model$cross_validation_metrics@metrics$r2 #0.085
################ REALIZAR PREDIÃÃO NO H20 ################

# SeparaÃ§Ã£o dos gÃªneros da mÃºsica 
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

testJoins$source_system_tab <- as.numeric(testJoins$source_system_tab)
testJoins$source_screen_name <- as.numeric(testJoins$source_screen_name)
testJoins$source_type <- as.numeric(testJoins$source_type)
testJoins$artist_name <- as.numeric(as.factor(testJoins$artist_name))
testJoins$gender <- as.numeric(testJoins$gender)

testJoins$bd_factor <- NULL
testJoins[testJoins$bd <= 0, "bd_factor"] <- "NA"
testJoins[testJoins$bd > 0 & testJoins$bd < 15, "bd_factor"] <- "Menor que 15"
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
testJoins$bd_factor_numeric <- as.numeric(testJoins$bd_factor)


testJoins2 <- testJoins[, c("id", "source_system_tab", "source_screen_name", "source_type", "song_length",
                            "genre_ids", "artist_name", "language", "city", "bd_factor_numeric", "gender")]

write.csv(testJoins, file = "D:/TestesR/Kaggle/Versao3/testJoins.csv", quote = FALSE, row.names = FALSE)

dataTest<-NULL
#predictTest <- h2o.importFile("D:/TestesR/Kaggle/testJoins3.csv")
dataTest <- as.h2o(testJoins2)

ResultadoPredicao <- NULL
ResultadoPredicao <- h2o.predict(dlearning.model, dataTest, type = "response")

testJoins <- NULL
testIds <- NULL
testJoins2 <- NULL


################ Tirar a duplicidade dos ids ################

h2o.exportFile(h2o.cbind(dataTest$id, ResultadoPredicao$predict), "D:/TestesR/Kaggle/Versao3/predicao2.csv")

arquivoResultado <- NULL
arquivoResultado <- read.csv("D:/TestesR/Kaggle/Versao3/predicao2.csv",sep = ",")

#Excluindo a Ãºltima linha duplicada

predicaoJoins <- NULL
predicaoJoins <- arquivoResultado[!duplicated(arquivoResultado$id, fromLast = FALSE),]
write.csv(predicaoJoins, file = "D:/TestesR/Kaggle/versao3/resultado2-1.csv", quote = FALSE, row.names = FALSE)

#Excluindo a primeira linha duplicada
predicaoJoins <- NULL
predicaoJoins <- arquivoResultado[!duplicated(arquivoResultado$id, fromLast = TRUE),]
write.csv(predicaoJoins, file = "D:/TestesR/Kaggle/versao3/resultado2-2.csv", quote = FALSE, row.names = FALSE)

arquivoResultado <- NULL
dataTest <- NULL
predicaoJoins <- NULL
ResultadoPredicao <- NULL
