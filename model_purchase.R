library(readr)
library(dplyr)
library(caret)


df_purchase <- read_csv("~/Documentos/workspace/chaordic-challenge/df_purchase.csv")

df_purchase <- df_purchase[complete.cases(df_purchase$gender), ] 

glimpse(df_purchase)
summary(df_purchase)

df_purchase <- droplevels(df_purchase)

trainIndex <- createDataPartition(df_purchase$gender, p = .75, list = FALSE, times = 1)

dfTrain <- df_purchase[trainIndex,]
dfTest <- df_purchase[-trainIndex,]

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)


table(df_purchase$gender)


ptm <- proc.time()

treeFit <- train(gender ~ ., data = select(dfTrain, -id),
                 method = "C5.0",
                 trControl = fitControl,
                 tuneGrid=expand.grid(model = "tree", winnow = FALSE, trials = c(1:10)))

rand_forest_time <- proc.time() - ptm

treeFit


treePredict <- predict(treeFit,newdata = dfTest)
confusionMatrix(treePredict, dfTest$gender)




