source('libraries.R')
load('originais.RData')

# data <- 
#   originais %>% 
#   filter(data > 2009)

data <- originais %>% 
  select(top100, time_signature, explicit, mode, danceability, energy, loudness, speechiness,
         acousticness, instrumentalness, liveness, valence,
         tempo, popularity, followers, minutos)

data$explicit <- as.numeric(paste(data$explicit))

data <- data %>% drop_na()

df <- data %>% 
  select(-vezes) %>% 
  drop_na()

df

onehot_obj <- onehot(data[,-1])

onehot_data <- data.frame(predict(onehot_obj, data[,-1]), top100 = data$top100)

onehot_data <-  onehot_data %>% 
  drop_na()

index <- createDataPartition(onehot_data$top100, p = 0.75, list = FALSE)

train <- onehot_data[index,]
test <- onehot_data[-index,]

fitControl <- trainControl(method = "cv",
                           number = 5, 
                           savePred = TRUE, 
                           classProb = TRUE)

knn.config <- caretModelSpec(method="knn", 
                             tuneGrid=expand.grid(k=seq(3, 41, 2)))

svmL.config <- caretModelSpec(method="svmLinear",
                              tuneGrid=expand.grid(C=seq(1, 10, 1)))

rf.config <- caretModelSpec(method = 'rf',
                            tunegrid = expand.grid(mtry = 1:(ncol(onehot_data)-1)))


modelos <- caretList(
    top100 ~ .,
    data=onehot_data,
    trControl=fitControl,
    metric="ROC",
    tuneList=list(
      knn       = knn.config,
      svmLinear = svmL.config,
      rf = rf.config
    )
)

model <- train(top100 ~ ., 
               data = train, 
               method = "rf", 
               tuneGrid = tune.grid,
               trControl = fitControl)

model
confusionMatrix(model)

  