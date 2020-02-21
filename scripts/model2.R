source('libraries.R')
load('originais.RData')

# data <- 
#   originais %>% 
#   filter(data > 2009)

data <- originais %>% 
  filter(data > 2000) %>% 
  mutate(top100 = ifelse(top100 == 'sim', 1, 0)) %>% 
  group_by(album_name) %>% 
  filter(sum(top100) > 0) %>% 
  ungroup() %>% 
  select(top100, time_signature, explicit, mode, danceability, energy, loudness, speechiness,
         acousticness, instrumentalness, liveness, valence,
         tempo, popularity, followers, minutos)

data$explicit <- as.numeric(paste(data$explicit))

data <- data %>% drop_na()

data <- data %>% 
  mutate(top100 = ifelse(top100 == 1, 'sim', 'nao'))

# df <- data %>% 
#   select(-vezes) %>% 
#   drop_na()

set.seed(100498)
index <- createDataPartition(data$top100, p = 0.75, list = FALSE)

train <- data[index,]
test <- data[-index,]

fitControl <- trainControl(method = "cv",
                           number = 3, 
                           savePred = TRUE, 
                           classProb = TRUE)

knn.config <- caretModelSpec(method="knn", 
                             tuneGrid=expand.grid(k=seq(3, 41, 2)))

svmL.config <- caretModelSpec(method="svmLinear",
                              tuneGrid=expand.grid(C=seq(1, 10, 1)))

rf.config <- caretModelSpec(method = 'rf',
                            tunegrid = expand.grid(mtry = 1:(ncol(data)-1)))


modelos <- caretList(
  top100 ~ .,
  data=train,
  trControl=fitControl,
  metric="kappa",
  tuneList=list(
    knn       = knn.config,
    svmLinear = svmL.config,
    rf = rf.config
  )
)

modelos

modelCor(resamples(modelos))

combinacao <- caretEnsemble(
  modelos,
  trControl = fitControl)

save(modelos, file = 'modelos.RData')
save(combinacao, file = 'combinacao.RData')

combinacao
