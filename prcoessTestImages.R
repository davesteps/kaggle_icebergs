
# assume packages and functions are sourced from 180109_haralick

test <- read_json("data/test.json", simplifyVector=TRUE)

saveRDS(test,'data/test.feather')
train <- readRDS('data/test.feather')

str(train)

band_1 <- train$band_1
train$band_1 <- NULL
band_2 <- train$band_2
train$band_2 <- NULL

train$inc_angle <- as.numeric(train$inc_angle)


testFeatDf <- plyr::ldply(1:nrow(train), procImage,F)
str(testFeatDf)
saveRDS(testFeatDf,file='test_featureDf.rdata')


fit
Xtest <- select(testFeatDf,-id)

pred <- predict(fit,Xtest,type='prob')
str(pred)
head(pred)
select(testFeatDf,id) %>%
  bind_cols(data.frame(pred)) %>%
  rename(is_iceberg = X1) %>% 
  select(-X0) %>%
  write.csv(.,'pred1.csv',row.names = F)





