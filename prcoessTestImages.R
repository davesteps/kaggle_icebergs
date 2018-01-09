
# assume packages and functions are sourced from 180109_haralick

test <- read_json("data/test.json", simplifyVector=TRUE)


band_1 <- train$band_1
train$band_1 <- NULL
band_2 <- train$band_2
train$band_2 <- NULL

train$inc_angle <- as.numeric(train$inc_angle)

testFeatDf <- plyr::ldply(1:nrow(test), procImage)
saveRDS(testFeatDf,file='test_featureDf.rdata')