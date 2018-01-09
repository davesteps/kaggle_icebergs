
library(jsonlite)
library(ggplot2)
library(raster)
library(dplyr)
library(EBImage)

train <- read_json("data/train.json", simplifyVector=TRUE)

str(train)

band_1 <- train$band_1
train$band_1 <- NULL
band_2 <- train$band_2
train$band_2 <- NULL

train$inc_angle <- as.numeric(train$inc_angle)

str(train)
summary(train$inc_angle)

ggplot(train)+geom_density(aes(x=inc_angle,fill=factor(is_iceberg)),alpha=0.5)


procImage <- function(i){
  # i <- 60
  print(i)
  r1 <- raster(matrix(band_1[[i]],nrow = 75,ncol = 75))
  r1mf <- focal(r1,w=matrix(1,3,3), fun=median,pad=T, padValue= mean(band_1[[i]]))
  r2 <- raster(matrix(band_2[[i]],nrow = 75,ncol = 75))
  r2mf <- focal(r2,w=matrix(1,3,3), fun=median,pad=T, padValue= mean(band_2[[i]]))
  stk <- stack(r1mf,r2mf)
  
  df <- rasterToPoints(stk) %>% data.frame()
  hc <- hclust(dist(df))
  c <- cutree(hc, k = 2)
  # ggplot(df)+geom_point(aes(x=layer.1,y=layer.2,col=c))
  r3 <- raster(t(matrix(c,nrow = 75,ncol = 75)))
  png(paste0('output/clump',i,'.png'),width = 800,height =800)
  par(mfrow=c(2,2))
  plot(r1)
  plot(r2)
  plot(r3)
  q99 <- df %>%
    filter(layer.1>quantile(layer.1,0.99)) %>%
    select(x,y)
  clmp <- clump(r3-1)
  # max(clmp[],na.rm = T)
  # points(q99)
  extObj <- modal(extract(clmp,q99),na.rm=T)
  clmp[clmp!=extObj] <- NA
  
  clmp <- focal(clmp,w=matrix(1,7,7),max,na.rm=T)
  plot(clmp)
  # mask(r1,clmp) %>% plot
  
  dev.off()

  ft1 = computeFeatures(as.matrix(!is.na(clmp)), as.matrix(r1),xname = 'band1')
  ft2 = computeFeatures(as.matrix(!is.na(clmp)), as.matrix(r2),xname = 'band2')
  cbind.data.frame(train[i,c(1,3,2)],ft1,ft2)
  
}

# plyr::ldply(10:20, procImage)
featDf <- plyr::ldply(1:nrow(train), procImage)
# saveRDS(featDf,file='featureDf.rdata')

featDf <- readRDS('featureDf.rdata')

library(randomForest)

trainX <- featDf %>% 
  select(-id,-is_iceberg) %>%
  na.roughfix()
trainY <- featDf %>%
  select(is_iceberg) %>%
  .$is_iceberg %>%
  as.factor()
nrow(trainX)
nrow(trainY)

fit <- randomForest(x=trainX,y=trainY,ntree=5e3)

actual <- as.integer(as.character(fit$y))
pred <- fit$votes[,2]
Metrics::logLoss(actual,pred)


fit
plot(fit)
vi <- varImpPlot(fit)
topVars <- vi %>%
  data.frame() %>%
  mutate(var = row.names(.)) %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice(1:10)


trainX %>%
  select_(.dots = topVars$var) ->
  trainXsub

fit <- randomForest(x=trainXsub,y=trainY,ntree=5e3)
fit
actual <- as.integer(as.character(fit$y))
pred <- fit$votes[,2]
Metrics::logLoss(actual,pred)



# don't use median filtered
# grow clump
# make sure it is in center
# stats from wider image
# scale image
# gray levels used for features
# supervised model for extracting object






