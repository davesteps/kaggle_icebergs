
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

edgeCells <- raster(matrix(band_2[[1]],nrow = 75,ncol = 75)) %>%
  focal(.,w=matrix(1,3,3), fun=median) %>%
  is.na() %>% 
  rasterToPoints() %>%
  data.frame() %>%
  filter(layer==1) %>%
  select(x,y)


procImage <- function(i){
  # i <- 14
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

  clmp <- clump(r3-1)
  clmpDf <- rasterToPoints(clmp) %>% data.frame()
  
  # if there is more than one clump we must choose one
  if(length(unique(clmpDf$clumps))>1){
    
    
    
    edgeClumps <- extract(clmp,edgeCells) %>%
      .[!is.na(.)] %>%
      unique()
    
    # choose the clump closest to center
    clumpInfo <- clmpDf %>%
      mutate(b1 = extract(r1mf,.[,1:2])) %>%
      group_by(clumps) %>%
      summarise(xmean=mean(x),
                ymean=mean(y),
                b1=mean(b1),
                n= n()) %>%
      mutate(centerDist = sqrt(((0.5-xmean)^2)+((0.5-ymean)^2)),
             hitsEdge = clumps %in% edgeClumps,
             distRank = min_rank(centerDist),
             sizeRank = min_rank(desc(n)),
             valuRank = min_rank(desc(b1)),
             meanRank = (distRank+sizeRank+valuRank)/3)
    
    nonEdgeClumps <- filter(clumpInfo,!hitsEdge)
    
    if(nrow(nonEdgeClumps)) clumpInfo <- nonEdgeClumps

    # # choose the clump with the most cells in the top percentile of band 1
    # q99 <- df %>%
    #   filter(layer.1>quantile(layer.1,0.99)) %>%
    #   select(x,y)
    # # max(clmp[],na.rm = T)
    # points(q99)
    # extObj <- modal(extract(clmp,q99),na.rm=T)
    extObj <- filter(clumpInfo,meanRank==min(meanRank))$clumps[1]

  } else {
    extObj <- 1
    }

  # slected clump
  clmp[clmp!=extObj] <- NA
  # grow clump
  clmp <- focal(clmp,w=matrix(1,5,5),max,na.rm=T)
  
  png(paste0('output/clump',i,'.png'),width = 800,height =800)
  par(mfrow=c(2,2))
  plot(r1)
  plot(r2)
  plot(r3)
  plot(clmp)
  # mask(r1,clmp) %>% plot
  dev.off()

  ft1 = computeFeatures(as.matrix(!is.na(clmp)), as.matrix(r1mf),xname = 'band1')
  ft2 = computeFeatures(as.matrix(!is.na(clmp)), as.matrix(r2mf),xname = 'band2')
  cbind.data.frame(train[i,c(1,3,2)],ft1,ft2)
  
}

# plyr::ldply(10:20, procImage)
featDf <- plyr::ldply(1:nrow(train), procImage)
saveRDS(featDf,file='featureDf.rdata')

# featDf <- readRDS('featureDf.rdata')

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
fit
actual <- as.integer(as.character(fit$y))
pred <- fit$votes[,2]
Metrics::logLoss(actual,pred)

fit
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


train[fit$predicted!=fit$y,]

# poor clustering eg 259 268 334 398 431 534
# don't use median filtered
# inspect which ones it is getting missclassifying
# grow clump
# make sure it is in center
# stats from wider image
# scale image
# gray levels used for features
# compute features at different distances
# supervised model for extracting object

corLocal
MoranLocal




