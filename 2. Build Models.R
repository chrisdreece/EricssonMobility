
## lateral model ----

lateralMovesRollup<-QBSnapshots %>%
  filter(lateral==1) %>%
  filter(positionBegin!='Not Specified' & positionEnd!='Not Specified') %>%
  ### THIS FILTER IS VERY IMPORTANT -- IT GETS RID OF NOISEY MOVEMENT INTO- AND OUT OF POSITIONS THAT REALLY MESSES THINGS UP
  filter(positionBegin!=positionEnd) %>%
  ### REMOVE TECHNICIAN PROGRAM -- EXPLAIN THIS TO PETER
  filter(positionBegin!='Technician Program') %>%
  group_by(latType) %>%
  summarise(freq=length(EmployeeID)) %>%
  arrange(-freq) %>%
  filter(freq>=10) %>%
  ungroup()

moves<-data.frame()


Sys.time()

for(i in 1:nrow(lateralMovesRollup)) {
  
  #i<-1
  
  moveString<-lateralMovesRollup$latType[i] 
  fromPosition <- sub("_TO_.*", "", moveString)      
  toPosition <- sub(".*_TO_", "", moveString)
  
  modelData <- QBSnapshots %>%
    filter(positionBegin==fromPosition) %>%
    mutate(lateral_i=ifelse(positionEnd==toPosition,1,0)) 
    #%>%
    #filter(EmployeeID!=371237)
  
  lmModel <- gam(lateral_i ~ s(Tenure), family=binomial, data=modelData)

  data.frame(Tenure=0:20) %>%
      mutate(risk=predict(lmModel,newdata=.,type="response")) %>%
      ggplot(aes(Tenure,risk)) + geom_line()
  
  activeHeadcount_i <- activeHeadcount %>%
    filter(Position1==fromPosition) %>%
    mutate(risk=predict(lmModel,newdata=.,type="response"))
  
  moves_i <- data.frame(
    from=fromPosition,
    to=toPosition,
    moves=round(sum(activeHeadcount_i$risk)/6,3)
    )
  
  moves<-rbind(moves,moves_i)
  
}

Sys.time()

moves<-filter(moves,moves!=0)

  ### check against history
qaTransfers <- moves %>%
  mutate(latType=paste(from,to,sep='_TO_')) %>%
  select(latType,moves) %>%
  mutate(moves=24*moves) %>%
  left_join(.,lateralMovesRollup,by='latType')
  
  ### create net table
losses <- moves %>%
  rename('Position1'='from') %>% 
  select(-to) %>%
  rename('losses'='moves') %>%
  group_by(Position1) %>%
  summarise(losses=sum(losses)) %>%
  ungroup()

gains <- moves %>%
  rename('Position1'='to') %>%
  select(-from) %>%
  rename('gains'='moves') %>%
  group_by(Position1) %>%
  summarise(gains=sum(gains)) %>%
  ungroup()

netMoves <-  gains %>%
  merge(.,losses,by="Position1",all.x=TRUE,all.y=TRUE) %>%
  mutate(gains=replace_na(gains,0),
         losses=replace_na(losses,0)) %>%
  mutate(net=gains-losses)


## promotions model ----

promotionsRollup<-QBSnapshots %>%
  filter(promotion==1 & levelBegin!=999 & levelEnd!=999) %>%
  group_by(Position1,promType) %>%
  summarise(freq=length(EmployeeID)) %>%
  arrange(-freq) %>%
  #filter(freq>=5) %>%
  ungroup()

promotionsLarge <- promotionsRollup %>%
  filter(freq>=15) %>%
  mutate(promType2=paste(Position1,substr(promType,1,1),sep='_'))


proms<-data.frame()

### Frequent Promotions (each gets its own GAM model)

pdf('D:/Development/Diagnostics/LMCO Mobility/promotionGams.pdf')

for(i in 1:nrow(promotionsLarge)) {
  
  pos1String <- as.character(promotionsLarge$Position1[i])
  moveString<-promotionsLarge$promType[i] 
  fromLevel <- as.numeric(sub("_TO_.*", "", moveString))      
  toLevel <- as.numeric(sub(".*_TO_", "", moveString))
  
  modelData <- QBSnapshots %>%
    filter(Position1==pos1String & levelBegin==fromLevel) %>%
    mutate(prom_i=ifelse(levelEnd==toLevel,1,0))
  
  lmModel <- gam(prom_i ~ s(Tenure), family=binomial,sp=.2, data=modelData)
  
  print(data.frame(Tenure=0:20) %>%
          mutate(risk=predict(lmModel,newdata=.,type="response")) %>%
          ggplot(aes(Tenure,risk)) + geom_line() +
    ggtitle(paste(pos1String,moveString,sep='')))
  
  activeHeadcount_i <- activeHeadcount %>%
    filter(Position1==pos1String & level==fromLevel) %>%
    mutate(risk=predict(lmModel,newdata=.,type="response"))
  
  proms_i <- data.frame(
    Position1=pos1String,
    from=fromLevel,
    to=toLevel,
    moves=round(sum(activeHeadcount_i$risk)/6,3)
  )
  
  proms<-rbind(proms,proms_i)
  
}

dev.off()

proms<-filter(proms,moves!=0)


### Less Frequent Promotions (pooled model)
pooledModelData <- QBSnapshots %>%
  mutate(promType2=paste(Position1,level,sep='_')) %>%
  filter(!(promType2 %in% promotionsLarge$promType2))

activeHeadcountPooledModel <- activeHeadcount %>%
  mutate(promType2=paste(Position1,level,sep='_')) %>%
  filter(!(promType2 %in% promotionsLarge$promType2))

promsPooled<-data.frame()

pdf('D:/Development/Diagnostics/LMCO Mobility/promotionsPooledModel.pdf')

for(i in 1:8){

  modelData<-pooledModelData %>%
    filter(level==i)
  
  lmModel <- gam(promotion ~ s(Tenure), family=binomial,sp=.2, data=modelData)
  
  print(data.frame(Tenure=0:20) %>%
          mutate(risk=predict(lmModel,newdata=.,type="response")) %>%
          ggplot(aes(Tenure,risk)) + geom_line() +
          ggtitle(paste('Level',i,sep=' ')))
  
  activeHeadcount_i <- activeHeadcountPooledModel %>%
    filter(level==i) %>%
    mutate(risk=as.numeric(predict(lmModel,newdata=.,type="response"))) %>%
    group_by(Position1,Position2) %>%
    summarise(moves=sum(risk)) %>%
    ungroup()
  
  promsPooled<-rbind(promsPooled,activeHeadcount_i)
  
}

dev.off()

promsPooled <- promsPooled %>%
  mutate(from=as.numeric(substr(Position2,7,7))) %>%
  mutate(to=from+1) %>%
  select(Position1,from,to,moves)

proms<-rbind(proms,promsPooled)


##### LEFT OFF HERE -- PLAN IS TO BIN ALL THE PROMOTIONS WITH SMALL SAMPLE SIZE (<10) INTO ONE LARGE
  ### POPULATION AND HAVE AN ALL-PURPOSE PROMOTION MODEL WITH TWO COVARIATES: LEVEL AND TENURE

### check against history
qaPromotions <- proms %>%
  mutate(promType=paste(from,to,sep='_TO_')) %>%
  select(Position1,promType,moves) %>%
  mutate(moves=24*moves) %>%
  left_join(.,promotionsRollup,by=c('Position1','promType'))

losses <- proms %>%
  rename('level'='from') %>% 
  select(-to) %>%
  rename('losses'='moves') %>%
  group_by(Position1,level) %>%
  summarise(losses=sum(losses)) %>%
  ungroup()

gains <- proms %>%
  rename('level'='to') %>% 
  select(-from) %>%
  rename('gains'='moves') %>%
  group_by(Position1,level) %>%
  summarise(gains=sum(gains)) %>%
  ungroup()

netProms <-  gains %>%
  merge(.,losses,by=c("Position1","level"),all.x=TRUE,all.y=TRUE) %>%
  mutate(gains=replace_na(gains,0),
         losses=replace_na(losses,0)) %>%
  mutate(net=gains-losses)