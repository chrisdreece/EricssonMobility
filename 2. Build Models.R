
## lateral model ----

lateralMovesRollup<-QBSnapshots %>%
  filter(lateral==1) %>%
  filter(positionBegin!='Not Specified' & positionEnd!='Not Specified') %>%
  ### THIS FILTER IS VERY IMPORTANT -- IT GETS RID OF NOISEY MOVEMENT INTO- AND OUT OF POSITIONS THAT REALLY MESSES THINGS UP
  filter(positionBegin!=positionEnd) %>%
  group_by(latType) %>%
  summarise(freq=length(EmployeeID)) %>%
  arrange(-freq) %>%
  filter(freq>=10) %>%
  ungroup()

moves<-data.frame()

for(i in 1:nrow(lateralMovesRollup)) {

  moveString<-lateralMovesRollup$latType[i] 
  fromPosition <- sub("_TO_.*", "", moveString)      
  toPosition <- sub(".*_TO_", "", moveString)
  
  modelData <- QBSnapshots %>%
    filter(positionBegin==fromPosition) %>%
    mutate(lateral_i=ifelse(positionEnd==toPosition,1,0)) 
  
  lmModel <- gam(lateral_i ~ s(Tenure), family=binomial, data=modelData)

  data.frame(Tenure=0:20) %>%
      mutate(risk=predict(lmModel,newdata=.,type="response")) %>%
      ggplot(aes(Tenure,risk)) + geom_line()
  
  activeHeadcount_i <- activeHeadcount %>%
    filter(Position2==fromPosition) %>%
    mutate(risk=predict(lmModel,newdata=.,type="response"))
  
  moves_i <- data.frame(
    from=fromPosition,
    to=toPosition,
    moves=round(sum(activeHeadcount_i$risk)/6,3)
    )
  
  moves<-rbind(moves,moves_i)
  
}

moves<-filter(moves,moves!=0)

  ### check against history
qaTransfers <- moves %>%
  mutate(latType=paste(from,to,sep='_TO_')) %>%
  select(latType,moves) %>%
  mutate(moves=36*moves) %>%
  left_join(.,lateralMovesRollup,by='latType')
  
  ### create net table
losses <- moves %>%
  rename('Position2'='from') %>% 
  select(-to) %>%
  rename('losses'='moves') %>%
  group_by(Position2) %>%
  summarise(losses=sum(losses)) %>%
  ungroup()

gains <- moves %>%
  rename('Position2'='to') %>%
  select(-from) %>%
  rename('gains'='moves') %>%
  group_by(Position2) %>%
  summarise(gains=sum(gains)) %>%
  ungroup()

netMoves <-  gains %>%
  merge(.,losses,by="Position2",all.x=TRUE,all.y=TRUE) %>%
  mutate(gains=replace_na(gains,0),
         losses=replace_na(losses,0)) %>%
  mutate(net=gains-losses)


## promotions model ----

promotionsRollup<-QBSnapshots %>%
  filter(promotion==1 & levelBegin!=999 & levelEnd!=999) %>%
  ### filter out executives and one level below (since we are not modeling promotions to Executive level)
  filter(levelBegin<=8) %>%
  ### THIS FILTER IS VERY IMPORTANT -- IT GETS RID OF NOISEY MOVEMENT INTO- AND OUT OF POSITIONS THAT REALLY MESSES THINGS UP
  filter(levelBegin!=levelEnd) %>%
  group_by(Position2,promType) %>%
  summarise(freq=length(EmployeeID)) %>%
  arrange(-freq) %>%
  #filter(freq>=5) %>%
  ungroup()

promotionsLarge <- promotionsRollup %>%
  filter(freq>=10) %>%
  mutate(promType2=paste(Position2,substr(promType,1,1),sep='_'))

promotionsSmall <- promotionsRollup %>%
  filter(freq<10) %>%
  mutate(promType2=paste(Position2,substr(promType,1,1),sep='_'))

proms<-data.frame()

### Frequent Promotions (each gets its own GAM model)

pdf('D:/Development/predict/Diagnostics/Ericsson Mobility/promotionGams.pdf')

for(i in 1:nrow(promotionsLarge)) {
  
  pos1String <- as.character(promotionsLarge$Position2[i])
  moveString<-promotionsLarge$promType[i] 
  fromLevel <- as.numeric(sub("_TO_.*", "", moveString))      
  toLevel <- as.numeric(sub(".*_TO_", "", moveString))
  
  modelData <- QBSnapshots %>%
    filter(Position2==pos1String & levelBegin==fromLevel) %>%
    mutate(prom_i=ifelse(levelEnd==toLevel & promotion==1,1,0))
  
  lmModel <- gam(prom_i ~ s(Tenure), family=binomial,sp=.2, data=modelData)
  
  print(data.frame(Tenure=0:20) %>%
          mutate(risk=predict(lmModel,newdata=.,type="response")) %>%
          ggplot(aes(Tenure,risk)) + geom_line() +
    ggtitle(paste(pos1String,moveString,sep='')))
  
  activeHeadcount_i <- activeHeadcount %>%
    filter(Position2==pos1String & level==fromLevel) %>%
    mutate(risk=predict(lmModel,newdata=.,type="response"))
  
  proms_i <- data.frame(
    Position2=pos1String,
    from=fromLevel,
    to=toLevel,
    moves=round(sum(activeHeadcount_i$risk)/6,3)
  )
  
  proms<-rbind(proms,proms_i)
  
}

dev.off()

proms<-filter(proms,moves!=0)




promsSmall<-data.frame()

### Frequent Promotions (each gets its own GAM model)

for(i in 1:nrow(promotionsSmall)) {
  
  pos1String <- as.character(promotionsSmall$Position2[i])
  moveString<-promotionsSmall$promType[i] 
  fromLevel <- as.numeric(sub("_TO_.*", "", moveString))      
  toLevel <- as.numeric(sub(".*_TO_", "", moveString))
  
  modelData <- QBSnapshots %>%
    filter(Position2==pos1String & levelBegin==fromLevel) %>%
    mutate(prom_i=ifelse(levelEnd==toLevel,1,0))
  
  promRate<-mean(modelData$prom_i)
  
  activeHeadcount_i <- activeHeadcount %>%
    filter(Position2==pos1String & level==fromLevel & promotion==1) %>%
    mutate(risk=promRate)
  
  proms_i <- data.frame(
    Position2=pos1String,
    from=fromLevel,
    to=toLevel,
    moves=round(sum(activeHeadcount_i$risk)/6,3)
  )
  
  promsSmall<-rbind(promsSmall,proms_i)
  
}


promsSmall<-filter(promsSmall,moves!=0)



proms<-rbind(proms,promsSmall)


##### LEFT OFF HERE -- PLAN IS TO BIN ALL THE PROMOTIONS WITH SMALL SAMPLE SIZE (<10) INTO ONE LARGE
  ### POPULATION AND HAVE AN ALL-PURPOSE PROMOTION MODEL WITH TWO COVARIATES: LEVEL AND TENURE

### check against history
qaPromotions <- proms %>%
  mutate(promType=paste(from,to,sep='_TO_')) %>%
  select(Position2,promType,moves) %>%
  mutate(moves=36*moves) %>%
  left_join(.,promotionsRollup,by=c('Position2','promType'))

losses <- proms %>%
  rename('level'='from') %>% 
  select(-to) %>%
  rename('losses'='moves') %>%
  group_by(Position2,level) %>%
  summarise(losses=sum(losses)) %>%
  ungroup()

gains <- proms %>%
  rename('level'='to') %>% 
  select(-from) %>%
  rename('gains'='moves') %>%
  group_by(Position2,level) %>%
  summarise(gains=sum(gains)) %>%
  ungroup()

netProms <-  gains %>%
  merge(.,losses,by=c("Position2","level"),all.x=TRUE,all.y=TRUE) %>%
  mutate(gains=replace_na(gains,0),
         losses=replace_na(losses,0)) %>%
  mutate(net=gains-losses)