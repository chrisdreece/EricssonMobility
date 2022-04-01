
library(dplyr)
library(tidyr)
library(lubridate)
library(RODBC)
library(mgcv)
library(ggplot2)
library(gdata)


######### DON'T FORGET -- THIS ALSO RUNS ON QBSNAPSHOTS, THE TABLE YOU UPDATE IN THE SQL PROCEDURE AND USE FOR THE TURNOVER PROCESS
## Import ----

sqlConnString <- odbcDriverConnect(paste("Driver=SQL Server Native Client 11.0; Server=", "172.25.16.138", ";Database=", "Ericsson_DW", ";Trusted_Connection=yes;", sep=''))
QBSnapshotsFull<-sqlQuery(sqlConnString, "select * from QBSnapshotsMobility", stringsAsFactors=FALSE)

  ## set aside position mapping
positionMapping<-QBSnapshotsFull %>%
  filter(IntervalDate==max(IntervalDate)) %>%
  select(EmployeeID,AssignedPositionKey,Position2,Position3)

table(QBSnapshotsFull$Position3)

QBSnapshotsFull <- QBSnapshotsFull %>%
  arrange(EmployeeID,IntervalDate) %>%
  select(EmployeeID,IntervalDate,Age,Tenure,Position2,Position3,Turnover) %>%
  mutate(level=case_when(
    Position3=='E' ~ 10,
    TRUE ~ as.numeric(substr(Position3,2,2))
  )) %>%
  group_by(EmployeeID) %>%
  mutate(lagLevel=lag(level),
         lagPosition2=lag(Position2),
         lagTenure=lag(Tenure)) %>%
  filter(!(is.na(lagPosition2))) %>%
  ungroup() %>%
  mutate(promotion=ifelse(level>lagLevel & level!=999 & lagLevel!=999 & Position2==lagPosition2,1,0)) %>%
  mutate(lateral=ifelse(lagPosition2!=Position2,1,0)) %>%
  mutate(latType=ifelse(lateral==1,
                        paste(lagPosition2,Position2,sep='_TO_'),
                        'N/A'))


### Special procedure for Ericsson, since it has missing tenure values
lmModel <- gam(Tenure ~ s(Age), family=gaussian, data=filter(QBSnapshotsFull, !(is.na(Tenure)) & !(is.na(Age))))

data.frame(Age=20:70) %>%
  mutate(Tenure=predict(lmModel,newdata=.,type="response")) %>%
  ggplot(aes(Age,Tenure)) + geom_line()


QBSnapshotsFull <- QBSnapshotsFull %>%
  mutate(Tenure = case_when(is.na(Tenure) & !(is.na(Age)) ~ predict(lmModel,newdata=.,type="response"),
                            is.na(Tenure) & (is.na(Age)) ~ mean(QBSnapshotsFull$Tenure,na.rm=TRUE),
                            TRUE ~ Tenure)) %>%
  filter(!(is.na(Tenure)))


activeHeadcount <- QBSnapshotsFull %>%
  filter(IntervalDate==max(IntervalDate) & Turnover==0) 

## Remove Outliers ----
  ### eliminate any lateral moves where >90% of the cases for that move type occur in a single month (assuming these are one-time events)
  ### I need the full timespan to do this properly -- this procedure with the data as is caused me to allow Multi-Functional-Finance moves as
  ### legit (the most common type), when that was clearly an outlier due to a huge one-time re-org

  ### you'll also want to compare software engineer and systems engineer movement in the final table
    ### they are responsible for a huge portion of the movement, but more of the software engineer movement
    ### got filtered out in the final table, it looks like, so systems engineer movement is huge and software engineer movement is small
    ### you'll need to look into that once you have more data

latsForElimination <- QBSnapshotsFull %>%
  filter(lateral==1) %>%
  group_by(latType) %>%
  mutate(totalFreq=length(latType)) %>%
  ungroup() %>%
  group_by(latType,totalFreq,IntervalDate) %>%
  summarise(freq=length(EmployeeID)) %>%
  mutate(pect=round(freq/totalFreq,2)) %>%
  arrange(-totalFreq,IntervalDate) %>%
  filter(pect>=.20) %>%
  ungroup() %>%
  select(latType,IntervalDate) %>%
  mutate(eliminationFlag=1)

QBSnapshotsFull <- QBSnapshotsFull %>%
  left_join(.,latsForElimination,by=c('latType','IntervalDate')) %>%
  mutate(eliminationFlag=replace_na(eliminationFlag,0)) %>%
  mutate(lateral=ifelse(eliminationFlag==1,0,lateral)) %>%
  select(-latType)

### filter down to an even three years of data
QBSnapshotsFull <- QBSnapshotsFull %>%
  filter(IntervalDate>=max(QBSnapshotsFull$IntervalDate)-months(35))

## Set up dataset with six-month periods ----
  ### using this alternative way to map in the monthRank column, because the dense_rank function took over two hours!

monthRankMapping <- QBSnapshotsFull %>%
  filter(IntervalDate>=max(QBSnapshotsFull$IntervalDate)-months(35)) %>%
  select(IntervalDate) %>%
  unique(.) %>%
  arrange(IntervalDate) %>%
  mutate(monthRank=row_number())


QBSnapshots <- QBSnapshotsFull %>% 
  left_join(.,monthRankMapping,by='IntervalDate') %>%
  #group_by(EmployeeID) %>%
  #mutate(dateRank = dense_rank(IntervalDate)) %>%
  ungroup()

# take an even three years of data, then divide the full timespan into six month chunks; for production you might take a shorter span
QBSnapshots <- QBSnapshots %>%
  mutate(period=ceiling(monthRank/6)) %>%
  group_by(period) %>%
  mutate(periodBegin=ifelse(monthRank==min(monthRank),1,0)) %>%
  ungroup() %>%
  group_by(EmployeeID,period) %>%
  mutate(promotion=max(promotion)) %>% mutate(lateral=max(lateral)) 

periodBegin <- QBSnapshots %>%
  group_by(EmployeeID,period) %>%
  filter(monthRank==min(monthRank)) %>%
  select(EmployeeID,period,lagPosition2,lagLevel) %>%
  rename('positionBegin'='lagPosition2',
         'levelBegin'='lagLevel') %>%
  ungroup()

periodEnd <- QBSnapshots %>%
  group_by(EmployeeID,period) %>%
  filter(monthRank==max(monthRank)) %>%
  select(EmployeeID,period,Position2,level) %>%
  rename('positionEnd'='Position2',
         'levelEnd'='level') %>%
  ungroup()

QBSnapshots <- QBSnapshots %>%
  left_join(.,periodBegin,by=c('EmployeeID','period')) %>%
  left_join(.,periodEnd,by=c('EmployeeID','period')) %>%  
  mutate(latType=ifelse(lateral==1,paste(positionBegin,positionEnd,sep='_TO_'),'N/A')) %>%
  mutate(promType=ifelse(promotion==1,paste(levelBegin,levelEnd,sep='_TO_'),'N/A')) %>%
  filter(periodBegin==1) %>%
  ungroup()
