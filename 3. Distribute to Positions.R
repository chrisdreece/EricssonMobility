
keep(netMoves,netProms,qaTransfers,qaPromotions,lateralMovesRollup,promotionsRollup,positionMapping,sqlConnString,sure=TRUE)

## Distribute Moves ----
  ## NOTE: this are monthly counts by Position
pos2Mapping <- positionMapping %>%
  group_by(Position2) %>%
  mutate(pos2Headcount=length(EmployeeID)) %>%
  ungroup() %>%
  group_by(AssignedPositionKey,Position2,pos2Headcount) %>%
  summarise(headcount=length(EmployeeID)) %>%
  ungroup() %>%
  mutate(prop=round(headcount/pos2Headcount,10)) %>%
  arrange(Position2,-prop) %>%
  select(Position2,AssignedPositionKey,prop)

AssignedPositionKeyMoves <- netMoves %>%
  left_join(.,pos2Mapping,by=c('Position2')) %>%
  mutate(LateralIn=gains*prop,
         LateralOut=losses*prop) %>%
  select(AssignedPositionKey,LateralIn,LateralOut)



## Distribute Promotions ----
## NOTE: this are monthly counts by Position
pos2pos3Mapping <- positionMapping %>%
  mutate(level=case_when(
    Position3=='E' ~ 10,
    TRUE ~ as.numeric(substr(Position3,2,2))
  )) %>%
  group_by(Position2,level) %>%
  mutate(pos2pos3Headcount=length(EmployeeID)) %>%
  ungroup() %>%
  group_by(AssignedPositionKey,Position2,level,pos2pos3Headcount) %>%
  summarise(headcount=length(EmployeeID)) %>%
  ungroup() %>%
  mutate(prop=round(headcount/pos2pos3Headcount,10)) %>%
  arrange(Position2,level,-prop) %>%
  select(Position2,level,AssignedPositionKey,prop)


### remove Position/Level combos from netProms that do not appear in the PositionMapping
  ### this occurs because the promotions model considers Position2='Electrical Engineer' and Level = 7 as eligible for promotion
  ### to level 8 (for example), and so sends some promotional headcount to this combo, BUT it's possible there are no level 8's in the active headcount, so no position to distribute to.
  ### this should be a small number, check that here:

existingCombos <- pos2pos3Mapping %>%
  mutate(Position2_level=paste(Position2,level,sep='-')) %>%
  select(Position2_level) %>%
  unique(.)

netPromsFilteredOut<-netProms %>%
  filter(!(paste(Position2,level,sep='-') %in% existingCombos$Position2_level))

netProms<-netProms %>%
  filter(paste(Position2,level,sep='-') %in% existingCombos$Position2_level)

AssignedPositionKeyProms <- netProms %>%
  left_join(.,pos2pos3Mapping,by=c('Position2','level')) %>%
  mutate(PromotionIn=gains*prop,
         PromotionOut=losses*prop) %>%
  select(AssignedPositionKey,PromotionIn,PromotionOut)


Transfers<-bind_rows(AssignedPositionKeyMoves,AssignedPositionKeyProms)
Transfers[is.na(Transfers)] <- 0

Transfers <- Transfers %>%
  group_by(AssignedPositionKey) %>%
  summarise(LateralIn=sum(LateralIn),
            LateralOut=sum(LateralOut),
            PromotionIn=sum(PromotionIn),
            PromotionOut=sum(PromotionOut))
  

sum(netMoves$gains)
sum(netMoves$losses)

sum(netProms$gains)
sum(netPromsFilteredOut$gains)
sum(netProms$losses)

sum(Transfers$LateralIn)
sum(Transfers$LateralOut)

sum(Transfers$PromotionIn)
sum(Transfers$PromotionOut)


sqlSave(sqlConnString,Transfers,tablename='Transfers', rownames = F)
