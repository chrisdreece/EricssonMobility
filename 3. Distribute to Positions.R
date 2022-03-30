
keep(netMoves,netProms,qaTransfers,qaPromotions,lateralMovesRollup,promotionsRollup,positionMapping,sqlConnString,sure=TRUE)

## Distribute Moves ----
  ## NOTE: this are monthly counts by Position
pos1Mapping <- positionMapping %>%
  group_by(Position1) %>%
  mutate(pos1Headcount=length(EmployeeID)) %>%
  ungroup() %>%
  group_by(PositionKey,Position1,pos1Headcount) %>%
  summarise(headcount=length(EmployeeID)) %>%
  ungroup() %>%
  mutate(prop=round(headcount/pos1Headcount,10)) %>%
  arrange(Position1,-prop) %>%
  select(Position1,PositionKey,prop)

positionKeyMoves <- netMoves %>%
  left_join(.,pos1Mapping,by=c('Position1')) %>%
  mutate(LateralIn=gains*prop,
         LateralOut=losses*prop) %>%
  select(PositionKey,LateralIn,LateralOut)



## Distribute Promotions ----
## NOTE: this are monthly counts by Position
pos1Pos2Mapping <- positionMapping %>%
  mutate(level=case_when(
    Position2 %in% c('Co-Op/Intern','Non-Exempt','Represented') ~ 0,
    Position2=='Not Specified' ~ 999,
    TRUE ~ as.numeric(substr(Position2,7,7))
  )) %>%
  group_by(Position1,level) %>%
  mutate(pos1Pos2Headcount=length(EmployeeID)) %>%
  ungroup() %>%
  group_by(PositionKey,Position1,level,pos1Pos2Headcount) %>%
  summarise(headcount=length(EmployeeID)) %>%
  ungroup() %>%
  mutate(prop=round(headcount/pos1Pos2Headcount,10)) %>%
  arrange(Position1,level,-prop) %>%
  select(Position1,level,PositionKey,prop)


### remove Position/Level combos from netProms that do not appear in the PositionMapping
  ### this occurs because the promotions model considers Position1='Electrical Engineer' and Level = 7 as eligible for promotion
  ### to level 8 (for example), and so sends some promotional headcount to this combo, BUT it's possible there are no level 8's in the active headcount, so no position to distribute to.
  ### this should be a small number, check that here:

existingCombos <- pos1Pos2Mapping %>%
  mutate(Position1_level=paste(Position1,level,sep='-')) %>%
  select(Position1_level) %>%
  unique(.)

netPromsFilteredOut<-netProms %>%
  filter(!(paste(Position1,level,sep='-') %in% existingCombos$Position1_level))

netProms<-netProms %>%
  filter(paste(Position1,level,sep='-') %in% existingCombos$Position1_level)

positionKeyProms <- netProms %>%
  left_join(.,pos1Pos2Mapping,by=c('Position1','level')) %>%
  mutate(PromotionIn=gains*prop,
         PromotionOut=losses*prop) %>%
  select(PositionKey,PromotionIn,PromotionOut)


Transfers<-bind_rows(positionKeyMoves,positionKeyProms)
Transfers[is.na(Transfers)] <- 0

Transfers <- Transfers %>%
  group_by(PositionKey) %>%
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
