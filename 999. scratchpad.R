
###

zzz <- QBSnapshotsFull %>%
  filter(Position1=='Multi Functional Finance' & level==3)

table(zzz$IntervalDate)


zzz <- QBSnapshotsFull %>%
  filter(Position1=='Electrical Engineering') %>%
  arrange(IntervalDate)

zzz <- activeHeadcount %>%
  filter(Position1=='Electrical Engineering')

zzz <- activeHeadcountPooledModel %>%
  filter(Position1=='Electrical Engineering')

zzz <- positionKeyMoves %>%
  filter(is.na(PositionKey))

