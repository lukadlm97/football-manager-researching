dataset <- read.csv("data/dataset.csv")

str(dataset)

srb_players <- dataset[dataset$NationID==802,]




install.packages('psych')
library(psych)



attacking_players <- srb_players[srb_players$AttackingMidCentral==20|srb_players$AttackingMidRight==20|srb_players$AttackingMidLeft==20,]
summary(attacking_players)
attacking_players <- subset()


nrow(attacking_players)
nrow(attacking_players[attacking_players$Age<20,])


young_players <- attacking_players[attacking_players$Age<18,]

junior_players <- attacking_players[attacking_players$Age<21,]
junior_players <- junior_players[junior_players$Age>17,]

young_senior_players <- attacking_players[attacking_players$Age<24,]
young_senior_players <- young_senior_players[young_senior_players$Age>20,]

#Age discovering
summary(young_players$Age)
summary(junior_players$Age)
summary(young_senior_players$Age)


describe(young_players$Age)
describe(junior_players$Age)
describe(young_senior_players$Age)

nrow(young_players)
nrow(junior_players)
nrow(young_senior_players)

shapiro.test(young_players$Age)
shapiro.test(junior_players$Age)
shapiro.test(young_senior_players$Age)

shapiro.test(attacking_players$Height)
