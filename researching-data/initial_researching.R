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
attacking_young_players <- attacking_players[attacking_players$Age<24,]


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

#Passing discovering
shapiro.test(attacking_young_players$Passing)
boxplot.stats(attacking_young_players$Passing)$stats

down_rank <- quantile(attacking_young_players$Passing,0.25)
down_rank
top_rank <-quantile(attacking_young_players$Passing,0.75) 
top_rank

bad_pass <- attacking_young_players[attacking_young_players$Passing<down_rank,]
nrow(bad_pass)
good_pass <- attacking_young_players[attacking_young_players$Passing>down_rank&attacking_young_players$Passing<top_rank,]
nrow(good_pass)
greate_pass <- attacking_young_players[attacking_young_players$Passing>top_rank,]
nrow(greate_pass)
shapiro.test(bad_pass$Passing)
shapiro.test(good_pass$Passing)
shapiro.test(greate_pass$Passing)

describe(bad_pass$Passing)
describe(good_pass$Passing)
describe(greate_pass$Passing)

str(attacking_young_players)

attacking_young_players[163,]

?strcmp

attacking_young_players[attacking_young_players$Name %like% "Sergej Milinkovic Savic"]

attacking_young_players$FIFA_Over <- NULL
attacking_young_players$FIFA_Pot <- NULL
attacking_young_players$PES <- NULL
attacking_young_players$Transfer <- NA

#SMS
attacking_young_players$FIFA_Over[163]<- 85
attacking_young_players$FIFA_Pot[163]<- 90
attacking_young_players$PES[163]<- 90
attacking_young_players$Transfer[163]<- 64

attacking_young_players$Name
attacking_young_players[264,]

#Pantovic
attacking_young_players$FIFA_Over[324]<- 66
attacking_young_players$FIFA_Pot[324]<- 74
attacking_young_players$PES[324]<- 74
attacking_young_players$Transfer[324]<- 0.4

#Adzic
attacking_young_players$FIFA_Over[264]<- 66
attacking_young_players$FIFA_Pot[264]<- 75
attacking_young_players$PES[264]<- 75
attacking_young_players$Transfer[264]<- 0.9

attacking_young_players$Name
attacking_young_players[271,]

#Djerlek
attacking_young_players$FIFA_Over[271]<- 63
attacking_young_players$FIFA_Pot[271]<- 76
attacking_young_players$PES[271]<- 76
attacking_young_players$Transfer[271]<- 0.725

attacking_young_players$Name
attacking_young_players[242,]

#Mihajlovic
attacking_young_players$Transfer[254]<- 0.65
#Gavic
attacking_young_players$Transfer[317]<- 0.350
#Ficovic
attacking_young_players$Transfer[306]<- 0.2
#Birmancevic
attacking_young_players$Transfer[275]<- 0.725

#Senic
attacking_young_players$Transfer[327]<- 0.025
#L.Ilic
attacking_young_players$Transfer[242]<- 0.9
#Plavsic
attacking_young_players$Transfer[173]<- 1.2
#N.Mihajlovic
attacking_young_players$Transfer[157]<- 0.525

#Lukovic
attacking_young_players$Transfer[170]<- 0.325
#D.Zivkovic
attacking_young_players$Transfer[172]<- 0.2
#Tisma
attacking_young_players$Transfer[142]<- 0.05
#Lutovac
attacking_young_players$Transfer[192]<- 0.65




#Siladji
attacking_young_players$Transfer[121]<- 0.65
#Gacinovic
attacking_young_players$Transfer[129]<- 7
#Radonjic
attacking_young_players$Transfer[156]<- 6.5
#Denic
attacking_young_players$Transfer[155]<- 0.325

#Mesarovic
attacking_young_players$Transfer[257]<- 0.2
#Kokir
attacking_young_players$Transfer[309]<- 0.9
#Arsic
attacking_young_players$Transfer[222]<- 0.3
#Pavkov
attacking_young_players$Transfer[151]<- 3.5

#Malbasic
attacking_young_players$Transfer[65]<- 0.6
#Ninkovic
attacking_young_players$Transfer[78]<- 1.7
#Milunovic
attacking_young_players$Transfer[67]<- 0.325
#S.Jovanovic
attacking_young_players$Transfer[73]<- 0.325


#Pavlovski
attacking_young_players$Transfer[82]<- 0.2
#Meleg
attacking_young_players$Transfer[83]<- 0.3
#Sindjic
attacking_young_players$Transfer[85]<- 0.1
#Masovic
attacking_young_players$Transfer[89]<- 0.4

#Palocevic
attacking_young_players$Transfer[79]<- 0.6
#Zocevic
attacking_young_players$Transfer[80]<- 0.1
#Simic
attacking_young_players$Transfer[87]<- 0.95
#Ivanic
attacking_young_players$Transfer[88]<- 1.4

#Kostic
attacking_young_players$Transfer[94]<- 32
#Markovic
attacking_young_players$Transfer[97]<- 0.35
#Hajdin
attacking_young_players$Transfer[99]<- 0.475
#Jankovic
attacking_young_players$Transfer[107]<- 0.125



#Kostic
attacking_young_players$Transfer[94]<- 32
#Markovic
attacking_young_players$Transfer[97]<- 0.35
#Hajdin
attacking_young_players$Transfer[99]<- 0.475
#Jankovic
attacking_young_players$Transfer[107]<- 0.125

players <- attacking_young_players[!is.na(attacking_young_players$Transfer),]
str(players)

nrow(players)

players$Name

#Drazic
attacking_young_players$Transfer[182]<- 0.475
#Rovcanin
attacking_young_players$Transfer[130]<- 0.2
#Kraklajic
attacking_young_players$Transfer[117]<- 0.2
#Zivkovic
attacking_young_players$Transfer[116]<- 4


attacking_young_players$Name

#Micin
attacking_young_players$Transfer[233]<- 0.725
#Gojkov
attacking_young_players$Transfer[217]<- 0.375
#Damnjanovic
attacking_young_players$Transfer[120]<- 0.3


aym <- attacking_young_players[!is.na(attacking_young_players$Transfer),]
aym
nrow(aym)
attacking_young_players$FIFA_Over<-NULL

summary(aym$Transfer)
boxplot(aym$Age)

save(aym,"attacking_young_players.csv")


write.csv(aym,file="attacking_young_players.csv",sep="\t",row.names=FALSE)


amf <- read.csv("attacking_young_players.csv")
summary(amf)

amf[amf$Age==17]

name <- c("Crossing","Passing")

str(srb_players)

mid_f<-NULL

mid_f <-amf[,name]
summary(mid_f)
#
mid_f$Dribbling <- amf$Dribbling
#
mid_f$Finishing <- amf$Finishing
#
mid_f$Aggression<-amf$Aggression


mid_f$Dirtiness <- amf$Dirtiness
mid_f$Professional <- amf$Professional
mid_f$Technique <- amf$Technique
mid_f$Temperament <- amf$Temperament
mid_f$Passing <- amf$Passing
mid_f$Crossing <- amf$Crossing
mid_f$Age <- amf$Age

mid_f$Acceleration <- amf$Acceleration 

mid_f <- NULL



str(mid_f)

mid_f <- as.data.frame(mid_f)

mid_f$ValueAtTransferMarket <- amf$Transfer

mid_f <- as.data.frame(mid_f)

library(corrplot)
mid.matrix <- cor(mid_f)

corrplot.mixed(mid.matrix,lt.cex=1.75,number.cex=1.75)

cot.matrix

library(caret)

set.seed(123)
ind <- createDataPartition(mid_f$ValueAtTransferMarket,p=0.8,list=FALSE)
test_ds <- mid_f[-ind,]
train_ds <- mid_f[ind,]

lm1 <- lm(ValueAtTransferMarket~.,train_ds)

summary(lm1)
library(car)

sort(sqrt(vif(lm1)))

lm1.pred <- predict(lm1,test_ds)

rss <- sum((lm1.pred-test_ds$ValueAtTransferMarket)^2)
rss
tss <- sum((mean(train_ds$ValueAtTransferMarket)-test_ds$ValueAtTransferMarket)^2)
tss
rsq <- 1-(rss/tss)
rsq
rmse <- sqrt(rss/nrow(test_ds))
rmse

mid_f$Name <- amf$Name
str(mid_f)
mid_f$Acceleration <-amf$Acceleration
mid_f$ValueAtTransferMarket <- amf$Transfer

sort_players <- mid_f[order(mid_f$ValueAtTransferMarket,decreasing = TRUE),]
sort_players

sort_players$Acceleration <- NULL

m <- cor(sort_players[,-1])
corrplot.mixed(m,lt.cex=0.75,number.cex=0.75)

write.csv(sort_players,file="sort_by_value.csv",sep="\t",row.names=FALSE)


league_coef <- as.data.frame(c(1.2,1.15,1.10,1.05,1,0.9))

names(league_coef)<-c("league_coef")
league_coef$league_name <- c("top_5","top_10","top_15","top_20","top_30","Other")

sort_players <- read.csv(file="sort_by_value.csv")
str(sort_players)
summary(sort_players)

league_coef

sort_players$Games <- NA
sort_players$Goals <- NA
sort_players$Assists <- NA
sort_players$League <- NA

#SMS
sort_players$Games[1]<-31
sort_players$Goals[1]<-5
sort_players$Assists[1]<-7
sort_players$League[1]<-league_coef[1,2]

sort_players[2,]
#Kostic
sort_players$Games[2]<-40
sort_players$Goals[2]<-12
sort_players$Assists[2]<-15
sort_players$League[2]<-league_coef[1,2]

sort_players[3,]
#Gacinovic
sort_players$Games[3]<-27
sort_players$Goals[3]<-1
sort_players$Assists[3]<-2
sort_players$League[3]<-league_coef[1,2]

sort_players[4,]
#Radonjic
sort_players$Games[4]<-25
sort_players$Goals[4]<-6
sort_players$Assists[4]<-1
sort_players$League[4]<-league_coef[1,2]

sort_players[5,]
#Zivkovic
sort_players$Games[5]<-30
sort_players$Goals[5]<-0
sort_players$Assists[5]<-7
sort_players$League[5]<-league_coef[2,2]

sort_players[6,]
#Pavkov
sort_players$Games[6]<-25
sort_players$Goals[6]<-9
sort_players$Assists[6]<-3
sort_players$League[6]<-league_coef[4,2]

sort_players[7,]
#Ninkovic
sort_players$Games[7]<-23
sort_players$Goals[7]<-6
sort_players$Assists[7]<-10
sort_players$League[7]<-league_coef[6,2]

sort_players[8,]
#Ivanic
sort_players$Games[8]<-26
sort_players$Goals[8]<-1
sort_players$Assists[8]<-2
sort_players$League[8]<-league_coef[4,2]

sort_players[9,]
#Pantic
sort_players$Games[9]<-11
sort_players$Goals[9]<-0
sort_players$Assists[9]<-1
sort_players$League[9]<-league_coef[6,2]

sort_players[10,]
#Plavsic
sort_players$Games[10]<-24
sort_players$Goals[10]<-1
sort_players$Assists[10]<-5
sort_players$League[10]<-league_coef[3,2]

sort_players[11,]
#Simic
sort_players$Games[11]<-49
sort_players$Goals[11]<-7
sort_players$Assists[11]<-2
sort_players$League[11]<-league_coef[6,2]


sort_players[12,]
#Filipovic
sort_players$Games[12]<-46
sort_players$Goals[12]<-1
sort_players$Assists[12]<-2
sort_players$League[12]<-league_coef[5,2]

head(sort_players,12)

sort_players[13,]
#Filipovic
sort_players$Games[13]<-29
sort_players$Goals[13]<-5
sort_players$Assists[13]<-4
sort_players$League[13]<-league_coef[2,2]


sort_players[14,]
#Adzic
sort_players$Games[14]<-33
sort_players$Goals[14]<-9
sort_players$Assists[14]<-4
sort_players$League[14]<-league_coef[4,2]


sort_players[15,]
#Micin
sort_players$Games[15]<-18
sort_players$Goals[15]<-5
sort_players$Assists[15]<-1
sort_players$League[15]<-league_coef[4,2]

sort_players[16,]
#Djerlek
sort_players$Games[16]<-18
sort_players$Goals[16]<-1
sort_players$Assists[16]<-0
sort_players$League[16]<-league_coef[4,2]

sort_players[17,]
#Birmancevic
sort_players$Games[17]<-29
sort_players$Goals[17]<-4
sort_players$Assists[17]<-8
sort_players$League[17]<-league_coef[4,2]

sort_players[18,]
#Siladji
#Dodato ali nije kopirano


#Lutovac
sort_players$Games[19]<-11
sort_players$Goals[19]<-1
sort_players$Assists[19]<-4
sort_players$League[19]<-league_coef[4,2]


sort_players[20,]
#Mihajlovic
sort_players$Games[20]<-27
sort_players$Goals[20]<-11
sort_players$Assists[20]<-2
sort_players$League[20]<-league_coef[4,2]


sort_players[21,]
#Malbasic
sort_players$Games[21]<-26
sort_players$Goals[21]<-4
sort_players$Assists[21]<-4
sort_players$League[21]<-league_coef[6,2]


sort_players[22,]
#Polocevic
sort_players$Games[22]<-11
sort_players$Goals[22]<-2
sort_players$Assists[22]<-4
sort_players$League[22]<-league_coef[6,2]

sort_players[23,]
#Mihajlovic
sort_players$Games[23]<-24
sort_players$Goals[23]<-4
sort_players$Assists[23]<-5
sort_players$League[23]<-league_coef[5,2]

sort_players[24,]
#Hajdin
sort_players$Games[24]<-23
sort_players$Goals[24]<-1
sort_players$Assists[24]<-3
sort_players$League[24]<-league_coef[4,2]


sort_players[25,]
#Drazic
sort_players$Games[25]<-31
sort_players$Goals[25]<-7
sort_players$Assists[25]<-7
sort_players$League[25]<-league_coef[5,2]


sp <- sort_players[1:10,]

summary(sp)
sp$League <- as.factor(sp$League)
sp$league <- NULL

ncol(sp)

ec <- c(-1,-15)

elim_c <- c(-1,-2,-3,-4,-5,-6,-7,-15)

cor.m <- cor(sp[,ec])

corrplot.mixed(cor.m,lt.cex=0.9,number.cex=0.9)
library(corrplot)

sp$SumApp <- 1.2*sp$Games+0.7*sp$Assists+1.1*sp$Goals
sp$SumApp <- as.numeric(sp$SumApp)

summary(sp$SumApp)
summary(sp$ValueAtTransferMarket)

length(sp$SumApp)

norm <- function(column){
  val <- ((column-min(column))/(max(column)-min(column)))
}

a <- norm(sp$SumApp)

summary(a)

sp$TMC <- sp$ValueAtTransferMarket

boxplot(sp$TMC)

boxplot.stats(sp$TMC)$stats
quantile(sp$TMC,seq(0.8,1,0.025))
tm_3q <- quantile(sp$TMC,0.75)
sp$TMC[sp$TMC>tm_3q]<-tm_3q

summary(sp$TMC)

sp$NormSum <- a
sp$NormTransfer <- norm(sp$TMC)

sp$Total <- sp$NormSum+0.7*sp$NormTransfer



str(sp)
summary(sp)

sort(sp$Total)

sp[order(sp$Total)]
