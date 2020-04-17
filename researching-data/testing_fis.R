top_30_first_fis <- read.csv("top_30_first_fis.csv")


str(top_30_first_fis)

top_30_first_fis$fis2 <-NA

top_30_first_fis[1,]
top_30_first_fis$fis2[1]<-1.48

top_30_first_fis[2,]
top_30_first_fis$fis2[2]<-1.5

top_30_first_fis[3,]
top_30_first_fis$fis2[3]<-1.5

top_30_first_fis[4,]
top_30_first_fis$fis2[4]<-1.5

top_30_first_fis[5,]
top_30_first_fis$fis2[5]<-1.5

top_30_first_fis[6,]
top_30_first_fis$fis2[6]<-1.5

top_30_first_fis[7,]
top_30_first_fis$fis2[7]<-1.48

top_30_first_fis[8,]
top_30_first_fis$fis2[8]<-0.838

top_30_first_fis[9,]
top_30_first_fis$fis2[9]<-1.5

top_30_first_fis[10,]
top_30_first_fis$fis2[10]<-1.46


top_30_first_fis[11,]
top_30_first_fis$fis2[11]<-0.47

top_30_first_fis[12,]
top_30_first_fis$fis2[12]<-0.767

top_30_first_fis[13,]
top_30_first_fis$fis2[13]<-0.847

top_30_first_fis[14,]
top_30_first_fis$fis2[14]<-0.844

top_30_first_fis[15,]
top_30_first_fis$fis2[15]<-0.515

top_30_first_fis[16,]
top_30_first_fis$fis2[16]<-0.818

top_30_first_fis[17,]
top_30_first_fis$fis2[17]<-0.67

top_30_first_fis[18,]
top_30_first_fis$fis2[18]<-0.466

top_30_first_fis[19,]
top_30_first_fis$fis2[19]<-1.04

top_30_first_fis[20,]
top_30_first_fis$fis2[20]<-0.67


top_30_first_fis[21,]
top_30_first_fis$fis2[21]<-1.5

top_30_first_fis[22,]
top_30_first_fis$fis2[22]<-1.49

top_30_first_fis[23,]
top_30_first_fis$fis2[23]<-1.48

top_30_first_fis[24,]
top_30_first_fis$fis2[24]<-1.44

top_30_first_fis[25,]
top_30_first_fis$fis2[25]<-1.48

top_30_first_fis[26,]
top_30_first_fis$fis2[26]<-0.467

top_30_first_fis[27,]
top_30_first_fis$fis2[27]<-0.467

top_30_first_fis[28,]
top_30_first_fis$fis2[28]<-1.07

top_30_first_fis[29,]
top_30_first_fis$fis2[29]<-0.843

top_30_first_fis[30,]
top_30_first_fis$fis2[30]<-1.48

df <- top_30_first_fis

df$fis1 <- NA

df[1,]
df$fis1[1]<-0.4

df[2,]
df$fis1[2]<-1.5

df[3,]
df$fis1[3]<-1.5

df[4,]
df$fis1[4]<-1.5

df[5,]
df$fis1[5]<-1.5

df[6,]
df$fis1[6]<-0.47

df[7,]
df$fis1[7]<-1.5

df[8,]
df$fis1[8]<-0.47

df[9,]
df$fis1[9]<-0.486

df[10,]
df$fis1[10]<-1.49



df[11,]
df$fis1[11]<-1.04

df[12,]
df$fis1[12]<-0.84
df[13,]
df$fis1[13]<-0.47
df[14,]
df$fis1[14]<-0.76
df[15,]
df$fis1[15]<-0.84
df[16,]
df$fis1[16]<-0.81
df[17,]
df$fis1[17]<-0.81
df[18,]
df$fis1[18]<- 1.04
df[19,]
df$fis1[19]<- 0.47
df[20,]  
df$fis1[20]<- 0.47
top_15 <- df[1:15,]

df[21,]
df$fis1[21]<-1.5
df[22,]
df$fis1[22] <- 1.5
df[23,]
df$fis1[23] <- 1.5
df[24,]
df$fis1[24] <- 0.47
df[25,]
df$fis1[25] <- 1.5

df[26,]
df$fis1[26] <- 1.04
df[27,]
df$fis1[27] <- 0.81
df[28,]
df$fis1[28] <- 1.04
df[29,]
df$fis1[29]<-0.47
df[30,]
df$fis1[30] <- 0.47



str(top_30_first_fis)
fis_import <- c(1,2,19,20)
top_30_first_fis<-df
top_30_first_fis[fis_import]

top_30_first_fis$NormCategory <- NA
top_30_first_fis$NormCategory[1:10]<- 'A'
top_30_first_fis$NormCategory[11:20]<-'B'
top_30_first_fis$NormCategory[21:30]<-'C'

fis1_33 <- quantile(top_30_first_fis$fis1,0.33)
fis1_66<-quantile(top_30_first_fis$fis1,0.66)
str(top_30_first_fis)
top_30_first_fis$NormCategory<-as.factor(top_30_first_fis$NormCategory)

top_30_first_fis$FirstFis <- NA
top_30_first_fis$FirstFis[top_30_first_fis$fis1<=fis1_33]<-'C'
top_30_first_fis$FirstFis[top_30_first_fis$fis1>=fis1_33&top_30_first_fis$fis1<=fis1_66]<-'B'
top_30_first_fis$FirstFis[top_30_first_fis$fis1>=fis1_66]<-'A'
top_30_first_fis$FirstFis <- as.factor(top_30_first_fis$FirstFis)



fis2_33 <- quantile(top_30_first_fis$fis2,0.33)
fis2_66<-quantile(top_30_first_fis$fis2,0.66)
str(top_30_first_fis)
top_30_first_fis$NormCategory<-as.factor(top_30_first_fis$NormCategory)

top_30_first_fis$SecoundFis <- NA
top_30_first_fis$SecoundFis[top_30_first_fis$fis2<=fis2_33]<-'C'
top_30_first_fis$SecoundFis[top_30_first_fis$fis2>=fis2_33&top_30_first_fis$fis2<=fis2_66]<-'B'
top_30_first_fis$SecoundFis[top_30_first_fis$fis2>=fis2_66]<-'A'
top_30_first_fis$SecoundFis <- as.factor(top_30_first_fis$SecoundFis)


top_30_first_fis[,c(1,2,19,20,21,22,23)]


secound_cm <- table(true=top_30_first_fis[,21],predicted=top_30_first_fis[,23])
secound_cm
