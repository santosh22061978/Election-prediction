
# R code to extract and prepare data for UP from 
# 1. Candidates data from election commission site
# 2. Religion ( distribution of hindu and muslim population) from Census 2011
# 3. Literacy and Sex Ratio from Census ( extracted from MapsOfIndia website)
# 4. Party incumbency effect from 2004 election winners

library(data.table)
library(dplyr)
library(sqldf)
library(stringdist)
library(reshape2)
library(RCurl)
library(XML)
library(data.table)

setwd( "C:/Users/user/Documents/GL-Classes/7/grp-assignment")

# Load EC Data for 2009 

EC_2009_Cand<-read.csv("GE_2009_Candidates.csv", header=T)
# Filter out blank rows form the dataset 
EC_2009_Cand <- filter(EC_2009_Cand,PC.name!='')
str(EC_2009_Cand)
View(EC_2009_Cand)


#Extract unique constituencies 
EC.Const.2009 <- as.data.frame(unique(EC_2009_Cand$PC.name))

#Extract unique states
EC.State.2009 <- as.data.frame(unique(EC_2009_Cand$State.name))

#####  Merge EC 2009 data with Religion data downloaded from Census 2011 #######

# EC data for UP
EC.Cand.2009.UP <- filter(EC_2009_Cand,ST_CODE=='S24')
EC.Const.UP.2009 <- unique(EC.Cand.2009.UP$PC.name)
EC.Const.UP.2009 <- as.data.frame(EC.Const.UP.2009)
EC.Const.UP.2009 <- plyr::rename(EC.Const.UP.2009, c(EC.Const.UP.2009="Constituency"))
View(EC.Const.UP.2009)
write.csv(EC.Const.UP.2009,"EC.CONST.UP.2009.csv")


# Load UP religion data from 2011 census
UP_Religion <-read.csv("UP_Religion.csv", header=T)
View(UP_Religion)

distance.methods<-c('lv','dl','jaccard','jw')
#distance.methods<-c('jw')
dist.methods<-list()

for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(UP_Religion$Area.Name),nrow = length(EC.Const.UP.2009$Constituency))
  for(i in 1:length(UP_Religion$Area.Name)) {
    for(j in 1:length(EC.Const.UP.2009$Constituency)) { 
      dist.name.enh[j,i]<-stringdist(tolower(UP_Religion[i,]$Area.Name),tolower(EC.Const.UP.2009[j,]),method = distance.methods[m])      
      #adist.enhance(UP_Crime[i,]$name,EC.Dist[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
  dist.matrix.1<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix.1, 1, base::min)
  for(i in 1:nrow(dist.matrix.1))
  {
    s2.i<-match(min.name.enh[i],dist.matrix.1[i,])
    s1.i<-i
    #Changed by Kapil 
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=UP_Religion[s2.i,]$Area.Name, s1name=EC.Const.UP.2009[s1.i,], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}

matched.names.matrix.1<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.1)

write.csv(matched.names.matrix.1,"matched.name.matrix.1.csv")


# Loading the matched constituency and district file 
matched.dist<-read.csv("UP-ReligionDistrict-Const-Matching.csv", header=T)
UP_Religion$UP_Religion_District <- UP_Religion$Area.Name

# Merge UP_Religion with matched.dist
UP_Religion <- merge(UP_Religion,matched.dist,by=c("UP_Religion_District"), all=TRUE)

UP_Religion <- plyr::rename(UP_Religion, c(UP_Constituency="PC.name"))
#Now merge UP_
EC.Cand.2009.UP.Final <-merge(EC.Cand.2009.UP,UP_Religion,by=c("PC.name"), all=TRUE)

# Select the requied rows and colums only 
EC.Cand.2009.UP.Final <- subset(EC.Cand.2009.UP.Final, select=c(1,3,8:18,24,25,26))
EC.Cand.2009.UP.Final<-EC.Cand.2009.UP.Final[1:1368,]



#####  Merge above dataset with Literacy and Sex ratio data  #######
#Extract the literacy data for UP districts 

urlUP<-"http://www.mapsofindia.com/maps/uttarpradesh/uttar-pradesh-district.htm"

#Our table is the 12th table
LiteracyData_UP<-readHTMLTable(urlUP,which=12)
write.csv(LiteracyData_UP,"LiteracyData_UP.csv")
str(LiteracyData_UP)

# do a string match for district of UP Literacy data and constituency from EC data 

distance.methods<-c('jw')
dist.methods<-list()

for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(LiteracyData_UP$District),nrow = length(EC.Const.UP.2009$Constituency))
  for(i in 1:length(LiteracyData_UP$District)) {
    for(j in 1:length(EC.Const.UP.2009$Constituency)) { 
      dist.name.enh[j,i]<-stringdist(tolower(LiteracyData_UP[i,]$District),tolower(EC.Const.UP.2009[j,]),method = distance.methods[m])      
      #adist.enhance(UP_Crime[i,]$name,EC.Dist[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
  dist.matrix.1<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix.1, 1, base::min)
  for(i in 1:nrow(dist.matrix.1))
  {
    s2.i<-match(min.name.enh[i],dist.matrix.1[i,])
    s1.i<-i
    #Changed by Kapil 
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=LiteracyData_UP[s2.i,]$District, s1name=EC.Const.UP.2009[s1.i,], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}

matched.names.matrix.1<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.1)

write.csv(matched.names.matrix.1,"matched.Literacy.matrix.UP.csv")


# Loading the matched constituency and district wise Literacy file
matched.dist<-read.csv("UP-LiteracyDistrict-Const-Matching.csv", header=T)
LiteracyData_UP$UP_Literacy <- LiteracyData_UP$District

# Merge UP_Religion with matched.dist
LiteracyData_UP <- merge(LiteracyData_UP,matched.dist,by=c("UP_Literacy"), all=TRUE)

LiteracyData_UP <- plyr::rename(LiteracyData_UP, c(UP_Constituency="PC.name"))
#Now merge UP_ with UP.Crime
EC.Cand.2009.UP.Final2 <-merge(EC.Cand.2009.UP.Final,LiteracyData_UP,by=c("PC.name"), all=TRUE)

# Select the requied rows and colums only 
EC.Cand.2009.UP.Final3 <- subset(EC.Cand.2009.UP.Final2, select=-c(17:21))
EC.Cand.2009.UP.Final3<-EC.Cand.2009.UP.Final3[1:1368,]

write.csv(EC.Cand.2009.UP.Final3,"EC.Final.UP.2009.csv")


#####  Merge above dataset with incumbency effect from 2004 Lok Sabha election #######

## Add data for incumbency effect  UP

EC.2004.UP.Winners <- read.csv("GE_2004_Winners_UP.csv", header=T)


# do a string match for Constituency in 2004 and 2009 data
distance.methods<-c('jw')
dist.methods<-list()

for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(EC.2004.UP.Winners$PC_NAME),nrow = length(EC.Const.UP.2009$Constituency))
  for(i in 1:length(EC.2004.UP.Winners$PC_NAME)) {
    for(j in 1:length(EC.Const.UP.2009$Constituency)) { 
      dist.name.enh[j,i]<-stringdist(tolower(EC.2004.UP.Winners[i,]$PC_NAME),tolower(EC.Const.UP.2009[j,]),method = distance.methods[m])      
      #adist.enhance(UP_Crime[i,]$name,EC.Dist[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
  dist.matrix.1<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix.1, 1, base::min)
  for(i in 1:nrow(dist.matrix.1))
  {
    s2.i<-match(min.name.enh[i],dist.matrix.1[i,])
    s1.i<-i
    #Changed by Kapil 
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=EC.2004.UP.Winners[s2.i,]$PC_NAME, s1name=EC.Const.UP.2009[s1.i,], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}

matched.names.matrix.1<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.1)

write.csv(matched.names.matrix.1,"matched.2004and2009.matrix.UP.csv")

# Loading the matched constituency data for 2004 and 2009
matched.dist<-read.csv("matched.2004and2009.EC.UP.csv", header=T)
EC.2004.UP.Winners$EC_CONSTITUENCY_2004 <- EC.2004.UP.Winners$PC_NAME

# Merge UP_Religion with matched.dist
EC.2004.UP.Winners <- merge(EC.2004.UP.Winners,matched.dist,by=c("EC_CONSTITUENCY_2004"), all=TRUE)
EC.2004.UP.Winners <- filter(EC.2004.UP.Winners,EC_CONSTITUENCY_2009 != 'NA')

EC.2004.UP.Winners <- plyr::rename(EC.2004.UP.Winners, c(EC_CONSTITUENCY_2009="PC.name"))
#Now merge UP_ with UP.Crime
EC.Cand.2009.UP.Final4 <-merge(EC.Cand.2009.UP.Final3,EC.2004.UP.Winners,by=c("PC.name"), all=TRUE)

# Select the requied rows and colums only 
EC.Cand.2009.UP.Final5 <- subset(EC.Cand.2009.UP.Final4, select=-c(19:21))

# Rename the column for incumbent party 
EC.Cand.2009.UP.Final5 <- plyr::rename(EC.Cand.2009.UP.Final5, c(PARTYABBRE="INCUMBENT_PARTY"))

# Final file with incumbency data 
write.csv(EC.Cand.2009.UP.Final5,"EC.Final.UP.2009.csv")


