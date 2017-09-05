

# R code to extract and prepare data for Maharashtra from 
# 1. Candidates data from election commission site
# 2. Religion ( distribution of hindu and muslim population) from Census 2011
# 3. Literacy and Sex Ratio from Census ( extracted from MapsOfIndia website)
# 4. Party incumbency effect from 2004 election winners

library(data.table)
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

# EC data for MH
EC.Cand.2009.MH <- filter(EC_2009_Cand,ST_CODE=='S13')
EC.Const.MH.2009 <- unique(EC.Cand.2009.MH$PC.name)
EC.Const.MH.2009 <- as.data.frame(EC.Const.MH.2009)
EC.Const.MH.2009 <- plyr::rename(EC.Const.MH.2009, c(EC.Const.MH.2009="Constituency"))
View(EC.Const.MH.2009)
write.csv(EC.Const.MH.2009,"EC.CONST.MH.2009.csv")


# Load MP religion data from 2011 census
# source :http://www.censusindia.gov.in/2011census/C-01.html

MH_Religion <-read.csv("MH_Religion.csv", header=T)
View(MH_Religion)

# Match the constituencies in EC data with the district in Religion data 

#distance.methods<-c('lv','dl','jaccard','jw')
distance.methods<-c('jw')
dist.methods<-list()

for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(MH_Religion$Area.Name),nrow = length(EC.Const.MH.2009$Constituency))
  for(i in 1:length(MH_Religion$Area.Name)) {
    for(j in 1:length(EC.Const.MH.2009$Constituency)) { 
      dist.name.enh[j,i]<-stringdist(tolower(MH_Religion[i,]$Area.Name),tolower(EC.Const.MH.2009[j,]),method = distance.methods[m])      
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
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=MH_Religion[s2.i,]$Area.Name, s1name=EC.Const.MH.2009[s1.i,], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}

matched.names.matrix.1<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.1)

write.csv(matched.names.matrix.1,"matched.name.matrix.MH.csv")


# Loading the matched constituency and district file 
matched.dist<-read.csv("MH-ReligionDistrict-Const-Matching.csv", header=T)
MH_Religion$MH_Religion_District <- MH_Religion$Area.Name

# Merge MP_Religion with matched.dist
MH_Religion <- merge(MH_Religion,matched.dist,by=c("MH_Religion_District"), all=TRUE)

MH_Religion <- plyr::rename(MH_Religion, c(MH_Constituency="PC.name"))
#Now merge UP_
EC.Cand.2009.MH.Final <-merge(EC.Cand.2009.MH,MH_Religion,by=c("PC.name"), all=TRUE)

# Select the requied rows and colums only 
EC.Cand.2009.MH.Final <- subset(EC.Cand.2009.MH.Final, select=c(1,3,8:18,24,25,26))
EC.Cand.2009.MH.Final<-EC.Cand.2009.MH.Final[1:819,]
write.csv(EC.Cand.2009.MP.Final,"EC_Religion_Matched_MH.csv")



#####  Merge above dataset with Literacy and Sex ratio data  #######

#Extract the literacy data for MH districts 

urlMH<-"http://www.mapsofindia.com/maps/maharashtra/maharashtra.htm"

#Our table is the 12th table
LiteracyData_MH<-readHTMLTable(urlMH,which=12)
write.csv(LiteracyData_MH,"LiteracyData_MH.csv")
View(LiteracyData_MH)

LiteracyData_MH <- LiteracyData_MH[-c(20),]
# do a string match for district of MH Literacy data and constituency from EC data 

distance.methods<-c('jw')
dist.methods<-list()

for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(LiteracyData_MH$District),nrow = length(EC.Const.MH.2009$Constituency))
  for(i in 1:length(LiteracyData_MH$District)) {
    for(j in 1:length(EC.Const.MH.2009$Constituency)) { 
      dist.name.enh[j,i]<-stringdist(tolower(LiteracyData_MH[i,]$District),tolower(EC.Const.MH.2009[j,]),method = distance.methods[m])      
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
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=LiteracyData_MH[s2.i,]$District, s1name=EC.Const.MH.2009[s1.i,], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}

matched.names.matrix.1<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.1)

write.csv(matched.names.matrix.1,"matched.Literacy.matrix.MH.csv")


# Loading the matched constituency and district wise Literacy file
matched.dist<-read.csv("MH-LiteracyDistrict-Const-Matching.csv", header=T)
LiteracyData_MH$MH_Literacy <- LiteracyData_MH$District

# Merge MP_Literacy with matched.dist
LiteracyData_MH <- merge(LiteracyData_MH,matched.dist,by=c("MH_Literacy"), all=TRUE)

LiteracyData_MH <- plyr::rename(LiteracyData_MH, c(MH_Constituency="PC.name"))
#Now merge with EC data 
EC.Cand.2009.MH.Final2 <-merge(EC.Cand.2009.MH.Final,LiteracyData_MH,by=c("PC.name"), all=TRUE)

# Select the requied rows and colums only 
EC.Cand.2009.MH.Final3 <- subset(EC.Cand.2009.MH.Final2, select=-c(17:21))
EC.Cand.2009.MH.Final3 <-EC.Cand.2009.MH.Final3[1:819,]

write.csv(EC.Cand.2009.MH.Final3,"EC.Final.MH.2009.csv")



#####  Merge above dataset with incumbency effect from 2004 Lok Sabha election #######

## Add data for incumbency effect  MH

EC.2004.MH.Winners <- read.csv("GE_2004_Winners_MH.csv", header=T)


# do a string match for Constituency in 2004 and 2009 data
distance.methods<-c('jw')
dist.methods<-list()

for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(EC.2004.MH.Winners$PC_NAME),nrow = length(EC.Const.MH.2009$Constituency))
  for(i in 1:length(EC.2004.MH.Winners$PC_NAME)) {
    for(j in 1:length(EC.Const.MH.2009$Constituency)) { 
      dist.name.enh[j,i]<-stringdist(tolower(EC.2004.MH.Winners[i,]$PC_NAME),tolower(EC.Const.MH.2009[j,]),method = distance.methods[m])      
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
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=EC.2004.MH.Winners[s2.i,]$PC_NAME, s1name=EC.Const.MH.2009[s1.i,], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}

matched.names.matrix.1<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.1)

write.csv(matched.names.matrix.1,"matched.2004and2009.matrix.MH.csv")

# Loading the matched constituency data for 2004 and 2009
matched.dist<-read.csv("matched.2004and2009.EC.MH.csv", header=T)
EC.2004.MH.Winners$EC_CONSTITUENCY_2004 <- EC.2004.MH.Winners$PC_NAME

# Merge UP_Religion with matched.dist
EC.2004.MH.Winners <- merge(EC.2004.MH.Winners,matched.dist,by=c("EC_CONSTITUENCY_2004"), all=TRUE)
EC.2004.MH.Winners <- filter(EC.2004.MH.Winners,EC_CONSTITUENCY_2009 != 'NA')

EC.2004.MH.Winners <- plyr::rename(EC.2004.MH.Winners, c(EC_CONSTITUENCY_2009="PC.name"))

EC.Cand.2009.MH.Final4 <-merge(EC.Cand.2009.MH.Final3,EC.2004.MH.Winners,by=c("PC.name"), all=TRUE)

# Select the requied rows and colums only 
EC.Cand.2009.MH.Final5 <- subset(EC.Cand.2009.MH.Final4, select=-c(19:21))

# Rename the column for incumbent party 
EC.Cand.2009.MH.Final5 <- plyr::rename(EC.Cand.2009.MH.Final5, c(PARTYABBRE="INCUMBENT_PARTY"))

# Final file with incumbency data 
write.csv(EC.Cand.2009.MH.Final5,"EC.Final.MH.2009.csv")




