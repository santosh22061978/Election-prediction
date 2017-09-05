# PM Group 13
# R script for exploratory data analysis 
library(dplyr)
library(data.table)
library(caret)
library(plyr)
library(car)
library(psych)
library(e1071)

setwd( "C:/Users/user/Documents/GL-Classes/7/grp-assignment")

GE_2009.Analysis<-read.csv("Final_Merged_Clean_ALL_2009.csv", header=T)

str(GE_2009.Analysis)

GE_2009.Analysis <- as.data.table(GE_2009.Analysis)

# Check for missing values acorss all the columns 
as.matrix(sapply(GE_2009.Analysis, function(x) sum(is.na(x))))

#Impute the NA values with the mean 

GE_2009.Analysis$Total.Persons[is.na(GE_2009.Analysis$Total.Persons)] <- mean(GE_2009.Analysis$Total.Persons,na.rm=TRUE)
GE_2009.Analysis$Total.HinduPersons[is.na(GE_2009.Analysis$Total.HinduPersons)] <- mean(GE_2009.Analysis$Total.HinduPersons,na.rm=TRUE)
GE_2009.Analysis$Total.Muslim.Persons[is.na(GE_2009.Analysis$Total.Muslim.Persons)] <- mean(GE_2009.Analysis$Total.Muslim.Persons,na.rm=TRUE)
GE_2009.Analysis$Sex.Ratio..per.1000.[is.na(GE_2009.Analysis$Sex.Ratio..per.1000.)] <- mean(GE_2009.Analysis$Sex.Ratio..per.1000.,na.rm=TRUE)
GE_2009.Analysis$Average.Literacy[is.na(GE_2009.Analysis$Average.Literacy)] <- mean(GE_2009.Analysis$Average.Literacy,na.rm=TRUE)



#Define some variables

#1 . Muslim percentage for each constituency 
GE_2009.Analysis <- GE_2009.Analysis[,MuslimPercent:=(round((Total.Muslim.Persons/Total.Persons)*100,2))]

#2. Hindu percentage for each constituency
GE_2009.Analysis <- GE_2009.Analysis[,HinduPercent:=(round((Total.HinduPersons/Total.Persons)*100,2))]

#3. Vote share per candidate 

GE_2009.Analysis <- GE_2009.Analysis[,VoteShare:=(round((Total.Votes.Polled/Total.voters)*100,2))]

#4. Candidate is graduate and above or not 
# 1 = Graduate or above 
# 0 = Not graduate 
levels(GE_2009.Analysis$Education)
GE_2009.Analysis$Graduate<-ifelse(GE_2009.Analysis$Education =="Graduate",1,0)
GE_2009.Analysis$Doctorate<-ifelse(GE_2009.Analysis$Education=="Doctorate",1,0)
GE_2009.Analysis$Graduate.Professional<-ifelse(GE_2009.Analysis$Education=="Graduate Professional",1,0)
GE_2009.Analysis$Post.Graduate<-ifelse(GE_2009.Analysis$Education =="Post Graduate",1,0)
GE_2009.Analysis$IsGraduate=GE_2009.Analysis$Graduate+GE_2009.Analysis$Doctorate+GE_2009.Analysis$Graduate.Professional + GE_2009.Analysis$Post.Graduate
GE_2009.Analysis$Qual<-as.factor(GE_2009.Analysis$Education )



#5. Candidate is criminal or not
# 1 = candidate with 1 or more than 1 criminal cases
# 0 = candidate with no criminal cases
 #GE_2009.Analysis$Criminal.Case <- as.factor(GE_2009.Analysis$Criminal.Case)
GE_2009.Analysis$HaveCriminalCharges <-ifelse(GE_2009.Analysis$Criminal.Case>0,1,0)

#6. Candidate is Crorepati or not 
# 1 = Candidate is Crorepati
# 0 = candidate is not Crorepati
GE_2009.Analysis$IsCrorepati<-ifelse(GE_2009.Analysis$`Total.Assets`>10000000,1,0)

#7 Rank candidates as top 3 rich and not rich 
# R1 = Rank 1 rich candidate 
# R2 = Rank 2 rich candidate 
# R3 = Rank 3 rich candidate 

GE_2009.Analysis <- transform(GE_2009.Analysis, 
                         Asset.Rank = ave(GE_2009.Analysis$Total.Assets, GE_2009.Analysis$PC.name, 
                                          FUN = function(x) rank(-x, ties.method = "first")))
GE_2009.Analysis$Rich<-ifelse(GE_2009.Analysis$Asset.Rank==1,"R1",
                         ifelse(GE_2009.Analysis$Asset.Rank==2,"R2",
                                ifelse(GE_2009.Analysis$Asset.Rank==3,"R3","NR")))

GE_2009.Analysis$Rich<-as.factor(GE_2009.Analysis$Rich)

# 8. National party 
# Following are considered as national party 
# INC, BJP, SP, BSP, SHS, NCP, ADMK, DMK, AITC, CPI,CPM
levels(GE_2009.Analysis$Party.Abbreviation)

GE_2009.Analysis$INC<-ifelse(GE_2009.Analysis$Party.Abbreviation=='INC',1,0)
GE_2009.Analysis$BJP<-ifelse(GE_2009.Analysis$Party.Abbreviation=="BJP",1,0)
GE_2009.Analysis$BSP<-ifelse(GE_2009.Analysis$Party.Abbreviation=="BSP",1,0)
GE_2009.Analysis$CPI<-ifelse(GE_2009.Analysis$Party.Abbreviation=="CPI",1,0)
GE_2009.Analysis$CPM<-ifelse(GE_2009.Analysis$Party.Abbreviation=="CPM",1,0)
GE_2009.Analysis$NCP<-ifelse(GE_2009.Analysis$Party.Abbreviation=="NCP",1,0)
GE_2009.Analysis$SP<-ifelse(GE_2009.Analysis$Party.Abbreviation=="SP",1,0)

GE_2009.Analysis$National<-GE_2009.Analysis$INC+GE_2009.Analysis$BJP+GE_2009.Analysis$BSP+GE_2009.Analysis$CPI+GE_2009.Analysis$CPM+GE_2009.Analysis$NCP+GE_2009.Analysis$SP

# 9. Regional Party 
# AITC, SHS, DMK, ADMK

GE_2009.Analysis$AITC<-ifelse(GE_2009.Analysis$Party.Abbreviation=='AITC',1,0)
GE_2009.Analysis$SHS<-ifelse(GE_2009.Analysis$Party.Abbreviation=="SHS",1,0)
GE_2009.Analysis$DMK<-ifelse(GE_2009.Analysis$Party.Abbreviation=="DMK",1,0)
GE_2009.Analysis$ADMK<-ifelse(GE_2009.Analysis$Party.Abbreviation=="ADMK",1,0)

GE_2009.Analysis$Regional <-GE_2009.Analysis$AITC+GE_2009.Analysis$SHS+GE_2009.Analysis$DMK+GE_2009.Analysis$ADMK

# 10. SC/ST Candidate 
GE_2009.Analysis$IsSC <-ifelse(GE_2009.Analysis$Candidate.Category =='SC',1,0)
GE_2009.Analysis$IsST <-ifelse(GE_2009.Analysis$Candidate.Category =='ST',1,0)

GE_2009.Analysis$Is_SC_ST <- GE_2009.Analysis$IsSC + GE_2009.Analysis$IsST
# Write data to csv 
write.csv(GE_2009.Analysis,"EC_2009_ExtraVariables.csv")


# Cluster analysis to see if there are any cluster formation 

#Cluster based on constituencies  


Cluster.const  <- ddply(GE_2009.Analysis, .(PC.name), transform,  crmean=mean(HaveCriminalCharges),
                      femmean=mean(Women),meancrore=mean(IsCrorepati),ElT=mean(Total_Electors),
                      MuslimT=mean(Total.Muslim.Persons),HinduT=mean(Total.HinduPersons),
                      pollP=mean(POLL.PERCENTAGE),Mser=mean(SeriousCrime),MuslimPer= mean(MuslimPercent),
                      HinduPer=mean(HinduPercent),AvgLiteracy=mean(Average.Literacy))


Cluster.const.1<-subset(Cluster.const,select=c(2, 56:66))
Cluster.const.2<- Cluster.const.1[!duplicated(Cluster.const.1), ]

# Scale all the variables for cluster analysis 

maxs <- apply(Cluster.const.2[,-c(1)],2,max)
mins <- apply(Cluster.const.2[,-c(1)],2,min)

scaled_data <- as.data.frame(scale(Cluster.const.2[,-c(1)],center = mins,scale = maxs-mins))
str(scaled_data)


const_id <- subset(Cluster.const.2, select = c(PC.name))

Cluster.Scaled.Const <- cbind(const_id,scaled_data)

#Make the data in random order

randomorder<-runif(nrow(Cluster.Scaled.Const))
Cluster.Scaled.Const<-Cluster.Scaled.Const[order(randomorder),]

# Remove the constituency column 
Cluster.Scaled.Const.withoutID <- Cluster.Scaled.Const[,-c(1)]

#Define Clusters:
#Clustering all attributes
#Euceldian dist
distance<-dist(Cluster.Scaled.Const.withoutID)
print(distance, digits=3)

# Aletrante Distance function 
#d_Euc<-dist(nmatrix, method="euclidean")
#d_Mht<-dist(nmatrix, method="manhattan")
#Hierarchial clustering (ward method: each points are indiv clusters,
#then aggregate to minimize Within SS)

#Cluster Dendogram with Complete Linkage
hc.EC_c<-hclust(distance)
plot(hc.EC_c, labels=Cluster.Scaled.Const$PC.name)
print(hc.EC_c)

#Cluster Dendogram with Average linkage

hc.EC_a<-hclust(distance, method="average")
plot(hc.EC_a, labels=Cluster.Scaled.Const$PC.name)
print(hc.EC_a)
#Comparisons of two methods
#extract members

members_hc.EC.c<-cutree(hc.EC_c,k=3)
members_hc.EC.a<-cutree(hc.EC_a,k=3)

#make table
table(members_hc.EC.c,members_hc.EC.a)

#What variables are playing roles?
aggregate(Cluster.Scaled.Const.withoutID,list(members_hc.EC.c),mean)

#We can repeat the same for Cluster by constituencies

###########
# K-Means Cluster Analysis
fit <- kmeans(Cluster.Scaled.Const.withoutID, 3) # 5 cluster solution
# get cluster means 
aggregate(Cluster.Scaled.Const.withoutID,by=list(fit$cluster),FUN=mean)
# append cluster assignment
scaled_data_withCluster <- data.frame(Cluster.Scaled.Const, fit$cluster)

library(cluster) 
clusplot(Cluster.Scaled.Const.withoutID, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)



# PCA Analysis 
GE_2009_PCA <-subset(GE_2009.Analysis, select=c(7,9,10,13,15:20,22,23,25:29,34,36:38,47,52,55))

cor(GE_2009_PCA)

#PCA
pca.GE_2009 <-princomp(GE_2009_PCA, scores=T,cor=T)
summary(pca.GE_2009)

loadings(pca.GE_2009)

plot(pca.GE_2009)
screeplot(pca.GE_2009,type="l",main="screeplot")

biplot(pca.GE_2009)
pca1<-pca.GE_2009$scores[,1]
pca2<-pca.GE_2009$scores[,2]
pca3<-pca.GE_2009$scores[,3]


GE_2009.Analysis$PCA1<-pca1
GE_2009.Analysis$PCA2<-pca2
GE_2009.Analysis$PCA3<-pca3

# Write the final clean csv with all variables 
write.csv(GE_2009.Analysis,"Final_Clean_GE_2009_5_STATES.csv")
