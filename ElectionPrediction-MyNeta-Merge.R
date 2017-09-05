# R code to merge election commission data along with Religion and Literacy data  with MyNeta data 

library(dplyr)
library(data.table)

setwd( "C:/Users/user/Documents/GL-Classes/7/grp-assignment")
#EC_2009_UP_Clean <-read.csv("EC.Final.UP.2009_Clean.csv", header=T)

EC_2009_ALL_Clean <-read.csv("EC.Final.ALL.2009_Clean_WithKey.csv", header=T)

MyNeta_2009_all <- read.csv("LokSabha2009_Merged_file_WithKey.csv", header=T)

# Filter out candidates with Independent party
EC_2009_ALL_Clean <- filter(EC_2009_ALL_Clean,Party.Abbreviation!='IND')
MyNeta_2009_all <- filter(MyNeta_2009_all,Party!='IND')

write.csv(EC_2009_ALL_Clean,"EC.Final.ALL.2009_Clean_NO_IND.csv")
write.csv(MyNeta_2009_all,"MyNeta2009_MergedFile_Clean_No_IND.csv")

distance.methods<-c('jw')
dist.methods<-list()

for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(MyNeta_2009_all$Key),nrow = length(EC_2009_ALL_Clean$Key))
  for(i in 1:length(MyNeta_2009_all$Key)) {
    for(j in 1:length(EC_2009_ALL_Clean$Key)) { 
      dist.name.enh[j,i]<-stringdist(tolower(MyNeta_2009_all[i,]$Key),tolower(EC_2009_ALL_Clean[j,]$Key),method = distance.methods[m])      
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
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=MyNeta_2009_all[s2.i,]$Key, s1name=EC_2009_ALL_Clean[s1.i,]$Key, adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}

matched.names.matrix.1<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.1)

write.csv(matched.names.matrix.1,"matched.Literacy.matrix.TN.csv")


#EC_2009_UP_Clean <- EC_2009_UP_Clean[,-c(1)]
EC_2009_ALL_Clean <- EC_2009_ALL_Clean[,-c(1)]
MyNeta_2009_all <- MyNeta_2009_all[,-c(1)]
# Create common fields

EC_2009_ALL_Clean <- plyr::rename(EC_2009_ALL_Clean, c(PC.name="Constituency"))
EC_2009_ALL_Clean <- plyr::rename(EC_2009_ALL_Clean, c(Candidate.Name="Candidate"))
EC_2009_ALL_Clean <- plyr::rename(EC_2009_ALL_Clean, c(Party.Abbreviation="Party"))

MyNeta_2009_all <- plyr::rename(MyNeta_2009_all, c(Candidates="Candidate"))


EC_MyNeta_Merged <-merge(EC_2009_ALL_Clean,MyNeta_2009_all,by=c("Candidate","Constituency", "Party"), all=TRUE)

EC_MyNeta_Merged_ALL <- filter(EC_MyNeta_Merged,State.name != 'NA')

write.csv(EC_MyNeta_Merged_ALL,"EC_MyNeta_Merged_Final_ALL.csv")
