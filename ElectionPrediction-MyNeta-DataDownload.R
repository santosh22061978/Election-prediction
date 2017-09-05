#I have used webscrapping process to extract data from web pages using R and presenting into table form
#install.packages("XML")
library(XML)

#Extract data of 2009 candidates first
link<-"http://www.myneta.info/ls2009/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary"
#Read the HTML table from the url, "which" specifies the table number on the web page.
candidates<-readHTMLTable(link,which=3)
str(candidates)
#By this way R converts all columns into factors
View(candidates)
# We remove first two rows and column 1 (SNo.) and column 8 (Total Liabilites) as it not reqd in our analysis
nrow(candidates)
candi<-candidates[3:7671,2:7]
#We now convert the data to a table
#install.packages("data.table")
library(data.table)
all_candidates<-as.data.table(candi)
View(all_candidates)
#We clean Total Assets Column
colnames(all_candidates)
all_candidates$`Total Assets`<-as.character(all_candidates$`Total Assets`)
#We replace 'Nil' value with 0 and commas with space
for(i in 1:nrow(all_candidates)){
  if(all_candidates$`Total Assets`[i]=="Nil"){
    all_candidates$`Total Assets`[i]=0
  }
  else{
    str<-all_candidates$`Total Assets`[i]
    cleanstr<-gsub(",","",substr(str,start=4,stop=regexpr("~",str)-2))
    all_candidates$`Total Assets`[i]<-cleanstr
  }
}
#Assets value is converted to numeric
all_candidates$`Total Assets`<-as.numeric(all_candidates$`Total Assets`)
View(all_candidates)
#We create csv file of exported data
setwd("E:/Abhijit/Study/Great Lakes/Predictive Modelling/Group Assignment/PM_Assignment/PM_Assignment_Grp13")
write.csv(all_candidates,file="2009_AllCandidates.csv")

##Serious crimes data is merged through following code
serious_crimes_2009<-"http://www.myneta.info/ls2009/index.php?action=summary&subAction=serious_crime&sort=candidate#summary"
serious_crimes_table<-readHTMLTable(serious_crimes_2009,which=3)
View(serious_crimes_table)
#We keep the required rows and columns and remove the rest
nrow(serious_crimes_table)
candsercrime<-serious_crimes_table[3:606,2:4]
candsercrimetable<-as.data.table(candsercrime)
#For serious crime, we write ' 1' 
candsercrimetable$SeriousCrime<- 1
View(candsercrimetable)
#Generate csv file of this data
write.csv(candsercrimetable,file="SeriousCrimeData.csv")

#We merge the two files
colnames(all_candidates)
all_candidates$Candidates <- all_candidates$`Candidateâ^???`
all_candidates$Constituency <- all_candidates$`Constituency`
all_candidates$Party <- all_candidates$`Party `
colnames(candsercrimetable)
candsercrimetable$Candidates <- candsercrimetable$`Candidateâ^???`
candsercrimetable$Constituency <- candsercrimetable$`Constituency `
candsercrimetable$Party <- candsercrimetable$`Party `
#Merging the two together
MergedCrimeData <- merge(all_candidates, candsercrimetable, by=c("Candidates","Constituency","Party"), all=TRUE)
View(MergedCrimeData)
#Cleaning the data to get only unique columns
MergedCrimeData2<-subset(MergedCrimeData, select=c(1:3,7:9,13))
#Replace the NA's with 0
MergedCrimeData2$SeriousCrime[is.na(MergedCrimeData2$SeriousCrime)] <- 0
View(MergedCrimeData2)
#Export the data to csv file
write.csv(MergedCrimeData2,file="MergedCrimeData.csv")

#We now merge women candidate data
women_url<-"http://www.myneta.info/ls2009/index.php?action=summary&subAction=women_candidate&sort=candidate#summary"
#Read the HTML table from the url
women_table<-readHTMLTable(women_url,which=3)
View(women_table)
#We now take only constituency, candidate and party names
nrow(women_table)
womencandidate<-women_table[3:521,2:4]
womencandidatetable<-as.data.table(womencandidate)
#We include a flag 1 to say serious crime
womencandidatetable$Women<- 1
View(womencandidatetable)
#Exporting the women candidate data to csv file
write.csv(womencandidatetable,file="WomenData.csv")

#Merging the data
colnames(womencandidatetable)
womencandidatetable$Candidates <- womencandidatetable$`Candidateâ^???`
womencandidatetable$Constituency <- womencandidatetable$`Constituency `
womencandidatetable$Party <- womencandidatetable$`Party `
#Merging the data
Women_Merged <- merge(MergedCrimeData2, womencandidatetable, by=c("Candidates","Constituency","Party"), all=TRUE)
View(Women_Merged)
#Cleaning the data to get only unique columns
Women_Merged_Table<-subset(Women_Merged, select=c(1:7,11))
#Replace the NA's with 0
Women_Merged_Table$Women[is.na(Women_Merged_Table$Women)] <- 0
View(Women_Merged_Table)
#Exporting the merged data to csv file
write.csv(Women_Merged_Table,file="MergedData_CrimeWomen.csv")

#We now merge winners data
winner_cand<-"http://www.myneta.info/ls2009/index.php?action=summary&subAction=winner_analyzed&sort=candidate#summary"
winner_cand_table<-readHTMLTable(winner_cand,which=3)
View(winner_cand_table)
#We remove first two rows and keep only candidate, constituency and party names
nrow(winner_cand_table)
womencandidate<-winner_cand_table[3:522,2:4]
winner_cand_table2<-as.data.table(womencandidate)
#We include a flag 1 to say serious crime
winner_cand_table2$Winner<- 1
View(winner_cand_table2)
#Exporting the serious crime data to csv file
write.csv(winner_cand_table2,file="Winners_of_2009.csv")

#We Merge the candidates data with winner data
colnames(winner_cand_table2)
winner_cand_table2$Candidates <- winner_cand_table2$`Candidateâ^???`
winner_cand_table2$Constituency <- winner_cand_table2$`Constituency `
winner_cand_table2$Party <- winner_cand_table2$`Party `
#Merging the two data together
Winner_Merged_data <- merge(Women_Merged_Table, winner_cand_table2, by=c("Candidates","Constituency","Party"), all=TRUE)
View(Winner_Merged_data)
#Cleaning the data to get only unique columns
Winner_Merged_data_2<-subset(Winner_Merged_data, select=c(1:8,12))
#We replace the NA's with 0 for analysis
Winner_Merged_data_2$Winner[is.na(Winner_Merged_data_2$Winner)] <- 0
View(Winner_Merged_data_2)
#We export the final merged data in to csv file
write.csv(Winner_Merged_data_2,file="LokSabha2009_Merged_file.csv")