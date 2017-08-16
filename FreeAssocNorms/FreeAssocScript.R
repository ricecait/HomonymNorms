#Load packages needed
#library("ggplot", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

#set working directory
setwd("/Users/caitlin/Box Sync/1-Box Folders/Toronto Project/Git/HomonymNorms/FreeAssocNorms")

#Read in data
FA = read.csv("./free_associates.csv")

#Delete cases for which Meaning_R1 or Meaning_R2 = "-". 
#There were 39 cases where Meaning_R1 = "-" and 84 cases where Meaning_R2 = '-'
FA = FA[FA$Meaning_R1  %in% c("1","2","?1","?2") & FA$Meaning_R2  %in% c("1","2","?1","?2"),]
FA = droplevels(FA)

#Convert Meaning_R1 and Meaning_R2 to character vectors of the concatenated values
FA$categ = as.factor(paste(FA$Meaning_R1,FA$Meaning_R2, sep = "_"))
FA$dum = 1;

#
n = nrow(FA)
su = aggregate(dum ~ categ, data = FA, FUN=length)
su$dum = su$dum/n*100

ggplot(su, aes(x=categ,y=dum))+geom_bar(stat="identity")

su$gr = "Disagreement";

su[su$categ == "1_1" | su$categ == "2_2",]$gr = "Certain Agreement";
su[su$categ == "1_?1" | su$categ == "2_?2" | 
        su$categ == "?1_1" | su$categ == "?2_2" | 
        su$categ == "?1_?1" | su$categ == "?2_?2",]$gr = "Uncertain Agreement";

#override very low representative levels

su$s_categ = "All"
su[su$categ == "1_1" | su$categ == "2_2",]$s_categ = su[su$categ == "1_1" | su$categ == "2_2",]$categ

ggplot(su, aes(x=s_categ,y=dum))+geom_bar(stat="identity", width = 1) + facet_grid(~gr,scale="free_x")


#Caitlin's work

#Proportion  agreement, disagreement, certain agreement, certain disagreement
summary(FA$categ)
nrow(FA)

su$gr2 = "Disagreement";
su[su$categ == "1_1" | su$categ == "2_2",]$gr2 = "CA";
su[su$categ == "1_?1" | su$categ == "2_?2" | 
        su$categ == "?1_1" | su$categ == "?2_2" | 
        su$categ == "?1_?1" | su$categ == "?2_?2",]$gr2 = "UA";
su[su$categ == "1_2" | su$categ == "2_1",]$gr2 = "CD";
su[su$categ == "?1_?2" | su$categ == "?1_2" | su$categ == "?2_?1" |
        su$categ == "?2_1" | su$categ == "1_?2" | 
        su$categ == "2_?1",]$gr2 = "UD";

xtabs(dum~gr, data=su)

#ggplot(su, aes(x=s_categ,y=dum))+geom_bar(stat="identity", width = 1) + facet_grid(~gr2,scale="free_x")
     #This still has panels

ggplot(su, aes(x=gr2,y=dum))+geom_bar(stat="identity", width = .9)

