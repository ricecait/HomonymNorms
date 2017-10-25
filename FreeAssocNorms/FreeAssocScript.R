#Load packages needed
library(ggplot2)
library(corrplot)

#set working directory
setwd("/Users/caitlin/Box Sync/1-Box Folders/Toronto Project/Git/HomonymNorms/FreeAssocNorms")

#Read in data
#FA = read.csv("./CleanedInput/free_associates.csv")
FA = read.csv("./CleanedInput/FAN Norms for analyses 10_22_17.csv")

#Delete cases for which Meaning_R1 or Meaning_R2 or Meaning_R23 = "-". 
#There were 39 cases where Meaning_R1 = "-",  84 cases where Meaning_R2 = '-', and X cases where Meaning_R3 = '-'
#FA = FA[FA$Meaning_R1  %in% c("1","2","?1","?2") & FA$Meaning_R2  %in% c("1","2","?1","?2") & FA$Meaning_R3  %in% c("1","2","?1","?2"),]
#FA = droplevels(FA)

#Delete cases for which Meaning_R1 or Meaning_R2 or Meaning_R23 = "3 | 4". 
FA = FA[FA$Meaning_R1  %in% c("1","2","?1","?2", "-") & FA$Meaning_R2  %in% c("1","2","?1","?2", "-") & FA$Meaning_R3  %in% c("1","2","?1","?2", "-"),]
FA = droplevels(FA)

#Convert Meaning_R1 and Meaning_R2 to character vectors of the concatenated values
FA$categ = as.factor(paste(FA$Meaning_R1,FA$Meaning_R2, FA$Meaning_R3, sep = "_"))
FA$dum = 1;

#
n = nrow(FA)
su = aggregate(dum ~ categ, data = FA, FUN=length)
su$dum = su$dum/n*100

ggplot(su, aes(x=categ,y=dum))+geom_bar(stat="identity")


#Make plot for two raters
#su$gr = "Disagreement";
#su[su$categ == "1_1_1" | su$categ == "2_2_2",]$gr = "Certain Agreement";
#su[su$categ == "?1_?1_?1" | su$categ == "?1_?1_1" | 
        #su$categ == "?1_1_?1" | su$categ == "?1_1_1" | su$categ == "?2_2_2" | su$categ == "1_1_?1" | su$categ == "2_?2_?2" | su$categ == "2_?2_2"| su$categ == "2_2_?2",]$gr = "Uncertain Agreement";						
#override very low representative levels
#su$s_categ = "All"
#su[su$categ == "1_1" | su$categ == "2_2",]$s_categ = su[su$categ == "1_1" | su$categ == "2_2",]$categ
#ggplot(su, aes(x=s_categ,y=dum))+geom_bar(stat="identity", width = 1) + facet_grid(~gr,scale="free_x")


#Proportion  agreement, disagreement, certain agreement, certain disagreement
summary(FA$categ)
nrow(FA)

#Make plot for three raters
su$gr2 = "Other";
su[su$categ == "1_1_1",]$gr2 = "CA 1";
su[su$categ == "2_2_2",]$gr2 = "CA 2"
#su[su$categ == "?1_?1_?1" | su$categ == "?1_?1_1" | 
        #su$categ == "?1_1_?1" | su$categ == "?1_1_1" | 
        #su$categ == "?2_2_2" | su$categ == "1_1_?1" | 
        #su$categ == "2_?2_?2" | su$categ == "2_?2_2"| su$categ == "2_2_?2",]$gr2 = "UA";
su[su$categ == "2_1_1" | su$categ == "2_1_2" | su$categ == "1_1_2" | su$categ == "1_2_1" | su$categ == "1_2_2" | su$categ == "2_2_1",]$gr2 = "CD"; 				
#su[su$categ == "?2_1_2" | su$categ == "?2_2_1" | su$categ == "1_2_?1" |su$categ == "2_1_?1" | su$categ == "2_2_?1" | su$categ == "?1_1_2"| su$categ == "?1_2_?1"| 
        #su$categ == "?1_2_1" | su$categ == "?1_2_2" | su$categ == "?2_?1_1" |su$categ == "?2_1_1" | su$categ == "1_?1_?1" | su$categ == "1_?1_1"| su$categ == "1_?1_2"|
        #su$categ == "1_?2_?2" | su$categ == "1_?2_1" | su$categ == "1_1_?2" |su$categ == "1_2_?2" | su$categ == "2_?2_1" | su$categ == "2_?1_?1"| su$categ == "2_?1_1",]$gr2 = "UD";
#su[su$categ == "-_-_-",]$gr2 = "OtherMeaning_A"   
#su[su$categ == "?2_-_2" |su$categ == "?1_-_1" |su$categ == "2_-_2" |su$categ == "1_1_-" |su$categ == "?2_2_-" |su$categ == "?2_2_2" |su$categ == "?1_2_-" |su$categ == "1_-_-" 
   #|su$categ == "1_-_1" |su$categ == "1_-_2" |su$categ == "1_?1_-" |su$categ == "2_2_-" |su$categ == "?1_?1_-" |su$categ == "?2_1_-" |su$categ == "-_1_1" ,]$gr2 = "PartDom_A"

     												
xtabs(dum~gr2, data=su)

#ggplot(su, aes(x=s_categ,y=dum))+geom_bar(stat="identity", width = 1) + facet_grid(~gr2,scale="free_x")
     #This still has panels

plot1 <- ggplot(su, aes(x=gr2,y=dum))+geom_bar(stat="identity", width = .95)
plot1 + labs(x="Agreement type \n \n Figure 1. Proportion rater agreement by type", y="Proportion") 
ggsave("./Output/AgreementFigure.tiff")


###END PLOT SECTION###

#Make copy of database
trend <- FA[FA$categ == "1_1" | FA$categ == "2_2", ]
#nr = nrow(trend)
tot = aggregate(dum ~ probe, data = trend, FUN = length)
M1 = aggregate(dum ~ probe, data = trend[trend$categ == '1_1', ], FUN = length)
M2 = aggregate(dum ~ probe, data = trend[trend$categ == '2_2', ], FUN = length)

colnames(tot)[2] = 'tot'
colnames(M1)[2] = "M1"
colnames(M2)[2] = "M2"

all = merge(tot, M1, by = 'probe', all.x = TRUE)
all = merge (all, M2, by = 'probe', all.x = TRUE)

all[is.na(all$M1), ]$M1 = 0
all[is.na(all$M2), ]$M2 = 0

all$max = pmax(all[,3], all[,4]) 


all$biggestFAN = all$max/all$tot

hist(all$biggestFAN)
nrow(all)
#Import files

     #Check OLD -  not identical beween files, need to manually reinspect

eDomNorms <- read.csv("./CleanedInput/eDom_norms.csv")
eDomNorms$imag = as.numeric(eDomNorms$imag)
     eDomNorms$sp1p2 = NULL
     eDomNorms$sp1p3 = NULL  
     eDomNorms$sp1p4 = NULL 
     eDomNorms$sp1p5 = NULL
     eDomNorms$sp1p6 = NULL
     eDomNorms$dictsp1p2 = NULL
     eDomNorms$dictsp1p3 = NULL  
     eDomNorms$dictsp1p4 = NULL 
     eDomNorms$dictsp1p5 = NULL
     eDomNorms$dictsp1p6 = NULL
     eDomNorms$NumSenses = NULL
     eDomNorms$NumMeanings = NULL
     #eDomNorms$U = NULL
     
eLex <- read.table("./CleanedInput/elexiconData.subsetsOfAmbiguous.txt", sep = "\t", header = TRUE)
     eLex$biggest = NULL
     eLex$dominance = NULL
#TwilleyUnion <- read.table("./CleanedInput/elexiconData.Union.Twilley.subsetsOfAmbiguous.txt", sep = "\t", header = TRUE)
     #TwilleyUnion$ACC = NULL
     #TwilleyUnion$RT = NULL
     #TwilleyUnion$signTest = NULL
     #TwilleyUnion$dominance = NULL
     #TwilleyUnion$biggest = NULL
     #TwilleyUnion$Twilleyp1 = NULL
     #TwilleyUnion$Twilleyp2 = NULL
     #TwilleyUnion$Twilleyp3 = NULL
     #TwilleyUnion$Obs = NULL
     #TwilleyUnion$Length = NULL
     


Descriptives <- read.table("./CleanedInput/itemData.ALL590ambiguous.txt", sep = "\t", header = TRUE)
     Descriptives$MRC_Imag.f <- as.numeric(Descriptives$MRC_Imag.f) 
     Descriptives$SingleWordOrPhrase = NULL
     Descriptives$NONE_EMPTYLINEINOTHERFILE = NULL
Twilley <-  read.table("./CleanedInput/itemMeans.dominance.Twilley.subsetsOfAmbiguous.txt", sep = "\t", header = TRUE)
     Twilley$dominance = NULL
     Twilley$biggest = NULL
     Twilley$signTest = NULL

#Merge files. NOTE: will need to go back through this and check to see what are exact replicates. Correlation = 1 then NULL.
eDomNorms <- merge(eDomNorms, eLex, by.x = "word", by.y = "Word", all.x = TRUE)
#eDomNorms <- merge(eDomNorms, TwilleyUnion, by.x = "word", by.y = "Word", all.x = TRUE)
eDomNorms <- merge(eDomNorms, Descriptives, by.x = "word", by.y = "word_LOWERCASE", all.x = TRUE)
eDomNorms <- merge(eDomNorms, Twilley, by.x = "word", by.y = "word", all.x = TRUE)




     #Merge in 4 new files into EDom norms, keep all.x although they should overlap except for the Twilley norms which are a subset
          #Rerun everything, check to see if correlates with ELP

#merge in to file
merged <- merge(eDomNorms, all, by.x = "word", by.y = "probe", all.x = TRUE)

#Correlation between eDom norms and FA norms
     #Note: might want to do Spearman or nonparametric correlation.
     #Violation of normality. Check assumptions of Spearman
     #Need to carefully consider how to deal with ties
cor(merged$biggest, merged$biggestFAN, method = "spearman")
no_ones <- merged[merged$biggest.y != 1, ]
cor(no_ones$biggest, no_ones$biggestFAN, method = "spearman")
cor(no_ones$biggest, no_ones$biggestFAN)



mergedclean = merged
mergedclean$word = NULL
mergedclean$wordCaseSensitive = NULL
mergedclean$Word_CASE_SENSITIVE = NULL
mergedclean$SouthFloridaAssociation_target = NULL
mergedclean$Word_CASE_SENSITIVE = NULL


M = cor(mergedclean)

FM = as.data.frame(M)

M[is.na(M)] <- 0
#col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#corrplot(M, method = "number", type = "upper")

#From: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
     mat <- as.matrix(mat)
     n <- ncol(mat)
     p.mat<- matrix(NA, n, n)
     diag(p.mat) <- 0
     for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
               tmp <- cor.test(mat[, i], mat[, j], ...)
               p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
          }
     }
     colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
     p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(M)
head(p.mat[, 1:5])

col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF","#EE9988" , "#BB4444"))
pdf("./Output/PLT.pdf")
PLT1 <- corrplot(M, method="color", col=col(400),  
         type="upper", order="hclust", 
         #addCoef.col = "black", # Add coefficient of correlation
         tl.cex = .5, tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
dev.off()

pdf("./Output/PLT2.pdf")
PLT2 <- corrplot(M, method="color", col=col(400),  
                type="upper", order="hclust", 
                #addCoef.col = "black", # Add coefficient of correlation
                tl.cex = .5, tl.col="black", tl.srt=45, #Text label color and rotation
                # Combine with significance
                p.mat = p.mat, sig.level = 0.15, insig = "blank", 
                # hide correlation coefficient on the principal diagonal
                diag=FALSE 
)
dev.off()

#####



#check what happens when do all cors that have data


M = cor(mergedclean,use="pairwise.complete.obs")
p.mat <- cor.mtest(M)

FM = as.data.frame(M)

M[is.na(M)] <- 0
p.mat <- cor.mtest(M)

pdf("./Output/PLT3.pdf")
PLT3 <- corrplot(M, method="color", col=col(400),  
                 type="upper", order="hclust", 
                 #addCoef.col = "black", # Add coefficient of correlation
                 tl.cex = .5, tl.col="black", tl.srt=45, #Text label color and rotation
                 # Combine with significance
                 p.mat = p.mat, sig.level = 0.05, insig = "blank", 
                 # hide correlation coefficient on the principal diagonal
                 diag=FALSE 
)
dev.off()


a=rownames(FM)
b = as.numeric(FM$ACC)
c = cbind(a,b)
#--> shows that accuracy is correlated with biggest, not U or biggestFAN.  cor is in wroong dir, however

a=rownames(FM)
b = as.numeric(FM$RT)
c = cbind(a,b)
#---> shows strongest cor is U, followed by biggestFAN, then biggest (which were comparable, but
#opposite directions

#Note: there is a conflict that needs to be resolved with OLD - there are variations. 
acc.lm = lm(ACC ~Length + biggest +LgSUBTLWF + OLD.x + BG_Sum + NPhon + NSyll + NumSenses + transitive_verb + intransitive_verb + noun, data = merged)
print(summary(acc.lm))

rt.lm = lm(RT ~Length + biggest +LgSUBTLWF + OLD.x + BG_Sum + NPhon + NSyll + NumSenses + transitive_verb + intransitive_verb + noun, data = merged)
print(summary(rt.lm))


#Drop all observations from merged that have more than 10 senses
nohybrids <- merged[merged$NumSenses < 10, ]


acc.lm = lm(ACC ~Length + biggest +LgSUBTLWF + OLD.x + BG_Sum + NPhon + NSyll + NumSenses + transitive_verb + intransitive_verb + noun, data = nohybrids)
print(summary(acc.lm))

rt.lm = lm(RT ~Length + biggest +LgSUBTLWF + OLD.x + BG_Sum + NPhon + NSyll + NumSenses + transitive_verb + intransitive_verb + noun, data = nohybrids)
print(summary(rt.lm))

#Substitute biggestFAN
acc.lm = lm(ACC ~Length + biggestFAN +LgSUBTLWF + OLD.x + BG_Sum + NPhon + NSyll + NumSenses + transitive_verb + intransitive_verb + noun, data = nohybrids)
print(summary(acc.lm))

rt.lm = lm(RT ~Length + biggestFAN +LgSUBTLWF + OLD.x + BG_Sum + NPhon + NSyll + NumSenses + transitive_verb + intransitive_verb + noun, data = nohybrids)
print(summary(rt.lm))

#Substitute biggestFAN
acc.lm = lm(ACC ~Length + U +LgSUBTLWF + OLD.x + BG_Sum + NPhon + NSyll + NumSenses + transitive_verb + intransitive_verb + noun, data = nohybrids)
print(summary(acc.lm))

rt.lm = lm(RT ~Length + U +LgSUBTLWF + OLD.x + BG_Sum + NPhon + NSyll + NumSenses + transitive_verb + intransitive_verb + noun, data = nohybrids)
print(summary(rt.lm))

cor.test(merged$biggestFAN, merged$biggest)
cor.test(merged$biggestFAN, merged$U)
cor.test(merged$U, merged$biggest)

#when returning to analyses, need to run simple correlations between 3 variables above for all dataset (max obs) vs union of three sets
     #use complete.obs, not pairwise.complete.obs



     

