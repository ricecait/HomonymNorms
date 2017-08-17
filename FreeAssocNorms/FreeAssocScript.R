#Load packages needed
#install.packages("corrplot")

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
su[su$categ == "1_1",]$gr2 = "CA 1";
su[su$categ == "2_2",]$gr2 = "CA 2"
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
ggsave("Figure1.pdf")

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


all$biggest = all$max/all$tot

hist(all$biggest)
nrow(all)
#Import eDom norms
eDomNorms <- read.csv("./eDom_norms.csv")
eDomNorms$imag = as.numeric(eDomNorms$imag)

#merge in to file
merged <- merge(eDomNorms, all, by.x = "word", by.y = "probe")

#Correlation between eDom norms and FA norms
     #Note: might want to do Spearman or nonparametric correlation.
     #Violation of normality. Check assumptions of Spearman
     #Need to carefully consider how to deal with ties
cor(merged$biggest.x, merged$biggest.y, method = "spearman")
no_ones <- merged[merged$biggest.y != 1, ]
cor(no_ones$biggest.x, no_ones$biggest.y, method = "spearman")
cor(no_ones$biggest.x, no_ones$biggest.y)



mergedclean = merged
mergedclean$word = NULL
mergedclean$wordCaseSensitive = NULL



M = cor(mergedclean)

M[is.na(M)] <- 0
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "number", type = "upper")

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

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         #addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)









