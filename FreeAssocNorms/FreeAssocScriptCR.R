#Load packages needed
library(ggplot2)
library(corrplot)


#set working directory
setwd("/Users/caitlin/Box Sync/1-Box Folders/Toronto Project/Git/HomonymNorms/FreeAssocNorms")

#Read in data
Biggest = read.csv("./CleanedInput/biggest_values.csv")

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
     eDomNorms$U = NULL
     
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


#Merge in new biggest norms
mergedall <- merge(eDomNorms, Biggest, by = 'word', all.y = TRUE)
mergedclean = mergedall
mergedclean$word = NULL
mergedclean$wordCaseSensitive = NULL
mergedclean$Word_CASE_SENSITIVE = NULL
mergedclean$SouthFloridaAssociation_target = NULL
mergedclean$Word_CASE_SENSITIVE = NULL


#Drop redundant variables
mergedcut <- mergedclean[ -c(1:12,18:21, 24, 30, 32:44, 46, 47, 50:79) ]
mergedcut <- mergedcut[ -c(4:5,14) ]


#Rename variables
#Could break these down by sublexical, lexical, and semantic factors. 
names(mergedcut)[names(mergedcut)=="NumInterpWordNet"] <- "Number of interpretations"
names(mergedcut)[names(mergedcut)=="imag"] <- "Imageability"
names(mergedcut)[names(mergedcut)=="OLD.x"] <- "OLD"
names(mergedcut)[names(mergedcut)=="eDom"] <- "eDom biggest"
names(mergedcut)[names(mergedcut)=="algorithm.FAN"] <- "algorithm.FAN biggest"
names(mergedcut)[names(mergedcut)=="human.FAN"] <- "human.FAN biggest"
names(mergedcut)[names(mergedcut)=="algorithm.SUBTL"] <- "algorithm.SUBTL biggest"
names(mergedcut)[names(mergedcut)=="wikipedia"] <- "Wikipedia biggest"
names(mergedcut)[names(mergedcut)=="nounInterp"] <- "Noun interpretations"
names(mergedcut)[names(mergedcut)=="verbInterp"] <- "Verb interpretations"
names(mergedcut)[names(mergedcut)=="coltN"] <- "Coltheart's N"
names(mergedcut)[names(mergedcut)=="nPhon"] <- "Number of phonemes"
names(mergedcut)[names(mergedcut)=="nSyll"] <- "Number of syllables"
names(mergedcut)[names(mergedcut)=="LgSUBTLWF"] <- "Word frequency"
names(mergedcut)[names(mergedcut)=="Ortho_N"] <- "Orthographic Neighbors"
names(mergedcut)[names(mergedcut)=="BG_Sum"] <- "Bigram frequency"
names(mergedcut)[names(mergedcut)=="NumMeanings"] <- "Number of meanings"
names(mergedcut)[names(mergedcut)=="NumSenses"] <- "Number of senses"




#Reorder column numbers
mergedcut <- mergedcut[c(14,15,16,17,18,1, 2,3, 4, 5, 6, 7, 8, 9, 10, 11,12, 13)]

M = cor(mergedcut)

FM = as.data.frame(M)

M[is.na(FM)] <- 0
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
pdf("./Output/PLT_11_06_17.pdf")
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

pdf("./Output/PLT2_11_06_17.pdf")
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


M = cor(mergedcut,use="pairwise.complete.obs")
p.mat <- cor.mtest(M)

FM = as.data.frame(M)

M[is.na(M)] <- 0
p.mat <- cor.mtest(M)

pdf("./Output/PLT3_11_06_17.pdf")
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







     

