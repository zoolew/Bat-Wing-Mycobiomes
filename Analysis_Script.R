#PERMANOVA

#load data, including bats with no cultured fungi

final0<-read.csv("data.csv",header=T)

#subset to remove metadata (matrix only)

final0m<-final0 [,13:73]

#run PERMANOVA

perm1 = adonis(final0m~Bat_Species+Site, data=final0, perm=999)

perm2 = adonis(final0m~Site+Bat_Species, data=final0, perm=999)

#Model for all fungi abundance

#load data

final<-read.csv("data.csv",header=T)

# subset to remove metadata (matrix only)

finalm<-final [,14:74]

#determine best transformation

transformTukey(final$all_fungi) 

#lambda=0.2, transform data

final$all_fungi <- (final$all_fungi^0.2)

#run zero-inflated model to determine how fungal abundance varies among WNS-susceptibility #groups while controlling for bat species and geographic site

zero_fungi <- glmmTMB(all_fungi ~ Group + (1| Bat_Species) + (1| Site), data=final, 

                                            ziformula=~ 1, family=gaussian) 

#get results from model

summary(zero_fungi)

##Model for yeast abundance

#load data

final<-read.csv("data.csv",header=T)

# subset to remove metadata (matrix only)

finalm<-final [,14:74]

#determine best transformation

transformTukey(final$yeastab)

#lambda 0.2, transform data

final$yeastab <- (final$yeastab^0.2)

#run zero-inflated model to determine how yeast abundance varies among WNS-susceptibility #groups while controlling for bat species and geographic site

zero_yeast <- glmmTMB(yeastab ~ Group + (1| Bat_Species) + (1| Site), data=final, 

                                            ziformula= ~ Group + (1| Bat_Species) + (1| Site), family=gaussian)
#get results from model

summary(zero_yeast)

#Shannon all fungi

#load data

final<-read.csv("data.csv",header=T)

#subset to remove metadata (matrix only)

finalm <- final [,14:74]

#calculate Shannon Index

shan=diversity(finalm, index="shan")

#determine best transformation

transformTukey(shan)

#lambda=0.5, transform data

shan <- (shan^0.5)

#run zero-inflated model to determine how the Shannon Diversity Index for all fungi varies #among WNS-susceptibility groups while controlling for bat species and geographic site

zero_shan6 <- glmmTMB(shan ~ Group+(1|Bat_Species) +(1|Site), data=final, 

                      ziformula=~1, family=gaussian)

#get results from best model

summary(zero_shan6)

##Shannon Diversity Index, yeast only

#load data

yeastonly<-read.csv("data.csv",header=T)

# subset to remove metadata (matrix only)

yeastonlym<-yeastonly [,14:35]

#calculate Shannon Index

shanY=diversity(yeastonlym, index="shan")

#determine best transformation

transformTukey(shanY) 

#lambda=0.325, transform data

shanY <- (shanY^0.325)

#run zero-inflated model to determine how the Shannon Diversity Index for yeasts only varies #among WNS-susceptibility groups while controlling for bat species and geographic site

shanY12 <- glmmTMB(shanY ~ Group+(1|Bat_Species)+(1|Site), data=yeastonly, 

                   ziformula= ~1, family=gaussian)

#get results from model

summary(shanY12)



##Deseq differential abundance analysis

glimpse(finalm)

row.names(finalm) <- final$ID

temp <- as.matrix(finalm)

temp <- t(temp)

counts <- temp

meta <- final[,1:10]

# Make column data file

coldata <- meta[,c(1:10)]

# Make row names of col data Bat ID from whole dataset

rownames(coldata) <- final$ID

# Do sample order in coldata and count matrix match?

all(rownames(coldata) == colnames(counts))

dds <- DESeqDataSetFromMatrix(countData = counts,

                              colData = coldata, design = ~ Group)

# compare groups in pairwise comparisons

dds$Group <- factor(dds$Group, levels = c("Susceptible","Tolerant"))

dds$Group <- factor(dds$Group, levels = c("Susceptible","Resistant"))

dds$Group <- factor(dds$Group, levels = c("Resistant","Tolerant"))

sizeFactors(dds) <- calcNormFactors(counts(dds), method = "none")

dds <- DESeq(dds)

res <- results(dds)

resOrdered <- res[order(res$padj),]

#write out results

write.table(resOrdered, "C:/Users/karen/Desktop/try1.txt", sep="\t")

#indicator species analysis
mult <- multipatt(finalm, final$Group, "IndVal.g")

mult.all <- mult$sign

mult.all.pvals <- mult.all[,"p.value"]

#adjust p-value for multiple comparisons

fdr.p.value <- p.adjust(mult.all.pvals, method="fdr")

mult.all.fdr <- cbind(mult.all, fdr.p.value)

attach(mult.all.fdr) ## allows sorting by header

mult.all.fdr.nona.sort <- mult.all.fdr[order(fdr.p.value, p.value, na.last=NA),] ## sort output by fdr, then p-value, omit "NA"

mult.all.fdr.nona.sort


##Determining if Pd abundance affects abundance of skin fungal assemblages

#load data

Pdfinal<-read.csv("data.csv",header=T)

# subset to remove metadata (matrix only)

Pdm<-Pdfinal [,9:73] 

#determine best data transformation for explanatory variable

transformTukey(Pdfinal$Pd) 

#lambda = 0.225, transform Pd abundance

Pdfinal$Pd <- (Pdfinal$Pd^.225)

#determine best data transformation for response variable

transformTukey(Pdfinal$all_fungi)

#lambda 0.175, transform fungal abundance

Pdfinal$all_fungi <- (Pdfinal$all_fungi^.175)

# run zero-inflated model to determine if the abundance of Pd on a bat affects the abundance of other fungi while controlling for bat species and site

zero_Pda <- glmmTMB(all_fungi ~ Pd + (1| Bat_Species) + (1| Site), data=Pdfinal, 
                    ziformula= ~ Pd + (1| Bat_Species) + (1| Site), family= truncated_nbinom1)

#get results for model
summary(zero_Pda)

##Determining if Pd abundance affects Shannon Diversity Index of skin fungal assemblages

#load data

Pdfinal<-read.csv("data.csv",header=T)

# subset to remove metadata (matrix only)

Pdm<-Pdfinal [,9:73]

#calculate Shannon Diversity Index

shanP=diversity(Pdm, index="shan")

#determine best transformation for Shannon Index

transformTukey(shanP)

#lambda = 0.4, transform data

shanP <- (shanP^0.4)

#determine best data transformation for explanatory variable

transformTukey(Pdfinal$Pd)

#lambda = 0.225, transform Pd abundance

Pdfinal$Pd <- (Pdfinal$Pd^0.225)

#run zero-inflated model to determine if the abundance of Pd on a bat affects the Shannon #Diversity Index for all fungi while controlling for bat species and site

Pds8 <- glmmTMB(shanP ~ Pd + (1|Bat_Species)+(1|Site), data=Pdfinal, 

                                ziformula= ~1, family=gaussian)
#get results for model

summary(Pds8)

##Determining if the abundance of Pd on a bat affects the composition of skin fungal #assemblages

#load data

Pd0<-read.csv("data.csv",header=T)

# subset to remove metadata (matrix only)

Pd0m<-Pd0 [,10:74] 

#run PERMANOVA

perm1 = adonis(Pd0m~Pd+Bat_Species+Site, data=Pd0, perm=999)

perm2 = adonis(Pd0m~Bat_Species+Site+Pd, data=Pd0, perm=999)

perm3 = adonis(Pd0m~Site+Pd+Bat_Species, data=Pd0, perm=999)

#Comparing the skin fungal assemblages on Myotis lucifugus sampled in New York to #populations sampled farther west

#load data

MYLU<-read.csv("MYLU2.csv",header=T)

# subset to remove metadata (matrix only)

MYLUm<-MYLU [,10:29]

#calculate Shannon Diversity Index

shan=diversity(MYLUm, index="shan")

#determine best data transformation for fungal abundance

transformTukey(final$all_fungi)

#transform data

final$all_fungi <- (final$all_fungi^0.2)

#run zero-inflated model to determine how the Shannon Diversity Index for all fungi varies #between WNS-susceptibility groups while controlling for geographic site

sh3 <- glmmTMB(shan ~ Group +(1|Site), data=MYLU, 
               ziformula= ~1, family=gaussian)

#get results from model

summary(sh3)

#run zero-inflated model to determine how fungi abundance varies among WNS-susceptibility #groups while controlling for geographic site

yea3 <- glmmTMB(fungiCFU ~ Group +(1|Site), data=MYLU, 
                ziformula= ~1, family=truncated_nbinom2)
#get results from model

summary(yea3)

#run zero-inflated model to determine how yeast abundance varies among WNS-susceptibility #groups while controlling for geographic site

yea3 <- glmmTMB(yeastCFU ~ Group +(1|Site), data=MYLU, 
                ziformula= ~1, family=truncated_nbinom2)

#get results from model

summary(yea3)

