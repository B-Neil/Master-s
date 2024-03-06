BiocManager::install(c("edgeR"))
BiocManager::install(c("Glimma"))
BiocManager::install(c("gplots"))
BiocManager::install(c("org.Mm.eg.db"))

library(edgeR)
library(limma)
library(Glimma)
library(gplots)
library(org.Mm.eg.db)
library(RColorBrewer)

sampleinfo <- read.delim("./MouseData/SampleInfo_corrected.txt", stringsAsFactors = TRUE)
sampleinfo
seqdata <- read.delim("./MouseData/GSE60450_Lactation-GenewiseCounts.txt", stringsAsFactors = FALSE)
head(seqdata)
dim(seqdata)

# remove first two columns
countdata <- seqdata[,-(1:2)]

# Store EntrezGeneID as rownames
rownames(countdata) <- seqdata[,1]

head(countdata)
colnames(countdata)

colnames(countdata) <- substr(colnames(countdata), 1, 7)
head(countdata)
table(colnames(countdata)==sampleinfo$SampleName)

# calculate counts per million
myCPM <- cpm(countdata)
head(myCPM)

# Which values in myCPM are greater than 0.5 in at least two samples
thresh <- myCPM > 0.5
keep <- rowSums(thresh) >= 2
counts.keep <- countdata[keep,]
summary(keep)
dim(counts.keep)

# create DGEList object
dgeObj <- DGEList(counts.keep)
dgeObj
names(dgeObj)
dgeObj$samples

## distributions - log transform
logcounts <- cpm(dgeObj,log=TRUE)
boxplot(logcounts, xlab="", ylab="Log2 counts per million",las=2)
abline(h=median(logcounts),col="blue")
title("Boxplots of logCPMs (unnormalised)")

## heatmaps
var_genes <- apply(logcounts, 1, var)
head(var_genes)
select_var <- names(sort(var_genes, decreasing=TRUE))[1:500]
head(select_var)
highly_variable_lcpm <- logcounts[select_var,]
dim(highly_variable_lcpm)
head(highly_variable_lcpm)
mypalette <- brewer.pal(11,"RdYlBu")
morecols <- colorRampPalette(mypalette)
col.cell <- c("purple","orange")[sampleinfo$CellType]
heatmap.2(highly_variable_lcpm, 
          col=rev(morecols(50)),
          trace="column", 
          main="Top 500 most variable genes across samples",
          ColSideColors=col.cell,scale="row")

## normalisation for composition bias
dgeObj <- calcNormFactors(dgeObj)
dgeObj$samples

plotMD(logcounts,column = 7)
abline(h=0,col="grey")

plotMD(dgeObj,column = 7)
abline(h=0,col="grey")


####### DE #####

group <- paste(sampleinfo$CellType,sampleinfo$Status,sep=".")
group

# design
group <- as.character(group)
type <- sapply(strsplit(group, ".", fixed=T), function(x) x[1])
status <- sapply(strsplit(group, ".", fixed=T), function(x) x[2])
# Specify a design matrix with an intercept term
design <- model.matrix(~ type + status)
design

# estimating dispersion
dgeObj <- estimateCommonDisp(dgeObj)
dgeObj <- estimateGLMTrendedDisp(dgeObj)
dgeObj <- estimateTagwiseDisp(dgeObj)


# tests DE
# fit LM
fit <- glmFit(dgeObj, design)
names(fit)
head(coef(fit))
# tests
lrt.BvsL <- glmLRT(fit, coef=2) 
topTags(lrt.BvsL)

# other contrasts
PvsV <- makeContrasts(statuspregnant-statusvirgin, levels=design)
lrt.pVsV <- glmLRT(fit, contrast=PvsV) 
topTags(lrt.pVsV)

results <- as.data.frame(topTags(lrt.BvsL,n = Inf))
results
dim(results)
summary(de <- decideTestsDGE(lrt.BvsL))

detags <- rownames(dgeObj)[as.logical(de)]
plotSmear(lrt.BvsL, de.tags=detags)

# volcano plot
signif <- -log10(results$FDR)
plot(results$logFC,signif,pch=16)
points(results[detags,"logFC"],-log10(results[detags,"FDR"]),pch=16,col="red")


# get annotation for genes in results
ann <- select(org.Mm.eg.db,keys=rownames(results),columns=c("ENTREZID","SYMBOL","GENENAME"))
head(ann)
results.annotated <- cbind(results, ann)
head(results.annotated)

write.csv(results.annotated,file="B.PregVsLacResults.csv",row.names=FALSE)

# gene set tests
BiocManager::install(c("fgsea"))
library(fgsea)


results <- as.data.frame(topTags(lrt.BvsL, n = Inf))

results.ord <- results[ order(-results[,"logFC"]), ]
head(results.ord)
ranks <- results.ord$logFC
names(ranks) <- rownames(results.ord)
head(ranks)

barplot(ranks)
load("./MouseData/mouse_H_v5.rdata")
pathways <- Mm.H
fgseaRes <- fgsea(pathways, ranks, minSize=15, maxSize = 500)
class(fgseaRes)
dim(fgseaRes)
head(fgseaRes[order(padj), ])

