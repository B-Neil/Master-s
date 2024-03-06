if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

#if (!requireNamespace("ComplexHeatmap", quietly = TRUE))
#  BiocManager::install("ComplexHeatmap") 

if (!requireNamespace("TCGAbiolinks", quietly = TRUE))
  BiocManager::install("TCGAbiolinks")

BiocManager::install("MultiAssayExperiment")

# BiocManager::install("maftools")

library(TCGAbiolinks)
library(MultiAssayExperiment)
#library(maftools)
#library(dplyr)
#library(ComplexHeatmap)

projects <- getGDCprojects()
projects$id

## example DESeq
## https://rpubs.com/tiagochst/TCGAbiolinks_to_DESEq2

proj <- "TCGA-GBM"
query <- GDCquery(
  project = proj,
  data.category = "Transcriptome Profiling", 
  data.type = "Gene Expression Quantification",
  workflow.type = "STAR - Counts"
)
GDCdownload(query)
data_rna_gbm  <- GDCprepare(query)

class(data_rna_gbm)
dim(data_rna_gbm)

data_rna_gbm$paper_BCR
data_rna_gbm$paper_Gender
data_rna_gbm$paper_Grade
data_rna_gbm$paper_IDH.status

meta_gbm = colData(data_rna_gbm)
dim(meta_gbm)
meta_gbm$patient
meta_gbm$paper_IDH.status

library(DESeq2)

data_de <- data_rna_gbm[,!is.na(data_rna_gbm$paper_IDH.status)]

ddsSE <- DESeqDataSet(data_de, design = ~ paper_IDH.status)

keep <- rowSums(counts(ddsSE)) >= 10
ddsSE <- ddsSE[keep,]
ddsSE <- DESeq(ddsSE)

resultsNames(ddsSE)

res <- results(ddsSE, name = "paper_IDH.status_WT_vs_Mutant")
dea <- as.data.frame(res)
summary(res)

## dados clinicos

query_clin <- GDCquery(project = "TCGA-GBM", 
                  data.category = "Clinical",
                  data.type = "Clinical Supplement", 
                  data.format = "BCR Biotab")
GDCdownload(query_clin)
clinical.gbm <- GDCprepare(query_clin)
names(clinical.gbm)

head (clinical.gbm$clinical_drug_gbm )
df = as.data.frame(clinical.gbm$clinical_patient_gbm)
View(df)


