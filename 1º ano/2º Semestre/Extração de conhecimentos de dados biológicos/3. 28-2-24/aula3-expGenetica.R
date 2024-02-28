### BIOCONDUCTOR ###

#if (!requireNamespace("BiocManager", quietly = TRUE) ) 
#  install.packages("BiocManager")
#BiocManager::install(version = "3.16")

#BiocManager::install(c("genefilter", "limma"))
#BiocManager::install(c("ALL"))
#BiocManager::install(c("GEOquery", "hgu95av2.db", "MLInterfaces", "GOstats"))

library(ALL)
data(ALL)
ALL
dim(ALL)

exp= exprs(ALL)

maximos = apply(exp,1,max)
minimos = apply(exp,1,min)
vl = maximos/minimos > 2
ALLm2=ALL[vl,]
ALLm2

s = which(as.character(ALL$mol.biol) %in% c("BCR/ABL", "NEG"))
ALLs = ALLm2[, s]
ALLs
ALLs$mol.biol = factor(ALLs$mol.biol)
table(ALLs$mol.biol)

library(genefilter)
tt = rowttests(ALLs, "mol.biol")
names(tt)
tt$p.value
rank = order(tt$p.value)
p20 = rank[1:20]
tt$p.value[p20]

g =featureNames(ALLm2[p20])
g
annotation(ALL)

library(hgu95av2.db)
unlist(mget(g, hgu95av2SYMBOL))

ALL20 = ALLs[p20,]
order_cols = order(ALL20$mol.biol)
order_cols
ALL20 = ALL20[,order_cols]
heatmap(exprs(ALL20), Colv = NA, labCol = ALL20$mol.biol)

library(limma)
design = model.matrix(~ALLm2$mol.biol)
fit = lmFit(ALLm2,design)
fit2 = eBayes(fit)
diff = topTable(fit2, coef=2, 10)
diff
head(unlist(mget(rownames(diff), hgu95av2SYMBOL)))

library(GOstats)
ALLenr = ALL[, s]
filt = nsFilter(ALLenr, require.entrez=T, remove.dupEntrez=T, var.func=IQR, var.cutoff=0.5, feature.exclude="^AFFX")
ALLf = filt$eset

affyUniverse = featureNames(ALLf)
entrezUniverse = unlist(mget(affyUniverse, hgu95av2ENTREZID))
length(entrezUniverse)

ttests = rowttests(ALLf, "mol.biol")
smPV = ttests$p.value < 0.01
sum(smPV)
pvalFiltered = ALLf[smPV, ]

selectedEntrezIds = unlist(mget(featureNames(pvalFiltered), hgu95av2ENTREZID))


params = new("GOHyperGParams", geneIds=selectedEntrezIds, 
             universeGeneIds=entrezUniverse, annotation="hgu95av2.db", ontology="MF", 
             pvalueCutoff= 0.025, testDirection="over")
hgOver = hyperGTest(params)
hgOver
summary(hgOver)
