## cachexia example

# loading data 

download.file("http://darwin.di.uminho.pt/dataAnalysisR/cachexia.csv","cachexia.csv")
download.file("http://darwin.di.uminho.pt/dataAnalysisR/meta_cachexia.csv","meta.csv")

cachexia = read.table("cachexia.csv", sep = ",", header = T, row.names = 1)
meta_cachexia = read.table("meta.csv", sep = ",", header = T, row.names = 1)
cachexia = cbind(cachexia, meta_cachexia)
dim(cachexia)


# check normal distribution

pv.orig = sapply(cachexia[,1:63], function(x) shapiro.test(x)$p.value)
sum(pv.orig < 0.01)
pv.log = sapply(cachexia.log[,1:63], function(x) shapiro.test(x)$p.value)
sum(pv.log < 0.01)
colnames(cachexia.log)[which(pv.log < 0.01)]

# differential expression analysis

testCompound = function(ds, index) ## parametric
{
  g1 = ds[ds$Muscle.loss=="cachexic",index]
  g2 = ds[ds$Muscle.loss=="control",index]
  restt = t.test(g1, g2)
  restt$p.value
}

pvs = c()
for (i in 1:63) pvs[i] = testCompound(cachexia.log, i)
range(pvs)
hist(-log10(pvs), breaks = 0:6,  col = "blue")

rank=order(pvs)[1:10]
names(cachexia)[rank] # compounds

testCompoundNP = function(ds, index)
{
  g1 = ds[ds$Muscle.loss=="cachexic",index]
  g2 = ds[ds$Muscle.loss=="control",index]
  restt = wilcox.test(g1, g2)
  restt$p.value
}


pvs_np = c()
for (i in 1:63) pvs_np[i] = testCompoundNP(cachexia.log, i)
range(pvs_np)
hist(-log10(pvs_np), breaks = 0:6, col = "green")

rank=order(pvs_np)[1:10]
names(cachexia)[rank] # compounds

testCompoundCheckingNormality = function(ds, index)
{
  g1 = ds[ds$Muscle.loss=="cachexic",index]
  g2 = ds[ds$Muscle.loss=="control",index]
  pv1 = shapiro.test(g1)$p.value
  pv2 = shapiro.test(g2)$p.value
  if (pv1 > 0.01 && pv2 > 0.01)  restt = t.test(g1, g2)
  else restt= wilcox.test(g1, g2)
  restt$p.value
}

#pvs_adj = p.adjust(pvs, "bonferroni") # more strict
pvs_adj = p.adjust(pvs, "BH") # "BH" more conservative
sum(pvs < 0.01)
sum(pvs_adj < 0.01)
hist(-log10(pvs_adj), breaks = 0:6, col = "orange")
