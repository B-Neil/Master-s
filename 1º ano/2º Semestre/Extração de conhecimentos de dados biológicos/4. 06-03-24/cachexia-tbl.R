## cachexia example

# loading data 

download.file("http://darwin.di.uminho.pt/dataAnalysisR/cachexia.csv","cachexia.csv")
download.file("http://darwin.di.uminho.pt/dataAnalysisR/meta_cachexia.csv","meta.csv")

cachexia = read.table("cachexia.csv", sep = ",", header = T, row.names = 1)
meta_cachexia = read.table("meta.csv", sep = ",", header = T, row.names = 1)
cachexia.log= log10(cachexia)
cachexia = cbind(cachexia, meta_cachexia)
dim(cachexia)


# check normal distribution
#a avaliar a normalidade das várias colunas
#b há 63 colunas que não seguem a normalidade

pv.orig = sapply(cachexia[,1:63], function(x) shapiro.test(x)$p.value)
sum(pv.orig < 0.01)
pv.log = sapply(cachexia.log[,1:63], function(x) shapiro.test(x)$p.value)
sum(pv.log < 0.01)
colnames(cachexia.log)[which(pv.log < 0.01)]




#2.1 analisar a expressão diferencial - #2. - Estamos a agrupar os dataset- os dados e os meta_dados e fazendo um teste de media comparando os valores de controlo com os "doentes" -  vamos separar as variaeis pelos dois grupos e fazer o t.test com o objetivo para avalair se há diferenças significativas nas medias do grupo 1 e do grupo 2.
#2.2- b - deveriamos verificar a normalidade pq dependendo deste parametro deveriamos usar um tipo diferente de teste usando o teste nao parametrico para variaveis que nao seguem a normalidade. A desvantagem e usar testes nao parametricos são menos potentes rejeitando menos vezes a hipotese nula. - No caso de seguir uma distribuição normal faziamos um t.test mas no caso que não faziamos um wilcox test. para verificar a normalidade fazeriamos um teste de shapiro.


# differential expression analysis
ds = merge(cachexia,meta_cachexia)

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


#3- 
