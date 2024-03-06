if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("DESeq2")
BiocManager::install("pheatmap")

library(DESeq2)
library(pheatmap)

# 1. abrir as contagens e criar o objeto DESeqDataSet
# carregar a matriz de contagens

c1c2 = read.table("C1C2.allgenes.tab", h=T, row.names=1)
dim(c1c2)
# definir as condições/experiencias/replicas a comparar
condition = factor(c("C1", "C1", "C1","C2", "C2", "C2"))
# pre-filtrar os genes com baixa contagem antes de correr funcoes do DESeq2
c1c2 = c1c2[ rowSums(c1c2) > 1, ]
# definir as condicoes das condições/experiencias/replicas no dataframe
cd=data.frame(c("C1","C1","C1","C2","C2","C2")) 
colnames(cd)[1]="condition"
rownames(cd)=colnames(c1c2)
cd
head(c1c2)

# criar um objeto de análise de expressão diferencial
dds = DESeqDataSetFromMatrix(countData = c1c2,colData = cd, design = ~ condition)
dds

# 2. Correr o teste de análise de expressão diferencial
# expressão diferencial das experiencias C1 vs C2
dds = DESeq(dds) 
res = results(dds) 
res
# ordernar os nossos resultados pelo menor valor de p:
resOrdered <- res[order(res$padj),]
# Resumir algumas contagens básicas utilizando a funcao summary
summary(res)
# existem muitos valores p abaixo de 0.1?
sum(res$padj < 0.1, na.rm=TRUE)

# 3. Explorar e exporter os resultados
# gráfico MA
DESeq2::plotMA(res, main="DESeq2", ylim=c(-2,2))
# gráfico de contagens para o gene mais significativo da comparação das duas condições
plotCounts(dds, gene=which.min(res$padj), intgroup="condition", pch = 19, col = condition)
resOrdered[1,]
# Exportar os resultados para fichero CSV
write.csv(as.data.frame(resOrdered),file="condition_treated_results.csv")

# 4. Transformação de dados e manipulação
# Transformacao de dados de contagem
# Para testar os valores de expressão diferencial deve-se utilizar as contagens sem qualquer transformacao, utilizando distribuições discretas
# Para ajudar a visualização dos dados ou para métodos de clustering pode ser util criar versoes de contagens transformadas
# VST: varianceStabilizingTransformation
vsd = varianceStabilizingTransformation(dds, blind=FALSE) 
head(assay(vsd), 3)
# compara os dados anteriores com os dados do commando head(counts(dds), 3)
head(counts(dds), 3)

# Criar um Heatmap com as contagens
select <- rownames(head(resOrdered,20))
vsd.counts <- assay(vsd)[select,]
df <- as.data.frame(colData(dds)[,c("condition")])

# comando para manter a ordem das linhas
pheatmap(vsd.counts, cluster_rows=FALSE)
# agrupar por linha e colunas
pheatmap(vsd.counts)

# 5. Heatmap de distancias entre amostras
# calcular a distancia entre as amostras
sampleDists <- dist(t(assay(vsd)))
# preparar as contagens
sampleDistMatrix <- as.matrix(sampleDists) 
rownames(sampleDistMatrix) <- dds$condition 
colnames(sampleDistMatrix) <- NULL

# 5.1 Heatmap contend as distancias Euclideanas entre as amostras calculadas a partir da transformacao vst.
library(RColorBrewer)
colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
pheatmap(sampleDistMatrix, clustering_distance_rows=sampleDists, clustering_distance_cols=sampleDists, col=colors)

# 5.2 Principal component plot of the samples
plotPCA(vsd, intgroup=c("condition"))

