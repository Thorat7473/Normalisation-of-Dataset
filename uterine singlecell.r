install.packages("Seurat")
install.packages("dplyr")
install.packages("patchwork")

library(Seurat)
library(dplyr)
library(patchwork)

pbmc.data = Read10X("C:/Users/hp/Downloads/Cancer Genomics/single cell analysis")
pbmc.data

pbmc = CreateSeuratObject(counts = pbmc.data, min.cells = 3, min.features = 200)
pbmc

pbmc.data[1:50, 1:10]

pbmc[["percent.mt"]] = PercentageFeatureSet(pbmc, pattern = "^MT-")
head(pbmc@meta.data)

VlnPlot(pbmc, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)

pbmc = subset(pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
pbmc

pbmc = NormalizeData(pbmc)
pbmc = FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)

# Identify the 10 most highly variable genes
top10 = head(VariableFeatures(pbmc), 10)
top10

plot1 = VariableFeaturePlot(pbmc)
plot1 
plot2 = LabelPoints(plot = plot1, points = top10, repel = TRUE)
plot2


all.genes = rownames(pbmc)
pbmc = ScaleData(pbmc, features = all.genes)

pbmc@assays$RNA@scale.data[1:50, 1:5]

pbmc = RunPCA(pbmc, features = VariableFeatures(object = pbmc))

DimHeatmap(pbmc, dims = 1:15, cells = 500, balanced = TRUE)
