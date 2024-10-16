#heatmap for the ventral and dorsal striatum rna seq data
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(openxlsx)
library(data.table)
library(ComplexHeatmap)
setwd("/Users/teestanaskar/Dropbox/Teesta/Brain/Human/Bothsex/RNAseq")

meta = read.xlsx("raw_data/Metadata.xlsx", sheet = 2)

meta$sampleID = paste0(meta$Regions,meta$SerieID)
rownames(meta) = meta$sampleID
#ordreing my metadata alphabetically
meta = meta %>% arrange(sampleID)
#remove any samples that are not matching 
meta = meta[!(meta$sampleID %in% c("VS35", "VS37")), ]
#filtering for VS
meta = meta[meta$Regions =="VS",]
meta$condition = as.factor(paste0(meta$Group,'_',meta$sampleID))
rownames(meta) = meta$condition

vs = read.xlsx("deseq2/DEG/VS_DEGs_Deseq2.xlsx", sheet = 2)
vs = vs[vs$pvalue< 0.05, ]
VS.vst = read.xlsx("raw_data/vst/VS_VST_counts.xlsx")
#subset the vst based on the DEGs
VS.vst = VS.vst[VS.vst$Gene.name %in% vs$Gene.name,]
VS.vst = VS.vst[complete.cases(VS.vst$Gene.name),]
rownames(VS.vst) = make.unique(VS.vst$Gene.name)
VS.vst = VS.vst[,6:ncol(VS.vst)]
VS.vst = VS.vst[,meta$sampleID]
colnames(VS.vst) = meta$Group
VS.vst =VS.vst[,order(colnames(VS.vst), decreasing = TRUE)]
VS.mat = as.matrix(VS.vst)
mat.scaled = t(apply(VS.mat, 1, scale))
colnames(mat.scaled) = colnames(VS.vst)
Heatmap(mat.scaled, cluster_rows = T, cluster_columns = T)
Heatmap(mat.scaled, 
        cluster_rows = TRUE, 
        cluster_columns = FALSE,
        row_names_gp = gpar(fontsize = 10, fontface = "bold", col = "blue"))  # Customize row names

ds = read.xlsx("deseq2/DEG/DS_DEGs_Deseq2.xlsx", sheet = 2)
ds = ds[ds$pvalue< 0.05,]
DS.mat = as.matrix(DS_deseq2)

#meta = read.xlsx("raw_data/Metadata.xlsx", sheet = 2)

meta$sampleID = paste0(meta$Regions,meta$SerieID)
rownames(meta) = meta$sampleID
#ordreing my metadata alphabetically
meta = meta %>% arrange(sampleID)
#remove any samples that are not matching 
meta = meta[!(meta$sampleID %in% c("VS35", "VS37")), ]
#filtering for VS
meta = meta[meta$Regions =="VS",]
meta$condition = as.factor(paste0(meta$Group,'_',meta$sampleID))
rownames(meta) = meta$condition

#visualization
Heatmap(VS.mat)
