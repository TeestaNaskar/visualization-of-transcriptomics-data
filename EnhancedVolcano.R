#Volcano plot with Enhancedvolcano package
#load required package
library(data.table)
library(openxlsx)
library(EnhancedVolcano)
library(tidyverse)
library(dplyr)
library(RColorBrewer) # for a colourful plot
library(ggrepel) # for nice annotations
setwd("/Users/teestanaskar/Desktop/Teestaowned.HurdLab.Data/RNAseq_arnab/Azenta.RNAseq.April2024/DEG/deseq2/")
all = read.csv("Wildtype-vs-FDD-Knockin/Differential_expression_analysis_table.csv")
male = read.csv("Wildtype_Male-vs-FDD-Knockin_Male/Differential_expression_analysis_table.csv")
female = read.csv("Wildtype_Female-vs-FDD-Knockin_Female/Differential_expression_analysis_table.csv")
rownames(male) = make.unique(male$Gene.name)
rownames(female) = make.unique(female$Gene.name)
rownames(all) = make.unique(all$Gene.name)
#customizing colors
keyvals <- ifelse(
  all$log2FoldChange < 0 & all$padj <0.05, 'blue', 
  ifelse(all$log2FoldChange > 0  & all$padj <0.05, 'red', 'black'))
keyvals[is.na(keyvals)] <- 'black'
names(keyvals)[keyvals == 'red'] <- 'upregulation'
names(keyvals)[keyvals == 'black'] <- 'NS'
names(keyvals)[keyvals == 'blue'] <- 'downregulation'

EnhancedVolcano(all,
                lab = rownames(all),
                x = 'log2FoldChange',
                y = 'padj')
#Enhanced volcano plot
p = EnhancedVolcano(all,
                lab = rownames(all),
                x = 'log2FoldChange',
                y = 'padj',
                selectLab = c('Nol4'), #selectlab is for any genes I want to highlight in the plot
                title= 'both sex combined',
                pCutoff= 0.05,
                pointSize = 5.0,
                labSize = 5.0,
                FCcutoff= 0.5,
                colCustom = keyvals,
                #xlim = c(-6,6),
                #ylim = c(0,12),
                #legendPosition = 'right',
                borderColour = 'black',
                legendLabSize = 20,
                border = 'full',
                shape = 'circle'
)
#save plot
# Open a TIFF device
tiff("EnhancedVolcano_sexcombined.tiff", width = 6, height = 6, units = "in", res = 300)
# Print the plot to the TIFF device
print(p)

# Close the device to save the file
dev.off()

