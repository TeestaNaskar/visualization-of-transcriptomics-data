#simple volcano plot
setwd("/Users/teestanaskar/Desktop/Teestaowned.HurdLab.Data/RNAseq_arnab/Azenta.RNAseq.April2024/DEG/deseq2/Wildtype_Male-vs-FDD-Knockin_Male")
all = read.csv('Wildtype-vs-FDD-Knockin/Differential_expression_analysis_table.csv', check.names = F)
male = read.csv("Differential_expression_analysis_table.csv")
female = read.csv('Wildtype_Female-vs-FDD-Knockin_Female/Differential_expression_analysis_table.csv', check.names = F)
female$color = 'gray'
female$color[(female$log2FoldChange>0) & (female$padj<0.05)]='magenta'
female$color[(female$log2FoldChange<0) & (female$padj<0.05)]='cyan'

plot(female$log2FoldChange, -log10(female$padj), col=female$color,
     xlab='Log2 Fold Change', ylab='-log10 padj', title('FDD vs Knock down'))
