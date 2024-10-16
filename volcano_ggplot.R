##volcano plot with ggplot
# Loading relevant libraries 
library(tidyverse) # includes ggplot2, for data visualisation. dplyr, for data manipulation.
library(RColorBrewer) # for a colourful plot
library(ggrepel) # for nice annotations
#import DEG
df = read.csv('Wildtype-vs-FDD-Knockin/Differential_expression_analysis_table.csv', check.names = F)
df = read.csv("Differential_expression_analysis_table.csv", check.names = F)
# Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2fc respectively positive or negative)<br /><br /><br />
df$diffexpressed <- "NO"
# if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP"<br /><br /><br />
df$diffexpressed[df$log2FoldChange > 0 & df$padj < 0.05] <- "UP"
# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"<br /><br /><br />
df$diffexpressed[df$log2FoldChange < 0 & df$padj < 0.05] <- "DOWN"
# Explore a bit<br /><br /><br />
head(df[order(df$padj) & df$diffexpressed == 'DOWN', ])
ggplot(data = df, aes(x = log2FoldChange, y = -log10(padj), col = diffexpressed)) + geom_point(size = 3.5, alpha = 2) + scale_color_manual(values = c("#00AFBB", "grey", "magenta"), # to set the colours of our variable<br /><br /><br />
                                                                                                                                labels = c("Downregulated", "Not significant", "Upregulated")) + ggtitle('FDD vs Knock out') # to set the labels in case we want to overwrite the categories from the dataframe (UP, DOWN, NO)</p><br /><br />
+ geom_vline(xintercept = c(-0.6, 0.6), col = "gray", linetype = 'dashed')
+ geom_hline(yintercept = -log10(0.05), col = "gray", linetype = 'dashed')
# Create a new column "delabel" to de, that will contain the name of the top 30 differentially expressed genes (NA in case they are not)
#df$delabel <- ifelse(df$Gene.name %in% head(df[order(df$padj), "gene_symbol"], 30), df$gene_symbol, NA)
df$delabel <- ifelse(df$Gene.name %in% df[(df$padj< 0.05), "Gene.name"], df$Gene.name, NA)
#oloting with gene id labelled
ggplot(data = df, aes(x = log2FoldChange, y = -log10(padj), col = diffexpressed, label = delabel)) +
  geom_point(size = 2) + 
  scale_color_manual(values = c("#00AFBB", "grey", "magenta"), # to set the colours of our variable  
                     labels = c("Downregulated", "Not significant", "Upregulated")) + # to set the labels in case we want to overwrite the categories from the dataframe (UP, DOWN, NO)
  ggtitle('FDD vs Knock out') +# Plot title 
  geom_text_repel()# To show all labels 
#coord_cartesian(ylim = c(0, 250), xlim = c(-10, 10))

#we can also create some levels for the genes and modify plots accordingly
#create some label for the genes
genes_to_label <- c("Rack1", "Ptma", "S100a6", "Rpl37", "Slc6a4", "Tmem79", "Ccdc40", "Acta1", "Fam189b", "Krt17", "")
df$label <- ifelse(df$Gene.name %in% genes_to_label, df$Gene.name, NA)

# Define colors
dark_colors <- c("blue", "grey", "magenta")  # Dark colors for the border
light_colors <- c("#ADD8E6","#D3D3D3", "#FFC0CB")  # Lighter colors for the fill (light blue and pink)

# Generate the volcano plot
ggplot(data = df, aes(x = log2FoldChange, y = -log10(padj),fill = diffexpressed, color = diffexpressed)) +
  geom_point(shape = 21, size = 3, stroke = 0.7) +  # Use shape 21 for points with both color and fill , stroke for border thickness
  scale_color_manual(values = dark_colors, labels = c("Downregulated", "Upregulated", "Not significant")) +
  scale_fill_manual(values = light_colors, labels = c("Downregulated", "Upregulated", "Not significant")) +
  ggtitle('FDD vs Knock out') +
  geom_text(aes(label = label), vjust = -1, hjust = 0.5, size = 4) + xlim(-4,2) +ylim(0,17)

