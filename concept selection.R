setwd('/Users/leichen/Research project/Predictive memory')
library(readxl)
library(tidyverse)
library(corrplot)
library(pheatmap)
library(stats)
first <- data.frame(read_excel('cos_matrix_brm_IFR.xlsx', sheet = '1st_200'))
second <- data.frame(read_excel('cos_matrix_brm_IFR.xlsx', sheet = '2nd_200'))
third <- data.frame(read_excel('cos_matrix_brm_IFR.xlsx',sheet = 'last_141'))
similarityMatrix <- merge(first,second, by = 'CONCEPT')
similarityMatrix <- merge(similarityMatrix,third, by = 'CONCEPT')
rownames(similarityMatrix) <- similarityMatrix[,1]
similarityMatrix <- similarityMatrix[,-1]
similarityMatrix <- as.matrix(similarityMatrix)
colnames(similarityMatrix)[394] <- 'ring(jewelry)'
rownames(similarityMatrix)[394] <- 'ring(jewelry)'



#Concept Selection: 1. hammer screwdriver hoe rake 2. toaster kettle oven dishwasher 3. bear deer squirrel hamster 4. guitar, harp, flute, harmonica

## Select 'sofa','desk','chair','dresser','bike','car','trolley','scooter','crown','ring(jewelry)','medal','necklace','shirt','skirt','mittens','sweater'

concept <- c('sofa','desk','chair','dresser','bike','car','trolley','scooter','crown','ring(jewelry)','medal','necklace','shirt','skirt','mittens','sweater')

correlationMatrix <- similarityMatrix[concept,concept]
corrplot(correlationMatrix, order = 'hclust', type = 'full', method = 'color',tl.col = 'black', tl.cex = 0.5)

colnames(correlationMatrix) <- c('a3','a1','a2','a4','b1','b2','b3','b4','c2','c1','c3','c4','d1','d2','d3', 'd4')
rownames(correlationMatrix) <- c('a3','a1','a2','a4','b1','b2','b3','b4','c2','c1','c3','c4','d1','d2','d3', 'd4')
