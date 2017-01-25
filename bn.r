#Q4
#cleanup before start
rm(list=ls(all=T))
library(bnlearn)
#4.b
#Read data from given CSV File
dataSet <- read.csv("bn-data.csv")
#Delete irrelvant first column
dataSet[1] <- NULL
#Construct Bayesian network using Hill Climbing greedy search
res <- hc(dataSet)
#Remove irrelevant edge from M..Work to Family as Mental work can't influence family
res$arcs <- res$arcs[-which((res$arcs[,'from'] == "M..Work" & res$arcs[,'to'] == "Family")),]
#Plot Bayesian network
plot(res)

#Q4.c
#Calculate conditional probablity and print table for each node.
CPT <- bn.fit(res, data = dataSet)
print(CPT$Smoking)
print(CPT$Family)
print(CPT$M..Work)
print(CPT$P..Work)
print(CPT$Pressure)
print(CPT$Proteins)