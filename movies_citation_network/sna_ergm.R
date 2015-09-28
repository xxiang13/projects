citationsWithIndex <- read.csv("~/Documents/MSiA/Spring 2015/MSiA 490 Social Network/project/citationsWithIndex.csv", stringsAsFactors=FALSE)
mydata = citationsWithIndex
mydata = mydata[order(mydata$citationsIndex.OutIndex,citationsIndex.InIndex),]
mydata$weight = 1
s = mydata$citationsIndex.OutIndex
r = mydata$citationsIndex.InIndex
s <- sapply(s, as.character)
r <- sapply(r, as.character)
w = mydata$weight
el = matrix(c(s,r,w), length(s), 3)
el

labels <- unique( c(el[,1], el[,2]) )
A1 <- matrix(0, length(labels), length(labels))
rownames(A1) <- colnames(A1) <- labels
A1
A1[ el[,1:2] ] <- as.numeric( el[,3] )
A1
require(sna)
p = gplot(A1, label=rownames(A1))

require(network)
G <- as.network(A1)
G
library(ergm)
library(sna) 
################
f1 = ergm(G ~ edges)
f1
summary(f1)
# The corresponding probability is 0.0004569544:
exp(-7.69047)/(1+exp(-7.69047))

#########
f2 <- ergm(G ~ edges + triangles, seed=1)       
f2
summary(f2)
