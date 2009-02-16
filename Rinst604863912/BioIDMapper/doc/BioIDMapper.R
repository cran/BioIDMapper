###################################################
### chunk number 1: 
###################################################
library(BioIDMapper)
bio.type(5)
bio.type("SNP id")


###################################################
### chunk number 2: 
###################################################
data(glist)
bio.convert(glist, 1, 5)->myMap
myMap[1:10,]


###################################################
### chunk number 3: 
###################################################
data(glist)
bio.convert(glist, 1, 5)->myMap
bio.sum(myMap)
bio.sum(myMap, glist, FALSE)->mySum


###################################################
### chunk number 4: 
###################################################
data(glist)
bio.convert(glist, 1, 5)->myMap
bio.select(myMap, 1, "41386735")


###################################################
### chunk number 5: 
###################################################
data(glist)
bio.convert(glist, 1, 5)->myMap  # Note: 1 - gi number; 5 - snp id
myMap[80:91,]


###################################################
### chunk number 6: 
###################################################
bio.type("PDB id")


###################################################
### chunk number 7: 
###################################################
data(ulist)
bio.convert(ulist, 19, 40)->myMap  
# Note: 19 - UniProt Accesion number; 40 - PDB id
myMap[1:10,]


###################################################
### chunk number 8: 
###################################################
data(ulist)
bio.convert(ulist, 19, 5)->myMap
# Note: 19-UniProtKB Accession; 5-SNP id
myMap[1:10,]


