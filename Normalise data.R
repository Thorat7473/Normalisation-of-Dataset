uterus_data=read.csv("C:/Users/hp/Downloads/Cancer Genomics/Uterus.can.csv")
uterus_data
mat= as.matrix(data1)
rownames(mat)= data[,1]    #matrix
View(mat)


for(i in 1:ncol(data1)){
  mat[, i] = (data1[, i]/sum(data1[,i]))*1000000   #normalize the data
}
l2cpm=log2(mat + 1)
l2cpm

