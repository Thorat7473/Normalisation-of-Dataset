U_cancer=function(data1){
  cancer=data1        #cancer is user input & data1 is value allotted to it

for(i in 1:ncol(data1)){
  cancer[, i] = (data1[, i]/sum(data1[,i]))*1000000   #normalize the data
  print(cancer)

l2cpm=log2(mat + 1)
l2cpm
}
return(cancer)
}

data1=read.csv("C:/Users/hp/Downloads/Cancer Genomics/Uterus.can.csv") #stored user input in csv file and read
data1  


y=function(data1){      # create 2nd function named as y 
  zs <- l2cpm
  for(i in 1:nrow(zs)){
    vec<-as.numeric(zs[i, ])
    zs[i, 1:ncol(zs)]<- (vec-mean(vec))/sd(vec)    #calculate Z score
    
  }
  zs[is.na(zs)]=0    
  View(zs)
}
  library(ComplexHeatmap)     #load complexheatmap library
  

  
  pdf('data.pdf',width = 10,height = 10)  #to save heatmap in pdf format 
  
  Heatmap(zs[readline(prompt="Enter any number")])        #range of heatmap to print
  
  
  dev.off()   #close the file

  