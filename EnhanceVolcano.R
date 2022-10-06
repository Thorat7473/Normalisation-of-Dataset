mat = matrix(NA, ncol= 4, nrow= nrow(cancer))
rownames(mat)= rownames(cancer)
colnames(mat)= c('grp1','grp2','pval','log2FC')
print(mat)

for(i in 1:nrow(cancer)){
  vec1 = as.numeric(cancer[i,1:4])
  vec2 = as.numeric(cancer[i,5:7])
  res = t.test(vec1, vec2, paired = F, alternative = 'two.sided')
  mat[i,1]= res$estimate[[1]]
  mat[i,2]= res$estimate[[2]]
  mat[i,3]= res$p.value
  mat[1,4]=mat[i,1]-mat[i,2]
  
}

mat= as.data.frame(mat)
num = which(is.nan(mat$pval))
mat[num,'pval']=1

EnhancedVolcano(mat,lab = rownames(mat),x='log2FC',y= 'pval')

