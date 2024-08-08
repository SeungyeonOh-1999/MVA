## 3. get_Xj_Vj()
##
## Input Variable
## data : a list containing Group1 and Group2
## train_n1 : the number of data of Group1 in the training dataset
## train_n2 : the number of data of Group2 in the training dataset
##
## Output Variable : A list
## Xj : sample mean differences between Group1 and Group2 for each feature
## Vj : pooled sample variances for each feature
## spmean_tr_sum : sample mean sums between Group1 and Group2 for each feature

get_Xj_Vj = function(data,train_n1, train_n2){
  
  tr_G1 = data$train_G1 ; tr_G2 = data$train_G2
  
  spmean_tr_G1 = apply(tr_G1,2,mean) ; spmean_tr_G2 = apply(tr_G2,2,mean) 
  spmean_tr_diff = spmean_tr_G1 - spmean_tr_G2
  spmean_tr_sum = spmean_tr_G1 + spmean_tr_G2
  
  spvar_tr_G1 = apply(tr_G1,2,var) ; spvar_tr_G2 = apply(tr_G2,2,var)
  spvar = ((train_n1-1)*spvar_tr_G1 + (train_n2-1)*spvar_tr_G2)/(train_n1+train_n2-2)
  
  output = list(Xj=spmean_tr_diff, Vj=spvar, spmean_tr_sum=spmean_tr_sum)
  return(output)
}
