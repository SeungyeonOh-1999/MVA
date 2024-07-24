## 3. get_Xj_Vj()
## This function calculates sample mean differences, pooled sample variances and sums of sample means for each feature.
##
## Input variable
## data : A list of group1 and group2 data matrix
## train_n1 : the number of training data of group1 
## train_n2 : the number of training data of group2
## 
## Output variable : A list 
## Xj : the sample mean differences
## Vj : the pooled sample variances 
## spmean_tr_sum : sums of sample means

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
