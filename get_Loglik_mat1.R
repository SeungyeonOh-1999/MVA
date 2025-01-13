## 4. get_Loglik_mat1()
##
## Input Variable
## xj_vj : a list containing sample mean differences, pooled sample variances, and sample mean sums
## ngrid_var : the number of grid points for variance parameters 
##
## Output Variable : A list
## v_k : grid points for variance parameters
## Loglik_mat :A p-by-(ngrid_var) matrix containing the density of pooled sample variances for a specific value v_k;

get_Loglik_mat1 = function(xj_vj, ngrid_var,train_n1, train_n2){
  
  min_Vj = min(xj_vj$Vj)
  max_Vj = max(xj_vj$Vj)
  v_k = exp(seq(log(min_Vj),log(max_Vj),length=ngrid_var)) 
  
  vj = xj_vj$Vj
  M2 = train_n1 + train_n2 - 2
  
  Loglik_mat = matrix(0, ncol=ngrid_var, nrow=P)
  
  for (i in 1:ngrid_var){
    vk = v_k[i]
    Loglik_mat[,i] = 1/2^(M2/2) * 1/gamma(M2/2) * (M2)^(M2/2) * vk^(-M2/2) * vj^(M2/2 - 1) * exp(-M2/(2*vk) * vj)
  }
  
  return(list(v_k=v_k,Loglik_mat=Loglik_mat))
}
