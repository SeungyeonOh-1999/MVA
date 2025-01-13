## 1.1 generate_param() 
##
## Input Variable
## p : the dimension of the data 
## base_sigma_2 : the baseline for variances
## prop : the proportion of variances with base_sigma_2 value
## delta : the remaining value apart from base_sigma_2
## sparse : boolean variable indicating whether the mean differences structure is sparse; default is TRUE.
##
## Output Variable : A list
## mu1 : the mean parameters of group 1 
## mu2 : the mean parameters of group 2
## sigma_2 : the variance parameters

generate_param = function(p,base_sigma_2,prop,delta, sparse=TRUE){
  
  mu1 = c(rep(1,100),rep(0,p-100)) ; mu2 = rep(0,p)
  sigma_2 = c(rep(base_sigma_2,p*prop), rep(delta,p*(1-prop)))
  
  if(sparse==FALSE){
    tmp_mu1 = rnorm((p-100),0,0.1)
    mu1 = c(rep(1,100),tmp_mu1)
  }
  
  return(list(mu1=mu1,mu2=mu2,sigma_2=sigma_2))
}

## 1.2 generate_param_beta()
generate_param_beta = function(p, beta, sparse=TRUE){
  
  sigma_2 = 5 * rbeta(p, 5, beta)
  mu1 = c(rep(1,100),rep(0,p-100)) ; mu2 = rep(0,p)
  
  if(sparse==FALSE){
    tmp_mu1 = rnorm((p-100),0,0.1)
    mu1 = c(rep(1,100),tmp_mu1)
  }
  
  return(list(mu1=mu1,mu2=mu2,sigma_2=sigma_2))
}

## 2. generate_data()
##
## Input variable 
## param : the output of generate_param()
## train_n1 : the number of training data of group1 
## train_n2 : the number of training data of group2
## test_n1 : the number of test data of group1 
## test_n2 : the number of test data of group2
##
## Output variable : A list 
## train_G1 : the train_n1-by-p train data matrix of group1 
## train_G2 : the train_n2-by-p train data matrix of group2
## test_set : the (test_n1 + test_n2)-by-p test data matrix

generate_data = function(param,train_n1,train_n2,test_n1,test_n2){
  
  G1 = mapply(function(x,y){rnorm((train_n1+test_n1),x,sqrt(y))}, param$mu1, param$sigma_2)
  G2 = mapply(function(x,y){rnorm((train_n2+test_n2),x,sqrt(y))}, param$mu2, param$sigma_2)
  train_G1 = G1[1:train_n1, ] ; train_G2 = G2[1:train_n2, ]
  
  test_G1 = G1[(train_n1+1):nrow(G1),] ; test_G2 = G2[(train_n2+1):nrow(G2),]
  test_set = rbind(test_G1, test_G2)
  
  
  output = list(train_G1 = train_G1, train_G2 = train_G2, test_set=test_set)
  return(output)
}

## 3. get_Xj_Vj()
##
## Input Variable
## data : the output of generate_data() or a list including the group1 train data, the group2 train data.
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

## 4. get_Loglik_mat1()
##
## Input Variable
## xj_vj : the output of get_Xj_Vj()
## ngrid_var : the number of grid points for variance parameters 
##
## Output Variable : A list
## v_k : grid points for variance parameters
## Loglik_mat :A p-by-(ngrid_var) matrix containing the density of pooled sample variances for a specific value v_k;

get_Loglik_mat1 = function(p, xj_vj, ngrid_var,train_n1, train_n2){
  
  min_Vj = min(xj_vj$Vj)
  max_Vj = max(xj_vj$Vj)
  v_k = exp(seq(log(min_Vj),log(max_Vj),length=ngrid_var)) 
  
  vj = xj_vj$Vj
  M2 = train_n1 + train_n2 - 2
  
  Loglik_mat = matrix(0, ncol=ngrid_var, nrow=p)
  
  for (i in 1:ngrid_var){
    vk = v_k[i]
    Loglik_mat[,i] = 1/2^(M2/2) * 1/gamma(M2/2) * (M2)^(M2/2) * vk^(-M2/2) * vj^(M2/2 - 1) * exp(-M2/(2*vk) * vj)
  }
  
  return(list(v_k=v_k,Loglik_mat=Loglik_mat))
}

## 5. get_Loglik_mat2()
## 
## Input Variable
## loglik_mat1 : second element of the output of get_Loglik_mat1()
## fv : first element of the output of get_Loglik_mat1()
##
## Output Variable : A list 
## u_k : grid points for mean difference parameters
## Loglik_mat : A p-by-(ngrid) matrix containing the density of sample mean difference for a specific value u_k.

get_Loglik_mat2 = function(p, xj_vj, ngrid, loglik_mat1, fv, train_n1, train_n2){
  
  min_Xj = min(xj_vj$Xj)
  max_Xj = max(xj_vj$Xj)
  u_k = seq(min_Xj, max_Xj, length.out = ngrid)
  xj = xj_vj$Xj
  vj = xj_vj$Vj
  vk = loglik_mat1$v_k
  M2 = train_n1 + train_n2 -2 
  
  Loglik_mat = matrix(0, ncol=ngrid, nrow=p)
  
  for (i in 1:ngrid){
    uk = u_k[i]
    f = matrix(0, ncol=length(vk), nrow=p)
    
    for (j in 1:length(vk)){
      v = vk[j]
      f[,j] = ( 1/sqrt(2*pi) * sqrt( (train_n1 * train_n2)/ ((train_n1+train_n2)*v) ) * 
                  exp(-(xj-uk)^2/2 * (train_n1 * train_n2)/ ((train_n1+train_n2)*v)) * 1/2^(M2/2) * 1/gamma(M2/2) *
                  (M2)^(M2/2) * v^(-M2/2) * vj^(M2/2 - 1) * exp(-M2/(2*v) * vj) ) 
    }
    
    Loglik_mat[,i] = f %*% fv
  }
  return(list(u_k=u_k,Loglik_mat=Loglik_mat))
}

## 6. get_est()
##
## Output : A data frame including the estimated mean differences and variances. ; (dim : p-by-2)
get_est = function(p, xj_vj, ngrid_var, ngrid, train_n1, train_n2){
  
  loglik_mat1 = get_Loglik_mat1(p, xj_vj, ngrid_var, train_n1, train_n2)
  L1 = loglik_mat1$Loglik_mat
  v_k = loglik_mat1$v_k
  g1 = mixsqp(L1)
  fv = g1$x
  sigma_2_est = (L1 %*% (v_k*fv)) / (L1 %*% fv)
  
  loglik_mat2 = get_Loglik_mat2(p, xj_vj, ngrid, loglik_mat1, fv, train_n1, train_n2)
  L2 = loglik_mat2$Loglik_mat
  u_k = loglik_mat2$u_k
  g2 = mixsqp(L2)
  fm = g2$x
  
  gn = seq(1:ngrid)
  tmp1 = mapply(function(u,w,i){u*w*L2[,i]},u_k,fm,gn)
  tmp2 = mapply(function(w,i){w*L2[,i]},fm,gn)
  mu_est = apply(tmp1,1,sum) / apply(tmp2,1,sum)
  est = data.frame(mu=mu_est, sigma_2=sigma_2_est)
  
  return(est)
}

# 7. evaluation()
## 
## Output Variable : A scalar ; Misclassification rate of MVA 
evaluation = function(data, xj_vj, est, test_group){
  
  MVA_a = est$mu / est$sigma_2
  MVA_a0 = -1/2 * sum(MVA_a*xj_vj$spmean_tr_sum)
  delta = data$test_set %*% MVA_a + MVA_a0
  yhat = ifelse(delta>0,1,2)
  conf_mat = confusionMatrix(as.factor(yhat), as.factor(test_group))
  acc = as.numeric(conf_mat$overall['Accuracy'])
  return(1-acc)
}