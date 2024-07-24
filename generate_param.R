## 1. generate_param()
## This function generates mean and variance parameters of normal distribution. 
##
## Input Variable
## p : the dimension of the data 
## base_sigma_2 : the baseline for variances
## prop : the proportion of variances with base_sigma_2 value
## delta : the remaining value apart from base_sigma_2
## sparse : boolean variable indicating whether the mean variances structure is sparse; default is TRUE.
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
