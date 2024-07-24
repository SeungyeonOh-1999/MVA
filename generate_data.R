## 2. generate_data()
## This function generates training data and test data of two groups.
##
## Input variable 
## param : A list of mean and variance parameters of two group 
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