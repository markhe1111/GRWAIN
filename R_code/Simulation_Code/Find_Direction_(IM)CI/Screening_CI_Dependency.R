
 

library(glmnet)

setwd ("C:/Users/markh/Dropbox/Columbia/GRWAIN/Simulations/CI/Clustered")

XY_na = read.csv('PosNeg1_1.csv', h = F)

XY_na[XY_na == -99] = NA





XY_na_copy = XY_na


standardize = function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T) )  
}


for( j in 1:20){
   
  j_B = j*5
  
  Xy_j = XY_na[, j:j_B]
  X = as.matrix(Xy_j[,1:4])
  y = Xy_j[,5]
  
  m = 1* !is.na(y)
  fit =  glm( m  ~   X   , family = 'binomial') 
  
  sumfit = summary(fit)
  Coef = sumfit$coefficients
  
  Coef
  
  pvals = Coef[,4]
  Betas = Coef[,1]
  

  sig_beta_signs = sign( Betas[ pvals <.05] )
  
  
  if(sum( sig_beta_signs)  > 0  ){
  
    y_rev = y * -1
    
 
    std_y_reverse = standardize(y_rev)
    Xy_jnew = cbind(X, std_y_reverse)
  
    XY_na_copy[,j_B] = std_y_reverse  
    }
  
   
}

