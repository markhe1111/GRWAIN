# Re-shape quantile function with covariante imbalance
# generate X from uniform 0 to 10

n = 300
p = 50

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
library(faux) 
 
setwd ("C:/Users/markh/Dropbox/Columbia/GRWAIN/Simulations/CI/Clustered")
 




# all POS

for(k in 1:10){
  
  
  namek = paste('PosNeg1_', k, '.csv', sep = '') 
  namek_true = paste('PosNeg1_', k, '_True.csv', sep = '') 
  
  Xy_true = list()
  Xy_na = list()
  
  for( j in 1:20){
    
    means = runif(4 , 0, 1)
    #X = rnorm_multi(n = 300  , mu = means,    r =  0.2 )
    
    X = data.frame( do.call(cbind, lapply(means, function(x)  rexp(n , x))))
    X = sapply(X, function(x) (x-min(x)) / (max(x) - min(x)) )
    y =   rexp(n  ,  .1)   # if IMCI, then this would be sumX
    y = (y - min(y)) / (max(y) - min(y)) 
    
    sumX = 1+  X[,1] + X[,2]+X[,3]

      
    p_obs_y=    1 - ( sumX  +1)/ max( ( sumX +1))           # probablity of being observed
    obs_y = rbinom(n,1,  p_obs_y)
    
    Xy = cbind(X, y )
    y_na = make_na( y , obs_y  ) 
    
    Xy_na1 = cbind(X, y_na )
    Xy_true[[j]] = Xy
    Xy_na[[j]] = Xy_na1
    
  }
  
  XY_true = do.call(cbind, Xy_true)
  XY_export = do.call(cbind, Xy_na)
  
  
  XY_export[ is.na(XY_export) ] =  - 99
  
  write.table( XY_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( XY_true ,  namek_true , sep = ',', col.names = F,row.names = F)
}





# Pos/ Neg

# first do pos, then do neg 


for(k in 1:10){
  
  
  namek = paste('PosNeg1_', k, '.csv', sep = '') 
  namek_true = paste('PosNeg1_', k, '_True.csv', sep = '') 
  
  Xy_true = list()
  Xy_na = list()
  
  for( j in 1:11){
    
    means = runif(4 , 0, 1)      
    X = data.frame( do.call(cbind, lapply(means, function(x)  rexp(n , x))))   #X = rnorm_multi(n = 300  , mu = means,    r =  0.2 )
    X = sapply(X, function(x) (x-min(x)) / (max(x) - min(x)) )
    y =  1+  X[,1] + X[,2]+X[,3]
    y = (y - min(y)) / (max(y) - min(y)) 
    p_obs_y=    1 - ( sumX  +1)/ max( ( sumX +1))           # probablity of being observed
    obs_y = rbinom(n,1,  p_obs_y)
    
    Xy = cbind(X, y )
    y_na = make_na( y , obs_y  ) 
    
    Xy_na1 = cbind(X, y_na )
    Xy_true[[j]] = Xy
    Xy_na[[j]] = Xy_na1
    
  }
  
  for( j in 12:20){
    
    means = runif(4 , 0, 1)      
    X = data.frame( do.call(cbind, lapply(means, function(x)  rexp(n , x))))   #X = rnorm_multi(n = 300  , mu = means,    r =  0.2 )
    X = sapply(X, function(x) (x-min(x)) / (max(x) - min(x)) )
    y =  1+  X[,1] + X[,2]+X[,3]  # if IMCI, then this would be sumX
    y = (y - min(y)) / (max(y) - min(y)) 
    
    sumX = 1+  X[,1] + X[,2]+X[,3]
    
    p_obs_y=     ( sumX  +1)/ max( ( sumX +1))           # probablity of being observed
    obs_y = rbinom(n,1,  p_obs_y)
    
    Xy = cbind(X, y )
    y_na = make_na( y , obs_y  ) 
    
    Xy_na1 = cbind(X, y_na )
    Xy_true[[j]] = Xy
    Xy_na[[j]] = Xy_na1
    
  }
  
  
  XY_true = do.call(cbind, Xy_true)
  XY_export = do.call(cbind, Xy_na)
  
  
  XY_export[ is.na(XY_export) ] =  - 99
  
  write.table( XY_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( XY_true ,  namek_true , sep = ',', col.names = F,row.names = F)
}



