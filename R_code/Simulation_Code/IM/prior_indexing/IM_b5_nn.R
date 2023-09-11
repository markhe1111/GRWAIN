rm(list = ls())

# Re-shape quantile function with covariante imbalance
# generate X from uniform 0 to 10


rm(list = ls())
n = 300
p = 100

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
 
 
setwd ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/IM/nonnorm")
 
for(k in 1 : 50){
 
  namek = paste('IMb5_', k, '.csv', sep = '') 
  namek_true = paste('IMb5_', k, '_True.csv', sep = '') 
  
   multi_missings = lapply(1:p, function(x) {
    
    #y = runif(n,0,10)
    
    y = rexp(n, .005)
    
    p_ob=   1- log(y+1)/ max(log(y+1)) # probablity of being observed
    
    p_ob[ y  >  quantile(y, .9)] = 0
    
    d = rbinom(n,1,prob=p_ob)  
    list(y  = y, pm = p_ob, d =d)
  }   )
  
  
  X_true  = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$y) ))
  M   = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$d) ))
  true_Pm =  data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$pm) ))
  
  
  X_true  = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$y) ))
  M   = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$d) ))
  true_Pm =  data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$pm) ))
  
  X_msg = data.frame( sapply(1:p, function(j)  make_na(X_true[,j], M[,j]  ) ))
  
  X_export   =  X_msg
  X_export[ is.na(X_export) ] =  - 99
  
  write.table( X_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( X_true ,  namek_true , sep = ',', col.names = F,row.names = F)
 }



 
 