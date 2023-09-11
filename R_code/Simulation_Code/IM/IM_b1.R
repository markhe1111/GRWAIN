rm(list = ls())


# Re-shape quantile function with covariante imbalance

# generate X from uniform 0 to 10

n = 300
p = 100

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
 
setwd ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/IM")

 
 for(k in 1:10){
  
 
  namek = paste('IMb1_', k, '.csv', sep = '') 
  namek_true = paste('IMb1_', k, '_True.csv', sep = '') 
  
   multi_missings = lapply(1:p, function(x) {
    
    #y = runif(n,0,10)
    
    y = rexp(n, .01)
    
    p_ob=   1- log(y+1)/ max(log(y+1)) # probablity of being observed
    
    #p_ob =  1 - rank(y)/ n 
    
    p_ob[ y  >  quantile(y, .8)] = 0
    
    d = rbinom(n,1,prob=p_ob)  
    
    #cbind(y, p_ob, d)
    
    list(y  = y, pm = p_ob, d =d)
  }   )
  
  
  X_true  = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$y) ))
  M   = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$d) ))
  true_Pm =  data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$pm) ))
  
  
  
  X_msg = sapply(1:p, function(j)  make_na(X_true[,j], M[,j]  )  )
  X_true_norm = sapply(X_true, function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x ,na.rm = T)))
  X_export = X_true_norm
  X_export[is.na(X_msg)] = NA
  X_export[ is.na(X_export) ] =  - 99

  write.table( X_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( X_true_norm ,  namek_true , sep = ',', col.names = F,row.names = F)
 }



 
 