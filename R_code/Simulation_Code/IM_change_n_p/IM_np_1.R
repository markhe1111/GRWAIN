# Re-shape quantile function with covariante imbalance

# generate X from uniform 0 to 10

n = 5000
p = 40

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
 
 
 
setwd ("C:/Users/markh/Dropbox/Columbia/GRWAIN/Simulations/IM_np/")

 
for(k in 1:50){
  
 
  namek = paste('IMnp1_', k, '.csv', sep = '') 
  namek_true = paste('IMnp1_', k, '_True.csv', sep = '') 
  
   multi_missings = lapply(1:p, function(x) {
    
     
    y = rexp(n, .01)
    
    
    p_ob=   1 -  (y+1)/ max( (y+1)) # probablity of being observed
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



 
double_check = function(){

  
  
  View(cbind(X_true[,1], M[,1])  )
  
  hist( X_true[,1] [M[,1]==1] ) 
  hist( X_true[,1] [M[,1]==0] ) 
  
  
  mean( X_true[,1] [M[,1]==1] ) 
  mean( X_true[,1] [M[,1]==0] ) 
  
}

 