# Re-shape quantile function with covariante imbalance

# generate X from uniform 0 to 10

rm(list = ls())
n = 300
p = 50

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
#dir.create ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/Informative_Distributions/CI/")

setwd ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/CI/")


for(k in 1:50){
  
  namek = paste('CIb2_', k, '.csv', sep = '') 
  namek_true = paste('CIb2_', k, '_True.csv', sep = '') 
  
  multi_missings = lapply(1:p, function(j) {
     
    
    y = runif(n, 0, 10)
    x = rexp(n  ,  .1)
    pm=   1- log(x+1)/ max(log(x+1)) # probablity of being observed
    
    d = rbinom(n,1,prob=pm)  
    list(x=x,  y= y, pm = pm, d =d)
  }   )
  
  X_true  = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$x) ))
  Y_true = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$y) ))
  M   = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$d) ))
  true_Pm =  data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$pm) ))
  
  
  X_na = sapply(1:p, function(j)  make_na(X_true[,j], M[,j]  )  )
  Y_na =  sapply(1:p, function(j)  make_na(Y_true[,j], M[,j]  )  )
  
  XY_true = cbind(X_true, Y_true)
  XY_msg = cbind(X_na, Y_na) 
  
  XY_true = sapply(XY_true, function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x ,na.rm = T)))
  XY_export = XY_true
  XY_export[is.na(XY_msg)] = NA
  XY_export[ is.na(XY_export) ] =  - 99

  
  write.table( XY_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( XY_true ,  namek_true , sep = ',', col.names = F,row.names = F)
}




