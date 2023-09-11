# Re-shape quantile function with covariante imbalance

# generate X from uniform 0 to 10

# YOU KNOW CONTROLS DO BADLY
BUT WHAT IF REALLY LIGHT 

rm(list = ls())
n = 300
p = 100
library(faux)

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
#dir.create ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/Informative_Distributions/CI/")

 
 setwd ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/Control/")


for(k in 1:50){
  
  namek = paste('UnifCtrl_', k, '.csv', sep = '') 
  namek_true = paste('UnifCtrl_', k, '_True.csv', sep = '') 
  means = rnorm(p , 0, 1)
  
   
  multi_missings = lapply(1:p, function(j) {
      
    y = runif(n, 0 , 1)
    p_ob = rep(0,n)
    p_ob[ y  >  quantile(y, .7)] = .5
    
    #pm = rbinom(n,1,prob = .5)
    d = rbinom(n,1,prob=p_ob)
    list(x=y,  pm = p_ob , d =d)
  }   )
   
  X_true  = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$x) ))
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




