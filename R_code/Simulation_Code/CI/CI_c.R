
rm(list = ls())
# Re-shape quantile function with covariante imbalance

# generate X from uniform 0 to 10

n = 300
p = 50

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
#dir.create ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/Informative_Distributions/CI/")

setwd ("C:/Users/th2953/Dropbox/Columbia/WGAN_IQ/Data/Simulations/CI/")


for(k in 11:50){
  
  namek = paste('CIc_', k, '.csv', sep = '') 
  namek_true = paste('CIc_', k, '_True.csv', sep = '') 
  
   
  multi_missings = lapply(1:p, function(j) {
    
    x = rexp(n,1)
    y = rexp(n,  .01)
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
   
  additional_msg = function(X_na,Y_na, j){
   
    non_msg  = which(!is.na(X_na[,j]))
    Yj_additional_msg_prob   =   1- rank(na.omit(X_na[,j])) /length(na.omit( (X_na[,j])))
    M_add = rbinom(length(non_msg),1,prob=Yj_additional_msg_prob)  
    Y_double_msg = make_na( Y_na[,j] [non_msg]  , M_add    )
    Y_na[,j] [non_msg] = Y_double_msg
    Y_na[,j]
  }
   
  
  Y_na2 =  data.frame(sapply(1:p, function(j)  additional_msg ( X_na, Y_na, j  )  ))
  #Y_na2
  
  XY_true = cbind(X_true, Y_true)
  XY_msg = cbind(X_na, Y_na2) 
  
  XY_true = sapply(XY_true, function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x ,na.rm = T)))
  XY_export = XY_true
  XY_export[is.na(XY_msg)] = NA
  XY_export[ is.na(XY_export) ] =  - 99
  
  write.table( XY_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( XY_true ,  namek_true , sep = ',', col.names = F,row.names = F)
}


