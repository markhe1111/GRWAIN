# Re-shape quantile function with covariante imbalance

# generate X from uniform 0 to 10

n = 300
p1 = 100
p2 = 33
p3 = 33
p=100

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
 
setwd ("C:\\Users\\xiaoming\\Desktop\\makr he\\GRWAIN\\Simulations\\IM_T")


 
 for(k in 1:50){
  
 
  namek = paste('IMa_', k, '.csv', sep = '') 
  namek_true = paste('IMa_', k, '_True.csv', sep = '') 
  
   multi_missings1 = lapply(1:p1, function(x) {
    
    #y = runif(n,0,10)
    
    y = rt(n, df = 10)
    
    
    y = abs(y) 
    
    p_ob=  1 - (y+1)/ max(y+1) # probablity of being observed !!!9.5
    
    # half-half for n
    #p_ob0=  (y+1)/ max(y+1)
    #p_ob1 = 1-(y+1)/ max(y+1)
    #sam_id = sample(n, n/2, replace = F)
    #index = 1:n
    #p_ob = p_ob0 * (index == sam_id) + p_ob1 * (index != sam_id)
    
    
    
    #p_ob =  1 - rank(y)/ n 
    #p_ob[ y  >  quantile(y, .8)] = 0
    
    #cbind(y, p_ob, d)
    
    d = rbinom(n,1,prob=p_ob)  
    
    list(y  = y, pm = p_ob, d =d)
  }   )
  
  
  X_true  = data.frame( do.call( cbind ,  lapply(multi_missings1, function(x)  x$y) ))
  M   = data.frame( do.call( cbind ,  lapply(multi_missings1, function(x)  x$d) ))
  true_Pm =  data.frame( do.call( cbind ,  lapply(multi_missings1, function(x)  x$pm) ))
  
  multi_missings2 = lapply(1:p2, function(x) {
    
    #y = runif(n,0,10)
    
    y = rexp(n, .1)
    
    p_ob=   (y+1)/ max(y+1) # probablity of being observed !!!9.5
    
    # half-half for n
    #p_ob0=  (y+1)/ max(y+1)
    #p_ob1 = 1-(y+1)/ max(y+1)
    #sam_id = sample(n, n/2, replace = F)
    #index = 1:n
    #p_ob = p_ob0 * (index == sam_id) + p_ob1 * (index != sam_id)
    
    
    
    #p_ob =  1 - rank(y)/ n 
    #p_ob[ y  >  quantile(y, .8)] = 0
    
    #cbind(y, p_ob, d)
    
    d = rbinom(n,1,prob=p_ob)  
    
    list(y  = y, pm = p_ob, d =d)
  }   )
  
  
  X_true2  = data.frame( do.call( cbind ,  lapply(multi_missings2, function(x)  x$y) ))
  M2   = data.frame( do.call( cbind ,  lapply(multi_missings2, function(x)  x$d) ))
  true_Pm2 =  data.frame( do.call( cbind ,  lapply(multi_missings2, function(x)  x$pm) ))


  multi_missings3 = lapply(1:p3, function(x) {
    
    #y = runif(n,0,10)
    
    y = rexp(n, .1)
    
    p_ob=   runif(n) # probablity of being observed !!!9.5
    
    # half-half for n
    #p_ob0=  (y+1)/ max(y+1)
    #p_ob1 = 1-(y+1)/ max(y+1)
    #sam_id = sample(n, n/2, replace = F)
    #index = 1:n
    #p_ob = p_ob0 * (index == sam_id) + p_ob1 * (index != sam_id)
    
    
    
    #p_ob =  1 - rank(y)/ n 
    #p_ob[ y  >  quantile(y, .8)] = 0
    
    #cbind(y, p_ob, d)
    
    d = rbinom(n,1,prob=p_ob)  
    
    list(y  = y, pm = p_ob, d =d)
  }   )
  
  
  X_true3  = data.frame( do.call( cbind ,  lapply(multi_missings3, function(x)  x$y) ))
  M3  = data.frame( do.call( cbind ,  lapply(multi_missings3, function(x)  x$d) ))
  true_Pm3 =  data.frame( do.call( cbind ,  lapply(multi_missings3, function(x)  x$pm) ))
  
  #X_true = cbind(X_true1, X_true2,)
  #M = cbind(M1, M2)
  #true_Pm = cbind(true_Pm1, true_Pm2)

  #X_true = cbind(X_true1, X_true2, X_true3)
  #M = cbind(M1, M2, M3)
  #true_Pm = cbind(true_Pm1, true_Pm2, true_Pm3)
  
  X_msg = sapply(1:p, function(j)  make_na(X_true[,j], M[,j]  ))
  X_true_norm = sapply(X_true, function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x ,na.rm = T)))
  X_export = X_true_norm
  X_export[is.na(X_msg)] = NA
  X_export[is.na(X_export) ] =  - 99
 
  
  write.table( X_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( X_true_norm ,  namek_true , sep = ',', col.names = F,row.names = F)
 }



 
 