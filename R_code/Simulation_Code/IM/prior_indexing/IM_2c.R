# Re-shape quantile function with covariante imbalance

# generate X from uniform 0 to 10

n = 300
p = 100

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
 
setwd ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/IM")

 
 for(k in 1:3){
  
 
  namek = paste('IM2c_', k, '.csv', sep = '') 
  namek_true = paste('IM2c_', k, '_True.csv', sep = '') 
  
   multi_missings = lapply(1:p, function(x) {
    
    #y = runif(n,0,10)
    
    y = rexp(n, .01)
    p_ob=   1- log(y+1)/ max(log(y+1)) # probablity of being observed
    
    #p_ob[ y  >  quantile(y, .8)] = 0
    
    d = rbinom(n,1,prob=p_ob)  
    
    #cbind(y, p_ob, d)
    
    list(y  = y, pm = p_ob, d =d)
  }   )
  
  
  X_true  = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$y) ))
  M   = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$d) ))
  true_Pm =  data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$pm) ))
  
  
 
  X_export = sapply(1:p, function(j)  make_na(X_true[,j], M[,j]  )  )
  X_export[ is.na(X_export) ] =  - 99

  write.table( X_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( X_true ,  namek_true , sep = ',', col.names = F,row.names = F)
 }



 


notrun = function(){
  
  Pm = cbind( true_Pm , true_Pm)
  
  XY_export[ is.na(XY_export) ] =  - 99
  
  
  j = 10
  
  x = X_true[,j]
  y = Y_true[,j]
  d = M[,j]
  pm = true_Pm[,j]
  #d = rbinom(n,1,prob=pm) # d is observed probab
  
  y1=y[d==1]; 
  x1=x[d==1]; 
  pi1=pm[d==1]
  
  y2=y[d==0]; 
  x2=x[d==0]; 
  pi2=1-pm[d==0]
  
  
  
  
  
  single_case = function(){
    
    qqplot(y1,y2); abline(0,1)
    qqplot(x1,x2); abline(0,1)
    
    
    # Re-shape quantile function of observed ones
    target.taus=seq(0,1, by=0.02)
    
    w1 = (1/pi1)/sum(1/pi1)
    
    # reshape quantile of X
    or = order(x1)
    x11 = x1[or]; w11= w1[or]
    q.ext = c(0,cumsum(w11)) # add quantile level 0
    x11.ext=c(min(x),x11)         # assign the smallest value to the quantile level 0
    xout.Q1=approx(q.ext,x11.ext,xout=target.taus)
    
    # reshape quantile of Y
    or = order(y1)
    y11 = y1[or]; w11= w1[or]
    q.ext = c(0,cumsum(w11)) # add quantile level 0
    y11.ext = c(min(y),y11)   
    yout.Q1=approx(q.ext,y11.ext,xout=target.taus)
    
    # Re-shape quantile function of observed ones
    
    
    w2 = (1/(pi2))/sum(1/(pi2))
    
    or = order(x2)
    x21 = x2[or]; w21= w2[or]
    q.ext = c(0,cumsum(w21)) # add quantile level 0
    x21.ext=c(min(x),x21)  # assign the smallest value to the quantile level 0
    xout.Q2=approx(q.ext,x21.ext,xout=target.taus)
    
    or = order(y2)
    y21 = y2[or]; w21= w2[or]
    q.ext = c(0,cumsum(w21)) # add quantile level 0
    y21.ext=c(min(y),y21)  # assign the smallest value to the quantile level 0
    yout.Q2=approx(q.ext,y21.ext,xout=target.taus)
    
    
    #QQ plot for x
    
    qqplot(x1,x2, xlab = "observed x (x1)", ylab ="missing x (x2)", main = "QQ plot")
    abline(0,1)
    # QQ plot after adjustment (red points)
    points(xout.Q1$y,xout.Q2$y,col=2, main="QQ plot ")
    
    #QQ plot for y
    
    qqplot(y1,y2, xlab = "observed y (y1)", ylab ="missing y (y2)", main = "QQ plot")
    abline(0,1)
    # QQ plot after adjustment (red points)
    points(yout.Q1$y,yout.Q2$y,col=2, main="QQ plot ")
    
  }
  
}