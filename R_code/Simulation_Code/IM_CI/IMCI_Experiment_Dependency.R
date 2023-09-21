# Re-shape quantile function with covariante imbalance
# generate X from uniform 0 to 10

n = 300
p = 50

 make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}
 
library(faux) 
 
setwd ("C:/Users/markh/Dropbox/Columbia/WGAN_IQ/Data/Simulations/IMCI/")

setwd ('Simulations/IMCI_experiment')
 
k = 1
 
 


IMCI_1 = function(){
   
  
  x = runif(n, 0, 1)
  y = 1+  x + rnorm(n)
  pm=   1- log(x+1)/ max(log(x+1)) # probablity of being observed
  d = rbinom(n,1,prob=pm)  
  
  
  multi_missings = lapply(1:p, function(x) {
    
    x = runif(n, 0, 1)
    y = 1+  x + rnorm(n)
    pm=   1- log(x+1)/ max(log(x+1)) # probablity of being observed
    d = rbinom(n,1,prob=pm)  
    list(x=x,  y= y, pm = pm, d =d)
  }   )

  X_true  = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$x) ))
  Y_true = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$y) ))
  M   = data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$d) ))
  true_Pm =  data.frame( do.call( cbind ,  lapply(multi_missings, function(x)  x$pm) ))
  
}


# a weird IMCI
x = runif(n)
y = rexp(n)
msg_x = ( x  +1)/ max( ( x +1))
msg_y = ( x  +1)/ max( ( x +1))


# simplest case of CI
x = runif(n)
y = rexp(n)
# x is fully observed 
Pmsg_y =  1- ( x  +1)/ max( ( x +1))

# for IMCI
Psmg_y =  1- ( x + y  +1)/ max( ( x+y +1))

msg_y = rbinom(n, 1, Pmsg_y)

plot(Pmsg_y, y)
plot(Pmsg_y, x)
plot(msg_y, x)


summary( glm( msg_y  ~   x   , family = 'binomial') )



summary( glm( msg_y  ~   x   , family = 'binomial') )

#   X1,X2,X3 ~ MVN(rho .25 ), y 
# msg (y ) ~  X1,X2,X3 
# X1

onlyCI_case  = function(){
  
  means = rnorm(5 , 0, 1)
  
  X = rnorm_multi(n = 300  , mu = means,    r =  -0.2 )
  X_norm = sapply(X, function(x) (x-min(x)) / (max(x) - min(x)) )
  X_norm
  
  y =   rexp(n  ,  .1)
  
  y = (y - min(y)) / (max(y) - min(y)) 
  
  
  sumX = 1+  X_norm[,1] + X_norm[,2] 
  
  p_msg_y=    1-( sumX  +1)/ max( ( sumX +1)) # probablity of being observed
  
  msg_y = rbinom(n,1,  p_msg_y)
  
  
  par(mfrow=c(2,1))
  hist( y[msg_y==1] , xlim = c(0,1)) 
  hist( y[msg_y==0], xlim=c(0,1),  col = rgb(0,0,1,.5) ) 
  
  
  
  summary( glm( msg_y  ~   X_norm   , family = 'binomial') )
  
  library(glmnet)
  lasso_msg = cv.glmnet(X_norm,  msg_y     , family = 'binomial') 
  coef(lasso_msg)
  
  hist(X_norm[,1])
  hist(X_norm[,2])
  hist(X_norm[,1])
  hist(X_norm[,1])

  hist(X_norm[,1])
  
  summary( glm( msg_y  ~   X_norm   , family = 'binomial') )
  
  
   
   
}




CI2_case =  function(){
    
  
  means = rnorm(3 , 0, 1)
  
  X = rnorm_multi(n = 300  , mu = means,    r =  0.2 )
  X_norm = sapply(X, function(x) (x-min(x)) / (max(x) - min(x)) )
   
  y =   rexp(n  ,  .1)
  
  y = (y - min(y)) / (max(y) - min(y)) 
  
  
  #sumX = 1+  X_norm[,1] + X_norm[,2] 

  sumX = apply(  X_norm, 1, max)
  
    
  p_obs_y=  1-  ( sumX  +1)/ max( ( sumX +1)) # probablity of being observed
  
  View(cbind(sumX, p_obs_y, obs_y  ))
  
  obs_y = rbinom(n,1,  p_obs_y)
  
  
  par(mfrow=c(2,1))
  hist( y[obs_y==1] , xlim = c(0,1)) 
  hist( y[obs_y==0], xlim=c(0,1)  , col = rgb(0,0,1,.5)  ) 
  
  
  plot( density( y[msg_y==1] , xlim = c(0,1), col = rgb(0,0,1,.5)) )
  plot(density( y[msg_y==0], xlim=c(0,1)    ) )
  
  summary( glm( obs_y  ~   X_norm   , family = 'binomial') )
  
  library(glmnet)
  lasso_msg = cv.glmnet(X_norm,  msg_y  , family = 'binomial') 
  coef(lasso_msg)
  
}




IMCI_case =  function(){
  
  
  means = rnorm(10 , 0, 1)
  
  X = rnorm_multi(n = 300  , mu = means,    r =  0.2 )
  X_norm = sapply(X, function(x) (x-min(x)) / (max(x) - min(x)) )
  
  #y =   rexp(n  ,  .1)
  #y = (y - min(y)) / (max(y) - min(y)) 
  
  
  #sumX = 1+  X_norm[,1] + X_norm[,2] 
  
  y = apply(  X_norm, 1, max)
  
  
  p_obs_y=  ( sumX  +1)/ max( ( sumX +1)) # probablity of being observed
  
  obs_y = rbinom(n,1,  p_obs_y)
  
  
  par(mfrow=c(2,1))
  hist( y[obs_y==1] , xlim = c(0,1)) 
  hist( y[obs_y==0], xlim=c(0,1)  , col = rgb(0,0,1,.5)  ) 
  
  
  plot( density( y[msg_y==1] , xlim = c(0,1), col = rgb(0,0,1,.5)) )
  plot(density( y[msg_y==0], xlim=c(0,1)    ) )
  
  summary( glm( obs_y  ~   X_norm   , family = 'binomial') )
  
  library(glmnet)
  lasso_msg = cv.glmnet(X_norm,  msg_y  , family = 'binomial') 
  coef(lasso_msg)
  
}





imci_3 = function(){
  multi_missings = lapply(1:p, function(x) {
    
    x = rexp(n,.2)
    y = 1+  2*x+rexp(n,.1)
    pm=   1- log(x+1)/ max(log(x+1)) # probablity of being observed
    
    d = rbinom(n,1,prob=pm)  
    list(x=x,  y= y, pm = pm, d =d)
  }   )
  
  
}






 for(k in 1:3){
  
 
  namek = paste('eIMCI1_', k, '.csv', sep = '') 
  namek_true = paste('eIMCI1_', k, '_True.csv', sep = '') 
  
  
  multi_missings = lapply(1:p, function(x) {
    
    x = runif(n, 0, 1)
    y = 1+  x + rnorm(n)
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
  
  
  
  
  
  
  
  
  
  
  
  
  XY_export = cbind(X_na, Y_na) 
  XY_export[ is.na(XY_export) ] =  - 99
  
  write.table( XY_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( XY_true ,  namek_true , sep = ',', col.names = F,row.names = F)
}


 
 
whaaat = function(){
#

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

