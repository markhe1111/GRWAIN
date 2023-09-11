
n = 500
p = 50


pos_msgs = lapply(1:p, function(x) {
   
  y = rexp(n, .1)
  
  p_ob=   1-  (y+1)/ max( (y+1)) # probablity of being observed
  
  d = rbinom(n,1,prob=p_ob)  
  
   
  list(y  = y, pm = p_ob, d =d)
}   )




neg_msgs = lapply(1:p, function(x) {
  
  y = rexp(n, .1)
  
  p_ob=   (y+1)/ max( (y+1)) # probablity of being observed
  
  d = rbinom(n,1,prob=p_ob)  
  
  
  list(y  = y, pm = p_ob, d =d)
}   )



pos_msgs[[1]]$y
pos_msgs[[1]]$d

X1 = do.call(cbind, lapply(pos_msgs, function(x) x$y))
X2 = do.call(cbind, lapply(neg_msgs, function(x) x$y))

d1 = do.call(cbind, lapply(pos_msgs, function(x) x$d))
d2 = do.call(cbind, lapply(neg_msgs, function(x) x$d))




X = data.frame(X1,X2)
M = data.frame(d1,d2)

p

observed_only = lapply(1:(2*p), function(j)  X [ M[,j]==1 , j ] )


plot(ecdf(observed_only[[1]]))


plot(ecdf(observed_only[[51]]))

plot(ecdf(observed_only[[51]]))


plot(ecdf(observed_only[[61]]))

O = observed_only

O = lapply(observed_only, function(x) (x-min(x))/(max(x) - min(x)) )


library(e1071)
 


varO = sapply(1:100, function(j)  var(O[[j]]  ))
plot(1:100, varO)

kurtO = sapply(1:100, function(j)  kurtosis(O[[j]]  ))
plot(1:100, kurtO)

skewO = sapply(1:100, function(j)  skewness(O[[j]]  ))
plot(1:100, skewO)

meanO = sapply(1:100, function(j)  mean(O[[j]]  ))
plot(1:100, meanO)


O1 = observed_only

var1 = sapply(O1, var)
sd1 = sqrt(var1)
mean1 = sapply(O1, mean)

library(caret)
library(glmnet)
library(factoextra)
library(gridExtra)
library(corrplot)
library(RColorBrewer) 
library(gplots)
library(jpeg)
library(ggplot2)
library(patchwork)


direction = matrix(c(rep(1, 50), rep(0, 50)), nrow = 100)#positive is 1

set.seed(1)
trainRows <- createDataPartition(y = direction, p = 0.8, list = FALSE)## train label

data = data.frame(direction, varO, meanO)
data = data[sample.int(dim(data)[1], replace = F),]





## method 1 logistic regression


## do not need standardize
## potential mean = 10

mean_diff = matrix(mean1 - 10, nrow = 100)
sd1 = matrix(sd1, nrow = 100)

data1 = data.frame(direction, mean_diff, sd1)
data1 = data1[sample.int(dim(data1)[1], replace = F),]

train_mean_diff = mean_diff[trainRows, ]
train_sd = sd1[trainRows, ]
train_direction = facotr(direction[trainRows, ])

test_mean_diff = mean_diff[-trainRows, ]
test_sd = sd1[-trainRows, ]
test_direction = factor(direction[-trainRows, ])

train_direction = direction[trainRows, ]

fit1 = glm(direction ~ mean_diff + sd1, data = data1[trainRows,], family = "binomial")

# not converge

## method2 
normalied_diff = train_mean_diff/train_sd
test_normalied_diff = test_mean_diff/test_sd

cut_point = (mean(normalied_diff[train_direction == 1]) + mean(normalied_diff[train_direction == 0]))/2


direction_pred2 = factor(1 * (test_normalied_diff <= cut_point))

confusionMatrix(direction_pred2, test_direction)

## method 3 SVM

set.seed(1)
linear.tune = tune.svm(direction ~ . , 
                        data = data[trainRows,], 
                        kernel = "linear", 
                        cost = exp(seq(-5,2,len = 50)),
                        scale = TRUE)

best.linear = linear.tune$best.model
pred.linear = predict(best.linear, newdata = data[-trainRows,])

confusionMatrix(data = factor(as.numeric(pred.linear > 0.5)), 
                reference = factor(data$direction[-trainRows]))


## method 3 cluster
## K-means
fviz_nbclust(data[,2:3],
             FUNcluster = kmeans,
             method = "silhouette")

km = kmeans(data[,2:3], centers = 2, nstart = 20)

km_vis = fviz_cluster(list(data = data[,2:3], cluster = km$cluster), 
                       ellipse.type = "convex", 
                       geom = c("point","text"),
                       labelsize = 5, 
                       palette = "Dark2") + labs(title = "K-means") 


scattor_plot = ggplot(data, aes(x = varO, y = meanO, color = direction)) +
  geom_point(size = 1)

scattor_plot + km_vis



