setwd()
library(matrixcalc)
imkmeans<-function(Y,k){
 y = vec(t(Y))
 x = kmeans (y, k)$cluster
 a = nrow(Y)
 b = ncol(Y)
 X = matrix (x, nrow= a, ncol= b, byrow = TRUE)
 mu = matrix (0, nrow = k, ncol = 1)
 sigma = matrix(0, nrow = k, ncol = 1)
 for ( i in 1:k ){
   yindex = y[x == i]
   mu[i] = mean(yindex)
   sigma[i] = sd(yindex)
 }
 mylist3<-list("X"=X,"mu"=mu,"sigma"=sigma)
 return(mylist3)
}
