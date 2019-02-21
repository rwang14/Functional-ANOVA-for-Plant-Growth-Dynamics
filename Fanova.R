library(fda)
#Y is the response data, total.time is the total observed days.
#tt, a vector in which contain the evaluation time points.
#K.int is the number of interior knots.
#X, a dataframe. Each column of it is a cateogrical factor of your model.
#Days, a matrix which reflects the observed days for each plant.
#interact = 1 means you want all the interactions. Otherwise, let interact = 0.
#p, only works when interac = 1. p means the power of the interaction. e.g. If you want your model be up to 3-way interaction, let p = 3. If interac = 0, this option will be omitted.
#d0 a non-negative integer. = 0 if you want to evaluate the original basis function. = 1 if first derivative and 2 if second derivative.
#lower, the lower bound of the possible values of the log of tunning parameter, labmda.
#upper,the upper bound of the possible values of the log of tunning parameter, lambda.
#formula, a string which describes the factors you want to consider in your model. If interact = 1, omit this option.
fanova_mean = function(Y, regular = 1, tt, K.int, n, order = 4, total.time, Days,
                       X, formula, interact = 0, p = 2, d0 = 0, d1 = 2, d2 = 2, lower, upper, n.eval = 100)
{
  J = length(tt)	# number of time points
  ### basis functions ###
  knots = total.time*(1:K.int)/(1+K.int)
  # K is the number of basis functions in a spline basis system
  K = length(knots) + order # number of basis functions
  basis = create.bspline.basis(c(0,total.time), K, norder = order)
  
  design_matrix = NULL
  if (interact == 1) {
    design_matrix = model.matrix(as.formula(paste("~.^", p, sep='')), data = X)
  }
  if (interact == 0){
    design_matrix = model.matrix(as.formula(formula), data = X)
  }
  ## evaluate original, the first, and second derivatives of basis functions ##
  BS = eval.basis(tt,basis,d0)
  
  ## penalty matrix ###
  Omega = inprod(basis,basis,d1,d2)	# for second derivative, using 4 rather than 2, 2 means the original function
  
  ### Design matrix ###
  if(regular == 1)
  {
    Y.vec = as.vector(t(Y))
    Xmat = design_matrix %x% BS
  }
  
  if(regular == 0){
    Y.vec = as.vector(t(Y))
    BS = eval.basis(as.numeric(Days[1,][!is.na(Days[1,])]),basis,d0)
    for ( i in 2: nrow(Y) ){
      BSnew = eval.basis(as.numeric(Days[i,][!is.na(Days[i,])]),basis,d0)
      BS = rbind(BS,BSnew)
    }
    
    nf = ncol(X)
    BSmat = BS
    for ( i in 2: (nf+1)){
      BSnew1 = BS
      BSmat = cbind(BSmat,BSnew1)
    }
    
    #####Constructing the design_matrix
    m = matrix(NA, nrow = nrow(Days) , ncol = 1)
    for ( i in 1: nrow(Days)){
      m[i] = length(as.numeric(Days[i,][!is.na(Days[i,])]))
    }
    
    des_X = matrix(NA, nrow = nrow(BSmat), ncol = ncol(BSmat))
    for(j in 1:ncol(design_matrix)){
      for ( i in 1:1){
        des_X[(1:m[i]),((j-1)*K+1):((j-1)*K+1):(j*K)] = design_matrix[i,j]
      }
      for ( i in 2:nrow(design_matrix)){
        des_X[(1:m[i]),(1:K)] = design_matrix[i,1] 
        des_X[(sum(m[1:(i-1)])+1):(sum(m[1:i])),((j-1)*K+1):(j*K)] = design_matrix[i,j]
      }
    }
    
    Xmat = des_X * BSmat  
  }
  
  ### Penalized least squares estimates ###
  tuning_nointer = function(lower, upper, Omega, Xmat, Y.vec){
    lam.list=exp(seq(lower,upper,1))
    gcv=rep(0,length(lam.list))
    for(ii in 1:length(lam.list)){
      Omega_lam = matrix(0, nrow = ncol(design_matrix)*nrow(Omega),ncol = ncol(design_matrix)*ncol(Omega))  
      for ( i in 1: ncol(design_matrix)){
        Omega_lam[((1+(i-1)*dim(Omega)[1]):(i*dim(Omega)[1])),((1+(i-1)*dim(Omega)[2]):(i*dim(Omega)[2]))] = Omega*lam.list[ii]
      }
      A <- solve(t(Xmat) %*% Xmat + Omega_lam)
      Y.vec.hat <- (Xmat%*%A) %*% (t(Xmat)%*%Y.vec)
      diag.mean <- sum(diag(t(Xmat)%*%Xmat%*%A))/(dim(Xmat)[1])
      gcv[ii] <- mean((Y.vec-Y.vec.hat)^2)/(1-diag.mean)^2
    }
    ind=which(gcv == min(gcv))
    lam.list[ind]
  }
  #Finding the tunning parameter lambda
  lam = tuning_nointer(lower,upper,Omega,Xmat,Y.vec)
  #Using lam to define the matrix adiag(Omega*lambda)
  Omegabylam = matrix(0, nrow = ncol(design_matrix)*nrow(Omega),ncol = ncol(design_matrix)*ncol(Omega))  
  for ( i in 1: ncol(design_matrix)){
    Omegabylam[((1+(i-1)*dim(Omega)[1]):(i*dim(Omega)[1])),((1+(i-1)*dim(Omega)[2]):(i*dim(Omega)[2]))] = Omega*lam
  }
  bhat = solve(t(Xmat)%*%Xmat+Omegabylam)%*%t(Xmat)%*%Y.vec
  
  ########### estimated curve ###########
  t.eval = total.time*seq(0,1,by=1/n.eval)	# evaluation time points
  m = length(t.eval)	# number of evaluation time points
  BS.eval = eval.basis(t.eval,basis,d0)
  #BS1.eval = eval.basis(t.eval,basis,1)
  #BS2.eval = eval.basis(t.eval,basis,2)
  t.eval = t.eval + 1		
  #each column is an estimated coefficient, say, hat(mu(t))
  para = matrix(0,nrow = m,ncol = ncol(design_matrix))
  for ( i in 1:ncol(design_matrix)){
    para[,i] = BS.eval %*% bhat[((i-1)*K+1):((i-1)*K+K)]
  }
  
  #Creating a dummy table to check X
  combination = expand.grid(X)
  uniq_comb = unique(combination)
  dummy_comb = data.frame(trt_comb = uniq_comb, cat = unique(model.matrix(as.formula(formula),data = uniq_comb)))
  # return the matrix of estimated mean functions
  return(list(est_fun = para,dummy_table = dummy_comb,lambda = lam, bhat = bhat,S = solve(t(Xmat)%*%Xmat+Omegabylam)%*%t(Xmat)))
}

#Example

analysis = fanova_mean(Y, regular = 0, tt = c(0:15,17:20), K.int = 6, n, order = 4, total.time = 20, Days,
                       X, interact = 0, p = 2, d0 = 0, d1 = 2, d2 = 2, lower = -10, upper = 15, n.eval = 100, formula = formula)
para = analysis$est_fun
dummy_table = analysis$dummy_table
