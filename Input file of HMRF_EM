setwd()
##### Iput the matrices ####
library(imager)
library(dplyr)
library(spatstat)
library(png) 
source("kmeans.R")
source("MAP.R")
source("EM.R")
#Load the image
orig = load.image("~/Desktop/Visible/image_sample_reduced.png")
I = orig[,,1,2]/(orig[,,1,1]+orig[,,1,2]+orig[,,1,3])
#I = grayscale(orig)
#Y = I[,,1,1] 
#Y=t(Y)
Y = t(I)
k = 2
map_iter = 20
em_iter = 20
sp = 0
#Z = cannyEdges(I) 
Z = cannyEdges(orig)
Z = Z[,,1,1]
Z = t(Z*1)
X = imkmeans(Y,k)$X
mu = imkmeans(Y,k)$mu
sigma = imkmeans(Y,k)$sigma
output = matrix(as.numeric(X),nrow = nrow(X), ncol = ncol(X))-1
writePNG(output, target = "initalimage.png")
system.time({
  em = EM(X,Y,Z,mu,sigma,k,em_iter,map_iter,epsilon_em = 0.00000001)
})
X = em$X
Xoutput = X-1
mu = em$mu
sigma = em$sigma

# Get the Image
OUTPUT = matrix(as.numeric(Xoutput),nrow = nrow(Xoutput), ncol = ncol(Xoutput))
pic = writePNG(OUTPUT,target = "finalimage_hmrf3.png")
writePNG(color_exchange(OUTPUT),"hmrf.png")
plot = em$plot
