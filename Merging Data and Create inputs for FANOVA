library(fda)
#Combine all excel files into one excel file
setwd("~/Documents/Visible")
a = list.files("sideview")
dir = paste("./sideview/",a,sep="")
dir
n = length(dir)
#######merging csv files into one csv file, we have 456 plants
merge.data = read.csv(file = dir[1],head = T, sep = ",")
for (i in 1:n){
  new.data = read.csv(file = dir[i], header=T, sep=",")
  merge.data = merge(merge.data,new.data, by = "row.names",all = TRUE )
}
#write.csv(merge.data,file = "./sideview/messy_data.csv",row.names = T)
#Remove the first 457 columns' massy info
merge.d = merge.data[,-c(1:457)]
#the first plant has been recounted
merge.d1 = merge.d[,-c(1:(2*26))]
#write.csv(merge.d1,file = "./sideview/correctly_merged.csv",row.names = T)
###### Take every 1-4 columns as extracted info
res1 = 0
for (j in 1: n){
  res1 = cbind(res1, merge.d1[,c(((j-1)*2*26+j),((j-1)*2*26+j+1),((j-1)*2*26+j+2),((j-1)*2*26+j+3))])
}
res1 = res1[,-1]
#write.csv(res1,file = "./sideview/res1.csv",row.names = T)
###### order the data by the order of dates
res2 = res1
# target location: 1-4 as a plant's info, 5-8, 9-12,13-16,17-20, 
#                       1,5,9,13,17
# algorithm writing: i: 1,2,3,4,5
#  i*4-3
for ( j in 1:n ){
  res2[,c((j*4-3):(j*4-3+3))] = res2[order(res2[,j*4-3+2]),][,c((j*4-3):(j*4-3+3))]
}
#write.csv(res2,file = "./sideview/res2.csv",row.names = T)
###Add year on dates in order to use the function "as.Date()".
date_data = res2
for ( j in 1:n){
  date_data[,j*4-3+2] = paste("2016",res2[,j*4-3+2],sep = "-")
}
#write.csv(date_data,file = "./sideview/datawithoyear.csv",row.names = T)  
########
##Remove the time points with NA size values
date_data = date_data[-c(23:28),]
data_days = date_data
for ( j in 1:n ){
for ( i in 1: nrow(date_data)[1] ){
  data_days[,j*4-3+2][i] = as.numeric(difftime(as.Date(date_data[,j*4-3+2][i]), as.Date(date_data[,j*4-3+2][1]), units = "days"))
}
}
#write.csv(data_days,file = "./sideview/data_days.csv",row.names = T)
# plant id 59, genotype 43, has values of 0, delete this plant's info
data_days1 = data_days[,-c(233,234,235,236)]
write.csv(data_days1,file = "./sideview/data_days1.csv",row.names = F)  

#######Input
tY = matrix(NA,nrow = nrow(data_days1),ncol = n-1)
for ( i in 1 : n-1){
  tY[,i] = data_days1[,i*4]
}
Y = t(tY)

tX = matrix(NA,nrow = nrow(data_days1),ncol = 455)
for ( i in 1 : n-1){
  tX[,i] = data_days1[,i*4-2]
  }
X = t(tX)

#nfis the number of factors in the model

X_data = matrix(nrow = nrow(Y), ncol = nf)
for ( i in 1: nrow(Y)){
  X_data[i,] = unique(X[i,])
}

X = X_data

tDays = matrix(NA,nrow = nrow(data_days1),ncol = n-1)
for ( i in 1 : n-1){
  tDays[,i] = data_days1[,i*4-1]
}
Days = t(tDays)
