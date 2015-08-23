//This simulation creates a matrix of same bi-partile graph as the original matrix, then fill random value of the observation. We will compare the singular values
rm(list=ls())
# Load data
library(XLConnect)
library(ggplot2)
library(grid)
library(gridExtra)
wb <- loadWorkbook("MATH.xlsx")
Tables <- readWorksheet(wb, sheet = "MATH", header = TRUE)

x = Tables
x = x[,-1]
x = as.matrix(x)

# basic graph properties:
dim(x)
table(rowSums(!is.na(x)))
table(colSums(!is.na(x)))
mean(rowSums(!is.na(x)))
mean(colSums(!is.na(x)))

#Drop student with 1 entry
B = (!is.na(x))
badstu = which(rowSums(B) < 2)
badtea = which(colSums(B) < 2)
G = x[-badstu, ]
G = G[,-badtea]
B = B[-badstu,]
B = B[,-badtea]
dim(G)

thresh = function(a, lam){
	a = a - lam
	a[a<0] = 0
	return(a)
}

lam = 590
tmp = G
obs = G[!is.na(G)]
tmp[is.na(G)] = 0

for( i in 1:5000){
  s = svd(tmp)
  s$d = thresh(s$d, lam)
  low = s$u%*%diag(s$d)%*%t(s$v)
  tmp[is.na(G)] = low[is.na(G)]
  if(round(i/1000)%in% 1:100) plot(sqrt(s$d), main = paste(i))
}


## Normalize the matrix
Gnorm=G
x=mean(tmp)
y=rowMeans(tmp)
w=colMeans(tmp)

for(j in 1:1276){
	Gnorm[j,]=Gnorm[j,]-y[j]
	}
	
for(k in 1:72){
	Gnorm[,k]=Gnorm[,k]-w[k]
	}
Gnorm=Gnorm-x

lam1=10000
tmp1 = Gnorm
obs1 = Gnorm[!is.na(Gnorm)]
tmp1[is.na(Gnorm)] = 0

for( h in 1:5000){
  s1 = svd(tmp1)
  s1$d = thresh(s1$d, lam1)
  low1 = s1$u%*%diag(s1$d)%*%t(s1$v)
  tmp1[is.na(Gnorm)] = low[is.na(Gnorm)]
  if(round(h/1000)%in% 1:100) plot(sqrt(s1$d), main = paste(h))
}

// Create a random matrix W:

K = as.list(G)
K=K[!is.na(K)]
K=as.numeric(K)
mu = mean(K)
sigma = sd(K)

library(truncnorm)
W = matrix(rtruncnorm(91872, a=100, b=500, mean = mu, sd = sigma), 1276, 72)
W[is.na(G)]=NA
dim(W)

new_lam = 590
new_tmp = W
new_obs = W[!is.na(W)]
new_tmp[is.na(W)] = 0

for( i in 1:5000){
  new_s = svd(new_tmp)
  new_s$d = thresh(new_s$d, new_lam)
  new_low = new_s$u%*%diag(new_s$d)%*%t(new_s$v)
  new_tmp[is.na(W)] = new_low[is.na(W)]
  if(round(i/1000)%in% 1:100) plot(sqrt(new_s$d), main = paste(i))
}

Wnorm=W
x=mean(new_tmp)
y=rowMeans(new_tmp)
w=colMeans(new_tmp)

for(j in 1:1276){
	Wnorm[j,]=Wnorm[j,]-y[j]
	}
	
for(k in 1:72){
	Wnorm[,k]=Wnorm[,k]-w[k]
	}
Wnorm=Wnorm-x

new_lam1=10000
new_tmp1 = Wnorm
new_obs1 = Wnorm[!is.na(Gnorm)]
new_tmp1[is.na(Wnorm)] = 0

for( h in 1:5000){
  new_s1 = svd(new_tmp1)
  new_s1$d = thresh(new_s1$d, new_lam1)
  new_low1 = new_s1$u%*%diag(new_s1$d)%*%t(new_s1$v)
  new_tmp1[is.na(Wnorm)] = low[is.na(Wnorm)]
  if(round(h/1000)%in% 1:100) plot(sqrt(new_s1$d), main = paste(h))
}

