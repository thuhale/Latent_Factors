##Cross-validation, 405-fold. Since the degree of freedom is 13, we only leave out 10 obs at a time.
## use a rank-2 matrix for estimation

rm(list=ls())
library(truncnorm)
x = read.csv("math.csv")
x = data.frame(x)
x=x[,-1]
x=as.matrix(x)

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
lam = 825

##Cross-validation sampling

obsSample = function(m,n){
	obs = which(!is.na(m), F)
	pos=sample(obs,n, replace=F)
	return(pos)
}
temp = G
error = c(1:405)
n=10

for (l in 1:405){
	pos=obsSample(temp, n)
	validation = temp[pos]
	temp[pos]=NA
	
	
	##Matrix W is G with observed value in pos replaced by na.
	W=G
	W[pos]=NA
	
	##Perform IFA on W:
	tmp = W
	tmp[is.na(W)] = 0
	
	for( i in 1:5000){
  		s = svd(tmp)
  		s$d = thresh(s$d, lam)
  		low = s$u%*%diag(s$d)%*%t(s$v)
  		tmp[is.na(W)] = low[is.na(W)]
	}
	rmse = sqrt(sum((G[pos]-tmp[pos])^2)/n)/mean(G[pos])
	error[l]=rmse
}
write.csv(error, "rmse-5.csv")

