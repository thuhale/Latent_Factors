##Cross-validation, 405-fold. Since the degree of freedom is 13, we only leave out 10 obs at a time.
## use normalization, rank-1 matrix
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
lam = 1360
lam1 = 600

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
	
	## Normalize matrix W
	Wnorm=W
	x=mean(tmp)
	y=rowMeans(tmp)
	z=colMeans(tmp)

	for(j in 1:1276){
		Wnorm[j,]=Wnorm[j,]-y[j]
	}
	
	for(k in 1:72){
		Wnorm[,k]=Wnorm[,k]-z[k]
	}
	Wnorm=Wnorm - x
	
	tmp1 = Wnorm
	obs1 = Wnorm[!is.na(Wnorm)]
	tmp1[is.na(Wnorm)] = 0

	for( h in 1:5000){
 	 	s1 = svd(tmp1)
		s1$d = thresh(s1$d, lam1)
		low1 = s1$u%*%diag(s1$d)%*%t(s1$v)
 		tmp1[is.na(Wnorm)] = low1[is.na(Wnorm)]
 	}
 	
 	recover = tmp1
 	for(j in 1:1276){
		recover [j,]=recover [j,]+y[j]
	}
	
	for(k in 1:72){
		recover [,k]=recover [,k] + z[k]
	}
	
	recover = recover + x
 	recover[recover>500]=500
	recover[recover<100] = 100
	
 	rmse = sqrt(sum((G[pos]-recover[pos])^2)/n)/mean(G[pos])
	error[l]=rmse
}
write.csv(error, "rmse-8.csv")

