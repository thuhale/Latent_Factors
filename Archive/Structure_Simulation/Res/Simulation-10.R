## Using a rank-2 matrix, lambda = 825, lambda1=400
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
tmp = G
obs = G[!is.na(G)]
tmp[is.na(G)] = 0

for( i in 1:5000){
  s = svd(tmp)
  s$d = thresh(s$d, lam)
  low = s$u%*%diag(s$d)%*%t(s$v)
  tmp[is.na(G)] = low[is.na(G)]
}
##Getting rid of fixed effect:
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

lam1=400
tmp1 = Gnorm
obs1 = Gnorm[!is.na(Gnorm)]
tmp1[is.na(Gnorm)] = 0

for( h in 1:5000){
  s1 = svd(tmp1)
  s1$d = thresh(s1$d, lam1)
  low1 = s1$u%*%diag(s1$d)%*%t(s1$v)
  tmp1[is.na(Gnorm)] = low1[is.na(Gnorm)]
}

ss1 = svd(tmp1)

##Generate random matrix
K = as.list(Gnorm)
K=K[!is.na(K)]
K=as.numeric(K)
mu = mean(K)
sigma = sd(K)

singular_matrix = matrix(, nrow = 100, ncol = 72)

for(l in 1:100){

	W = matrix(rtruncnorm(91872, a=100, b=500, mean = mu, sd = sigma), 1276, 72)
	W[is.na(G)]=NA
	
	new_lam = 400
	new_tmp = W
	new_obs = W[!is.na(W)]
	new_tmp[is.na(W)] = 0

	for( i in 1:5000){
  	new_s = svd(new_tmp)
  	new_s$d = thresh(new_s$d, new_lam)
  	new_low = new_s$u%*%diag(new_s$d)%*%t(new_s$v)
  	new_tmp[is.na(W)] = new_low[is.na(W)]
  	if(round(i/1000)%in% 1:100) plot(sqrt(new_s$d), main = paste(h))
	}
	new_ss = svd(new_tmp)
	singular_matrix[l,]=new_ss$d
	write.csv(singular_matrix, "singular_matrix-10.csv")
	print(ss1$d)
}

