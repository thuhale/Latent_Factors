## Structure simulation using a rank-3 matrix, lambda = 590, lambda1=550, normalize by minus x
#resample

rm(list=ls())
library(truncnorm)

x = read.csv("~/Desktop/Latent_Factors/Data/math.csv")
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

lam = 590
tmp = G
obs = G[!is.na(G)]
tmp[is.na(G)] = 0

for( i in 1:5000){
  s = svd(tmp)
  s$d = thresh(s$d, lam)
  low = s$u%*%diag(s$d)%*%t(s$v)
  tmp[is.na(G)] = low[is.na(G)]
}
ss=svd(tmp)
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
Gnorm=Gnorm+x

lam1=550
tmp1 = Gnorm
obs1 = Gnorm[!is.na(Gnorm)]
tmp1[is.na(Gnorm)] = mean(Gnorm[!is.na(Gnorm)])

for( h in 1:5000){
  s1 = svd(tmp1)
  s1$d = thresh(s1$d, lam1)
  low1 = s1$u%*%diag(s1$d)%*%t(s1$v)
  tmp1[is.na(Gnorm)] = low1[is.na(Gnorm)]
}

ss1 = svd(tmp1)

##Generate random matrix
K = as.list(G)
K=K[!is.na(K)]
K=as.numeric(K)
mu = mean(K)
sigma = sd(K)
singular_matrix = matrix(, nrow = 100, ncol = 72)
singular_matrix1 = matrix(, nrow = 100, ncol = 72)
student_matrix = matrix(0, nrow = 1276, ncol = 72)
student_matrix1 = matrix(0, nrow = 1276, ncol = 72)



for(l in 1:100){
	W = matrix(rtruncnorm(91872, a=100, b = 500, mean = mu, sd = sigma), 1276, 72)
	W[is.na(G)]=NA    
	
	new_lam = 870
	new_tmp = W
	new_obs = W[!is.na(W)]
	new_tmp[is.na(W)] = mu

	for( i in 1:5000){
  	new_s = svd(new_tmp)
  	new_s$d = thresh(new_s$d, new_lam)
  	new_low = new_s$u%*%diag(new_s$d)%*%t(new_s$v)
  	new_tmp[is.na(W)] = new_low[is.na(W)]
	}
	new_ss = svd(new_tmp)
	singular_matrix[l,]=new_ss$d
	student_matrix = student_matrix+new_ss$u

	
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
	Wnorm=Wnorm+x

	new_lam1=850
	new_tmp1 = Wnorm
	new_obs1 = Wnorm[!is.na(Wnorm)]
	new_tmp1[is.na(Wnorm)] = mean(Wnorm[!is.na(Wnorm)])

	for( h in 1:5000){
 	 	new_s1 = svd(new_tmp1)
  		new_s1$d = thresh(new_s1$d, new_lam1)
  		new_low1 = new_s1$u%*%diag(new_s1$d)%*%t(new_s1$v)
  		new_tmp1[is.na(Wnorm)] = new_low1[is.na(Wnorm)]
	}
	new_ss1 = svd(new_tmp1)
	singular_matrix1[l,]=new_ss1$d
	student_matrix1 = student_matrix1+new_ss1$u


}
student_matrix = student_matrix/100
student_matrix1 = student_matrix1/100

write.csv(singular_matrix, "two-tier-structure-simulation.csv")
write.csv(singular_matrix1, "two-tier-structure-simulation1.csv")
write.csv(student_matrix, "student_dim.csv")
write.csv(student_matrix1, "student_dim1.csv")


print(ss1$d)
print(ss$d)

