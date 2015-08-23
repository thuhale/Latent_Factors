## Cross validation using the model: m_{jj} = mu+a_i+b_j
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
lam = 590

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
	x=c(1:1276)
	y=c(1:72)
	
	pos=obsSample(temp, n)
	validation = temp[pos]
	temp[pos]=NA
	
	
	W=G
	W[pos]=NA
	
		
	for (i in 1:1276){
		student = W[i,]
		x[i]=mean(student[!is.na(student)])
	}
	
	for (j in 1:72){
		teacher = W[,j]
		y[j] = mean(teacher[!is.na(teacher)])
	}
	
	z=mean(W[!is.na(W)])
	
	prediction = W
	
	for (i in 1:1276){
		for (j in 1:72){
			prediction[i,j]=x[i]+y[j]-z
		}
	}
	
	
	rmse = sqrt(sum((G[pos]-prediction[pos])^2)/n)/mean(G[pos])
	error[l]=rmse
}
write.csv(error, "rmse.csv")

