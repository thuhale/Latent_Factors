## Cross validation using the model: m_{jj} = mu
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

	z=mean(W[!is.na(W)])
	
	prediction = W
	prediction[is.na(prediction)]=z
	
	rmse = sqrt(sum((G[pos]-prediction[pos])^2)/n)/mean(G[pos])
	error[l]=rmse
}
write.csv(error, "rmse.csv")

