rm(list=ls())
# Load data
library(XLConnect)
wb <- loadWorkbook("~/Desktop/Latent_Factors/data/MATH.xlsx")
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
tmp[is.na(G)] = mean(G[!is.na(G)])

for( i in 1:5000){
  s = svd(tmp)
  s$d = thresh(s$d, lam)
  low = s$u%*%diag(s$d)%*%t(s$v)
  tmp[is.na(G)] = low[is.na(G)]
  if(round(i/1000)%in% 1:100) plot(sqrt(s$d), main = paste(i))
}
plot(sqrt(s$d))
title("Analysis - Lambda = 200")

## Normalize the matrix
Gnorm=G
x=mean(tmp)
y=rowMeans(tmp)
z=colMeans(tmp)

for(j in 1:1276){
	Gnorm[j,]=Gnorm[j,]-y[j]
	}
	
for(k in 1:72){
	Gnorm[,k]=Gnorm[,k]-z[k]
	}
Gnorm=Gnorm+x

lam1 = 550
tmp1 = Gnorm
obs1 = Gnorm[!is.na(Gnorm)]
tmp1[is.na(Gnorm)] = mean(Gnorm[!is.na(Gnorm)])
for( h in 1:5000){
  s1 = svd(tmp1)
  s1$d = thresh(s1$d, lam1)
  low1 = s1$u%*%diag(s1$d)%*%t(s1$v)
  tmp1[is.na(Gnorm)] = low1[is.na(Gnorm)]
  if(round(h/1000)%in% 1:100) plot(sqrt(s1$d), main = paste(h))
}
wb2<- loadWorkbook("~/Desktop/Latent_Factors/data/MATH.xlsx")
Tables2 <- readWorksheet(wb2, sheet = getSheets(wb2))
Year = Tables2$"Year"
Year=Year[,-1]
Pre_k = Tables2$"Pre-k"

age = matrix(,nrow=1870, ncol=74)
for (i in 1:1870){
	student_age = Pre_k$unique.student_birth_year[i]
	for (j in 1:74){
		age[i,j]= Year[i,j]-student_age
		
	}
}

teacher_grade = c(1:74)
for (j in 1:74){
	student_age = age[,j]
	teacher_grade[j] = mean(student_age[!is.na(student_age)])
}
lm1 = lm(teacher_grade[-badtea]~s$v[,1:3])
lm2 = lm(teacher_grade[-badtea]~s1$v[,1:3])
summary(lm1)
summary(lm2)