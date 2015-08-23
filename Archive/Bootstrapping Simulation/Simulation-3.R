library(CCA)
rm(list=ls())


x = read.csv("math.csv")
x = data.frame(x)
x=x[,-1]

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
tmp[is.na(G)] = 0

for( i in 1:5000){
  s = svd(tmp)
  s$d = thresh(s$d, lam)
  low = s$u%*%diag(s$d)%*%t(s$v)
  tmp[is.na(G)] = low[is.na(G)]
  if(round(i/1000)%in% 1:100) plot(sqrt(s$d), main = paste(i))
}
write.csv(s$u,"student-initial.csv")
write.csv(s$v,"teacher-initial.csv")
print(s$d)

##Bootstrapping
h=1276
w=72
complete=s$u%*%diag(s$d)%*%t(s$v)
sigmahat=(sum((complete[!is.na(G)]-G[!is.na(G)])^2)/(sum(!is.na(G))-3*1276-3*72))^0.5

student_xcoef=matrix(nrow=3,ncol=3,0)
student_ycoef=matrix(nrow=3,ncol=3,0)
student_cor=c(0,0,0)

teacher_xcoef=matrix(nrow=3,ncol=3,0)
teacher_ycoef=matrix(nrow=3,ncol=3,0)
teacher_cor=c(0,0,0) 


for(k in 1:100){

	error=matrix( rnorm(1276*72,mean=0,sd=sigmahat), 1276, 72) 
	Ghat=complete+error

	tmp1 = Ghat
	tmp1[is.na(Ghat)] = 0

	for( i in 1:5000){
  		s1 = svd(tmp1)
  		s1$d = thresh(s1$d, lam)
  		low1 = s1$u%*%diag(s1$d)%*%t(s1$v)
 		tmp1[is.na(Ghat)] = low1[is.na(Ghat)]
	}
	cc_student =cc(s$u[,1:3], s1$u[,1:3])
	student_xcoef=student_xcoef+ cc_student$xcoef
	student_ycoef=student_ycoef+ cc_student$ycoef
	student_cor=student_cor+cc_student$cor
	
	cc_teacher =cc(s$v[,1:3], s1$v[,1:3])
	teacher_xcoef=teacher_xcoef+ cc_teacher$xcoef
	teacher_ycoef=teacher_ycoef+ cc_teacher$ycoef
	teacher_cor=teacher_cor+cc_teacher$cor
}

student_xcoef=student_xcoef/100
student_ycoef=student_ycoef/100
student_cor=student_cor/100

teacher_xcoef=teacher_xcoef/100
teacher_ycoef=teacher_ycoef/100
teacher_cor=teacher_cor/100

print (student_cor)
print (teacher_cor)

write.csv(student_xcoef,"student_xcoef.csv")
write.csv(student_ycoef,"student_ycoef.csv")
write.csv(student_cor, "student_cor.csv")

write.csv(teacher_xcoef,"teacher_xcoef.csv")
write.csv(teacher_ycoef,"teacher_ycoef.csv")
write.csv(teacher_cor, "teacher_cor.csv")

