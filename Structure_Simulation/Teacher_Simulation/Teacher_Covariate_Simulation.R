## Structure simulation using a rank-3 matrix, lambda = 590, lambda1=550, normalize by minus x
#resample

rm(list=ls())
library(truncnorm)
library(XLConnect)

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


teacher = read.csv("~/Desktop/Latent_Factors/Data/teacher_list.csv")
dim(teacher)

teacher_grade_Rsquare = c(1:100)
teacher_grade_Rsquare1 = c(1:100)

thresh = function(a, lam){
	a = a - lam
	a[a<0] = 0
	return(a)
}

teacher$flagship_u = 0
teacher$flagship_u[teacher$school_name=="University of Florida"]=1

teacher$minority = 1
teacher$minority[teacher$racial_ethnic_cd=="W"]=0
teacher$minority[0]=NA

experience_Rsquare = c(1:100)
experience_Rsquare1 = c(1:100)


uni_Rsquare = c(1:100)
uni_Rsquare1 = c(1:100)


race_Rsquare = c(1:100)
race_Rsquare1 = c(1:100)




##Generate random matrix
K = as.list(G)
K=K[!is.na(K)]
K=as.numeric(K)
mu = mean(K)
sigma = sd(K)



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
		
	lm1 = lm(teacher$total_years_amt[-badtea]~new_s$v[,1:3])
	lm2 = lm(teacher$total_years_amt[-badtea]~new_s1$v[,1:3])
	
	experience_Rsquare[l]=summary(lm1)$r.squared
	experience_Rsquare1[l]=summary(lm2)$r.squared
	
	
	lm3= glm(teacher$flagship_u[-badtea]~new_s$v[,1:3], family = binomial)
	lm4= glm(teacher$flagship_u[-badtea]~new_s1$v[,1:3], family = binomial)
	
	uni_Rsquare[l] = 1-lm3$dev/lm3$null.dev
	uni_Rsquare1[l] = 1-lm4$dev/lm4$null.dev
	
	lm5= glm(teacher$minority[-badtea]~new_s$v[,1:3], family = binomial)
	lm6= glm(teacher$minority[-badtea]~new_s1$v[,1:3], family = binomial)
	
	race_Rsquare[l] = 1-lm5$dev/lm5$null.dev
	race_Rsquare1[l] = 1-lm6$dev/lm6$null.dev



	
}


write.csv(experience_Rsquare, "teacher_experience_Rsquare.csv")
write.csv(experience_Rsquare1, "teacher_experience_Rsquare1.csv")

write.csv(uni_Rsquare, "teacher_university_Rsquare.csv")
write.csv(uni_Rsquare1, "teacher_university_Rsquare1.csv")

write.csv(race_Rsquare, "teacher_race_Rsquare.csv")
write.csv(race_Rsquare1, "teacher_race_Rsquare1.csv")





















