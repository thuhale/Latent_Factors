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

wb2<- loadWorkbook("~/Desktop/Latent_Factors/Data/MATH.xlsx")
Tables2 <- readWorksheet(wb2, sheet = getSheets(wb2))
Pre_k = Tables2$"Pre-k"
dem = Tables2$"Demographic"

##Lunch variable
Pre_k$LunchStat=as.numeric(Pre_k$LunchStat)
Pre_k$Num_of_years = as.numeric(Pre_k$Num_of_years)
Pre_k$notLunch =Pre_k$Num_of_years-as.numeric(Pre_k$LunchStat)

## Student Homeless Status
Pre_k$Homeless = as.numeric(Pre_k$Homeless)
Pre_k$binary = Pre_k$Homeless
Pre_k$binary[Pre_k$Homeless > 0] = 1

#Gender
dem$gender = dem$gender_cd
dem$gender[dem$gender=="M"]=0
dem$gender[dem$gender=="F"]=1
dem$gender=as.numeric(dem$gender)

#racial
dem$race = dem$racial_ethnic_cd
dem$race[dem$race != "W"] = 1
dem$race[dem$race == "W"] = 0
dem$race=as.numeric(dem$race)


#country:
dem$country = dem$country_cd
dem$country[dem$country != "US"] = 1
dem$country[dem$country == "US"] = 0
dem$country=as.numeric(dem$country)

#language:
dem$language = dem$language_cd  
dem$language[dem$language != "EN"] = 1
dem$language[dem$language == "EN"] = 0
dem$language=as.numeric(dem$language)

#birth_country:
dem$birth_country = dem$country_cd_borned_in
dem$birth_country[dem$birth_country != "US"] = 1
dem$birth_country[dem$birth_country == "US"] = 0
dem$birth_country=as.numeric(dem$birth_country)

#parent_language:
dem$parent_language = dem$language_have_parents_speaking
dem$parent_language[dem$parent_language != "EN"] = 1
dem$parent_language[dem$parent_language == "EN"] = 0
dem$parent_language=as.numeric(dem$parent_language)


thresh = function(a, lam){
	a = a - lam
	a[a<0] = 0
	return(a)
}


##Generate random matrix
K = as.list(G)
K=K[!is.na(K)]
K=as.numeric(K)
mu = mean(K)
sigma = sd(K)

Lunch_Rsquare = c(1:100)
Lunch_Rsquare1 = c(1:100)


Homeless_Rsquare = c(1:100)
Homeless_Rsquare1 = c(1:100)


Gender_Rsquare = c(1:100)
Gender_Rsquare1 = c(1:100)

Race_Rsquare = c(1:100)
Race_Rsquare1 = c(1:100)

Country_Rsquare = c(1:100)
Country_Rsquare1 = c(1:100)

Language_Rsquare = c(1:100)
Language_Rsquare1 = c(1:100)

Birth_Country_Rsquare = c(1:100)
Birth_Country_Rsquare1 = c(1:100)


Parent_Language_Rsquare = c(1:100)
Parent_Language_Rsquare1 = c(1:100)







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
		
	lm1= glm(cbind(Pre_k$LunchStat[-badstu],Pre_k$notLunch[-badstu])~new_s$u[,1:3], family = binomial)
	lm2= glm(cbind(Pre_k$LunchStat[-badstu],Pre_k$notLunch[-badstu])~new_s1$u[,1:3], family = binomial)
	Lunch_Rsquare[l]=1-lm1$dev/lm1$null.dev
	Lunch_Rsquare1[l]=1-lm2$dev/lm2$null.dev
	
	lm3= glm(Pre_k$binary[-badstu]~new_s$u[,1:3], family = binomial)
	lm4= glm(Pre_k$binary[-badstu]~new_s1$u[,1:3], family = binomial)
	Homeless_Rsquare[l] = 1-lm3$dev/lm3$null.dev
	Homeless_Rsquare1[l] = 1-lm4$dev/lm4$null.dev
	
	lm5= glm(dem$gender[-badstu]~new_s$u[,1:3], family = binomial)
	lm6= glm(dem$gender[-badstu]~new_s1$u[,1:3], family = binomial)
	Gender_Rsquare[l] = 1-lm5$dev/lm5$null.dev
	Gender_Rsquare1[l] = 1-lm6$dev/lm6$null.dev
	
	lm7= glm(dem$race[-badstu]~new_s$u[,1:3], family = binomial)
	lm8= glm(dem$race[-badstu]~new_s1$u[,1:3], family = binomial)
	Race_Rsquare[l] = 1-lm7$dev/lm7$null.dev
	Race_Rsquare1[l] = 1-lm8$dev/lm8$null.dev
	
	lm9= glm(dem$country[-badstu]~new_s$u[,1:3], family = binomial)
	lm10= glm(dem$country[-badstu]~new_s1$u[,1:3], family = binomial)
	Country_Rsquare[l] = 1-lm9$dev/lm9$null.dev
	Country_Rsquare1[l] = 1-lm10$dev/lm10$null.dev
	
	lm11= glm(dem$language[-badstu]~new_s$u[,1:3], family = binomial)
	lm12= glm(dem$language[-badstu]~new_s1$u[,1:3], family = binomial)
	Language_Rsquare[l] = 1-lm11$dev/lm11$null.dev
	Language_Rsquare1[l] = 1-lm12$dev/lm12$null.dev
	
	lm13= glm(dem$birth_country[-badstu]~new_s$u[,1:3], family = binomial)
	lm14= glm(dem$birth_country[-badstu]~new_s1$u[,1:3], family = binomial)
	Birth_Country_Rsquare[l] = 1-lm13$dev/lm13$null.dev
	Birth_Country_Rsquare1[l] = 1-lm14$dev/lm14$null.dev

	lm15= glm(dem$parent_language[-badstu]~new_s$u[,1:3], family = binomial)
	lm16= glm(dem$parent_language[-badstu]~new_s1$u[,1:3], family = binomial)
	Parent_Language_Rsquare[l] = 1-lm15$dev/lm15$null.dev
	Parent_Language_Rsquare1[l] = 1-lm16$dev/lm16$null.dev
}


write.csv(Lunch_Rsquare, "Lunch_Rsquare.csv")
write.csv(Lunch_Rsquare1, "Lunch_Rsquare1.csv")

write.csv(Homeless_Rsquare, "Homeless_Rsquare.csv")
write.csv(Homeless_Rsquare1, "Homeless_Rsquare1.csv")

write.csv(Gender_Rsquare, "Gender_Rsquare.csv")
write.csv(Gender_Rsquare1, "Gender_Rsquare1.csv")

write.csv(Race_Rsquare, "Race_Rsquare.csv")
write.csv(Race_Rsquare1, "Race_Rsquare1.csv")

write.csv(Country_Rsquare, "Country_Rsquare.csv")
write.csv(Country_Rsquare1, "Country_Rsquare1.csv")

write.csv(Language_Rsquare, "Language_Rsquare.csv")
write.csv(Language_Rsquare1, "Language_Rsquare1.csv")

write.csv(Birth_Country_Rsquare, "Birth_Country_Rsquare.csv")
write.csv(Birth_Country_Rsquare1, "Birth_Country_Rsquare1.csv")

write.csv(Parent_Language_Rsquare, "Parent_Language_Rsquare.csv")
write.csv(Parent_Language_Rsquare1, "Parent_Language_Rsquare1.csv")




















