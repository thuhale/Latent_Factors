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
plot(sqrt(s1$d), ylab = "Squared Root of Singular Values of the IFA estimator of N*")
k = 1

recover1 = tmp1
for(j in 1:1276){
	recover1[j,] = recover1[j,]+y[j]
	}
	
for(k in 1:72){
	recover1[,k] = recover1[,k]+z[k]
	}
recover1 = recover1-x

recover1[recover1<100]=100
recover1[recover1>500] = 500

m<-mean(tmp1)
std<-sqrt(var(as.vector(tmp1)))

ma = mean(Gnorm[!is.na(Gnorm)])
stda<-sqrt(var(as.vector(Gnorm[!is.na(Gnorm)])))


hist(tmp1, prob=TRUE, xlab="Interaction Score", ylab = "Frequency", breaks=10, col=rgb(1,1,0,0.7), xlim=c(-200,200), ylim=c(0,0.025), main="Student-Teacher Interaction - Fixed Effect Excluded")
curve(dnorm(x, mean =m, sd = std), col="red", lty=4, lwd=1, add=TRUE, yaxt="n")
par(new=TRUE)
hist(Gnorm[!is.na(Gnorm)], prob=TRUE, xlim=c(-200,200), ylim=c(0,0.025),breaks=10,col=rgb(0,1,1,0.4),main="",xlab="",ylab="")
curve(dnorm(x, mean =ma, sd = stda), col="deepskyblue4", lwd=1, add=TRUE, yaxt="n")
legend("topright", legend = c("Predicted Scores by IFA", "Observed Scores"), lty = c(4, 1), lwd = c(1,1,1), col = c("red", "deepskyblue4"))

m<-mean(tmp)
std<-sqrt(var(as.vector(tmp)))

ma = mean(G[!is.na(G)])
stda<-sqrt(var(as.vector(G[!is.na(G)])))


hist(tmp, prob=TRUE, xlab="Interaction Score", ylab = "Frequency", breaks=10, col=rgb(1,1,0,0.7), xlim=c(0,500), ylim=c(0,0.01), main="Student-Teacher Interaction - Fixed Effect Included")
curve(dnorm(x, mean =m, sd = std), col="red", lty=4, lwd=1, add=TRUE, yaxt="n")
par(new=TRUE)
hist(G[!is.na(G)], prob=TRUE, xlim=c(0,500), ylim=c(0,0.01),breaks=10,col=rgb(0,1,1,0.4),main="",xlab="",ylab="")
curve(dnorm(x, mean =ma, sd = stda), col="deepskyblue4", lwd=1, add=TRUE, yaxt="n")
legend("topright", legend = c("Predicted Scores by IFA", "Observed Scores"), lty = c(4, 1), lwd = c(1,1,1), col = c("red", "deepskyblue4"))


wb2<- loadWorkbook("~/Desktop/Latent_Factors/data/MATH.xlsx")
Tables2 <- readWorksheet(wb2, sheet = getSheets(wb2))
z = Tables2$"Pre-k"
dim(z)

z$LunchStat=as.numeric(z$LunchStat)
z$Num_of_years = as.numeric(z$Num_of_years)
z$notLunch =z$Num_of_years-as.numeric(z$LunchStat)

lm1= glm(cbind(z$LunchStat[-badstu],z$notLunch[-badstu])~s$u[,1:3], family = binomial)
summary(lm1)

lm2= glm(cbind(z$LunchStat[-badstu],z$notLunch[-badstu])~s1$u[,1:3], family = binomial)
summary(lm2)


## Student Homeless Status
z$Homeless = as.numeric(z$Homeless)
z$binary = z$Homeless
z$binary[z$Homeless > 0] = 1
lm3= glm(z$binary[-badstu]~s$u[,1:3], family = binomial)
summary(lm3)

lm4= glm(z$binary[-badstu]~s1$u[,1:3], family = binomial)
summary(lm4)

## Demographic:
dem = Tables2$"Demographic"

#gender:
dem$gender = dem$gender_cd
dem$gender[dem$gender=="M"]=0
dem$gender[dem$gender=="F"]=1
dem$gender=as.numeric(dem$gender)

lm5= glm(dem$gender[-badstu]~s$u[,1:3], family = binomial)
summary(lm5)

lm6= glm(dem$gender[-badstu]~s1$u[,1:3], family = binomial)
summary(lm6)

#racial
dem$race = dem$racial_ethnic_cd
dem$race[dem$race != "W"] = 1
dem$race[dem$race == "W"] = 0
dem$race=as.numeric(dem$race)

lm7= glm(dem$race[-badstu]~s$u[,1:3], family = binomial)
summary(lm7)

lm8= glm(dem$race[-badstu]~s1$u[,1:3], family = binomial)
summary(lm8)

#country:
dem$country = dem$country_cd
dem$country[dem$country != "US"] = 1
dem$country[dem$country == "US"] = 0
dem$country=as.numeric(dem$country)

lm9= glm(dem$country[-badstu]~s$u[,1:3], family = binomial)
summary(lm9)

lm10= glm(dem$country[-badstu]~s1$u[,1:3], family = binomial)
summary(lm10)

#language:
dem$language = dem$language_cd  
dem$language[dem$language != "EN"] = 1
dem$language[dem$language == "EN"] = 0
dem$language=as.numeric(dem$language)

lm11= glm(dem$language[-badstu]~s$u[,1:3], family = binomial)
summary(lm11)

lm12= glm(dem$language[-badstu]~s1$u[,1:3], family = binomial)
summary(lm12)


#birth_country:
dem$birth_country = dem$country_cd_borned_in
dem$birth_country[dem$birth_country != "US"] = 1
dem$birth_country[dem$birth_country == "US"] = 0
dem$birth_country=as.numeric(dem$birth_country)

lm13= glm(dem$birth_country[-badstu]~s$u[,1:3], family = binomial)
summary(lm13)

lm14= glm(dem$birth_country[-badstu]~s1$u[,1:3], family = binomial)
summary(lm14)

#parent_language:
dem$parent_language = dem$language_have_parents_speaking
dem$parent_language[dem$parent_language != "EN"] = 1
dem$parent_language[dem$parent_language == "EN"] = 0
dem$parent_language=as.numeric(dem$parent_language)

lm15= glm(dem$parent_language[-badstu]~s$u[,1:3], family = binomial)
summary(lm16)

lm16= glm(dem$parent_language[-badstu]~s1$u[,1:3], family = binomial)
summary(lm16)






q1=qplot(s1$u[,1], z$lunchYear[-badstu] )
plot1=q1+geom_smooth(method="lm")+geom_point(position="jitter")+labs(x="1st Latent Factor", y="reduced_lunch")




