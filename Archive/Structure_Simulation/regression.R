## Structure simulation using a rank-3 matrix, lambda = 590, lambda1=550, normalize by minus x
#resample

rm(list=ls())

library(truncnorm)
library(XLConnect)


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


student = read.csv("student_dim.csv")
student=student[,-1]

student1 = read.csv("student_dim1.csv")
student1=student1[,-1]



wb2<- loadWorkbook("MATH.xlsx")
Tables2 <- readWorksheet(wb2, sheet = getSheets(wb2))
z = Tables2$"Pre-k"
dim(z)

z$LunchStat=as.numeric(z$LunchStat)
z$Num_of_years = as.numeric(z$Num_of_years)
z$notLunch =z$Num_of_years-as.numeric(z$LunchStat)

lm1= glm(cbind(z$LunchStat[-badstu],z$notLunch[-badstu])~student[,1]+student[,2]+student[,3], family = binomial)
summary(lm1)

lm2= glm(cbind(z$LunchStat[-badstu],z$notLunch[-badstu])~student1[,1]+student1[,2]+student1[,3], family = binomial)
summary(lm2)

## Student birth-year
z$birthYear = as.factor(z$unique.student_birth_year)
lm3= lm(z$birthYear[-badstu]~s$u[,1:3])
summary(lm3)
lm4= lm(z$birthYear[-badstu]~s1$u[,1:3])
summary(lm4)

## Student Homeless Status
z$Homeless = as.numeric(z$Homeless)
z$binary = z$Homeless
z$binary[z$Homeless > 0] = 1
lm5= glm(z$binary[-badstu]~student[,1]+student[,2]+student[,3], family = binomial)
summary(lm5)

lm6= glm(z$binary[-badstu]~student1[,1]+student1[,2]+student1[,3], family = binomial)
summary(lm6)

## Demographic:
dem = Tables2$"Demographic"

#gender:
dem$gender = dem$gender_cd
dem$gender[dem$gender=="M"]=0
dem$gender[dem$gender=="F"]=1
dem$gender=as.numeric(dem$gender)

lm7= glm(dem$gender[-badstu]~student[,1]+student[,2]+student[,3], family = binomial)
summary(lm7)

lm8= glm(dem$gender[-badstu]~student1[,1]+student1[,2]+student1[,3], family = binomial)
summary(lm8)

#racial
dem$race = dem$racial_ethnic_cd
dem$race[dem$race != "W"] = 1
dem$race[dem$race == "W"] = 0
dem$race=as.numeric(dem$race)

lm9= glm(dem$race[-badstu]~student[,1]+student[,2]+student[,3], family = binomial)
summary(lm9)

lm10= glm(dem$race[-badstu]~student1[,1]+student1[,2]+student1[,3], family = binomial)
summary(lm10)
