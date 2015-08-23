singular = read.csv("two-tier-structure-simulation.csv")
singular = singular[,-1]
par(mfrow=c(1,3))
hist(singular$V1, col = "steelblue1", ylab ="Frequency", xlab="First Singular Value of M is 63,022", main ="", breaks = 20)

hist(singular$V2, col = "steelblue1", ylab ="Frequency", xlab = "Second Singular Value of M is 4,228", main ="", breaks = 20)

hist(singular$V3, col = "steelblue1", ylab ="Frequency", xlab="Third Singular Value of M is 603", main ="", breaks = 20)

v1_sd = sd(singular$V1)*sqrt(99/100)
v1_mean = mean(singular$V1)
z1=(63022-v1_mean)/v1_sd
z1

v2_sd = sd(singular$V2)*sqrt(99/100)
v2_mean = mean(singular$V2)
z2=(4228-v2_mean)/v2_sd
z2

v3_sd = sd(singular$V3)*sqrt(99/100)
v3_mean = mean(singular$V3)
z3=(603-v3_mean)/v3_sd
z3


curve(dnorm(x, mean =0, sd = 1), col="steelblue4", lwd=1, add=TRUE, yaxt="n", xlim = c(-7,7), xlab="z-scores of the Singular Values in Respect to the Simulated Values", ylab ="")
abline(v=6.9, col = "red", lty=3)
abline(v=z2, col = "turquoise2")
abline(v=z3, col = "magenta")
legend("topright", inset=c(0,0),legend = c("1st Singular Value = 25", "2nd Singular Value = 6.3", "Singular Value = -5.4"), lty = c(3,1,1), col = c("red", "turquoise2", "magenta"))






singular = read.csv("two-tier-structure-simulation1.csv")
singular = singular[,-1]
par(mfrow=c(1,3))
hist(singular$V1, col = "steelblue1", ylab ="Frequency", xlab="First Singular Value of N is 3,350", main ="", breaks = 20)

hist(singular$V2, col = "steelblue1", ylab ="Frequency", xlab = "Second Singular Value of N is 1,830", main ="", breaks = 20)

hist(singular$V3, col = "steelblue1", ylab ="Frequency", xlab="Third Singular Value of N is 560", main ="", breaks = 20)

v1_sd = sd(singular$V1)*sqrt(99/100)
v1_mean = mean(singular$V1)
z1=(3350-v1_mean)/v1_sd
z1

v2_sd = sd(singular$V2)*sqrt(99/100)
v2_mean = mean(singular$V2)
z2=(1830-v2_mean)/v2_sd
z2

v3_sd = sd(singular$V3)*sqrt(99/100)
v3_mean = mean(singular$V3)
z3=(560-v3_mean)/v3_sd
z3

curve(dnorm(x, mean =0, sd = 1), col="steelblue4", lwd=1, add=TRUE, yaxt="n", xlim = c(-7,7), xlab="z-scores of the Singular Values in Respect to the Simulated Values", ylab ="")
abline(v=6.5, col = "red", lty=3)
abline(v=6.9, col = "turquoise2", lty=3)
abline(v=z3, col = "magenta")
legend("topright", inset=c(0,0),legend = c("1st Singular Value = 9.5", "2nd Singular Value = 19.2", "Singular Value = -6.6"), lty = c(3,3,1), col = c("red", "turquoise2", "magenta"))




library(XLConnect)
wb2<- loadWorkbook("MATH.xlsx")
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
lm5= glm(z$binary[-badstu]~s$u[,1:3], family = binomial)
summary(lm5)

lm6= glm(z$binary[-badstu]~s1$u[,1:3], family = binomial)
summary(lm6)

## Demographic:
dem = Tables2$"Demographic"

#gender:
dem$gender = dem$gender_cd
dem$gender[dem$gender=="M"]=0
dem$gender[dem$gender=="F"]=1
dem$gender=as.numeric(dem$gender)

lm7= glm(dem$gender[-badstu]~s$u[,1:3], family = binomial)
summary(lm7)

lm8= glm(dem$gender[-badstu]~s1$u[,1:3], family = binomial)
summary(lm8)

#racial
dem$race = dem$racial_ethnic_cd
dem$race[dem$race != "W"] = 1
dem$race[dem$race == "W"] = 0
dem$race=as.numeric(dem$race)

lm9= glm(dem$race[-badstu]~s$u[,1:3], family = binomial)
summary(lm9)

lm10= glm(dem$race[-badstu]~s1$u[,1:3], family = binomial)
summary(lm10)









q1=qplot(s1$u[,1], z$lunchYear[-badstu] )
plot1=q1+geom_smooth(method="lm")+geom_point(position="jitter")+labs(x="1st Latent Factor", y="reduced_lunch")




