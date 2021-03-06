#Load the Pearson Father-Son Height data.
setwd("C:/Users/Chien/Documents/MEGAsync/EE303�G�j�k���R(EE5324701)/lec2")
height = read.table("C:/Users/Chien/Documents/MEGAsync/EE303�G�j�k���R(EE5324701)/lec2/PearsonHeightData.txt", header = T)
setwd("C:/Users/Chien/Documents/MEGAsync/EE303�G�j�k���R(EE5324701)/lec2")
height = read.table("PearsonHeightData.txt", header = T)
dim(height)
head(height)
plot(Son ~ Father, data = height)

#The regression line
lmod = lm(Son ~ Father, data = height) #[format: regress y on x]
abline(lmod, col = "red")
summary(lmod)

n = nrow(height)
sx = sqrt((sum((height$Father - mean(height$Father))^2))/n)
sy = sqrt((sum((height$Son - mean(height$Son))^2))/n)
r = (sum(((height$Son - mean(height$Son))/sy)*((height$Father - mean(height$Father))/sx)))/n
b1 = r*sy/sx
b0 = mean(height$Son) - b1*mean(height$Father)
# Direct Computation for b0, b1
c(b0, b1)   
lmod$coefficients  #1
lmod$residuals     #2
lmod$effects       #3
lmod$rank          #4
lmod$fitted.values #5

#The SD line.
#This line passes through the mean of the dataset (xbar, ybar) but has slope equal to sy/sx if r > 0 and -sy/sx if r < 0.
sd.slope = sy/sx
sd.intercept = mean(height$Son) - (sy/sx)*mean(height$Father)
plot(Son ~ Father, data = height)
abline(lmod, col = "red")
abline(a = sd.intercept, b = sd.slope, col = "blue")

#Example Two: Wages Data
load("/Users/myichael/Google Drive/Course/Stat 151A/Data/wage1.RData")
load("wage1.RData")
wages = data
wages.desc = desc

head(wages)
dim(wages)
wages.desc

#To illustrate simple linear regression, we take y = wages$wage and x = wages$educ.

y = wages$wage
x = wages$educ
plot(x, y, xlab = "Education", ylab = "Hourly Wages")
#The regression line
slm = lm(wage ~ educ, data = wages)
summary(slm)
abline(slm, col = "red")
#The estimated intercept term is -0.90485. This means that the estimated average hourly wage for people with no education is -0.90485. The fact that this is negative seems nonsensical. Why is this happening? 
#The estimate for beta1 is 0.54136. The interpretation is that for every additional year of education, the average hourly wage increases by 54 cents. Or, for every additional four years of education, the average hourly wage increases by 4*54 = 2.16 dollars. Is this satisfying? A percentage increase in wage is more meaningful than this. For this, we can take y = log(wages$wage) instead of wages$wage
y = log(wages$wage)
x = wages$educ
plot(x, y, xlab = "Education", ylab = "Log Hourly Wages")
reg = lm(y ~ x)
summary(reg)
abline(reg)
#The estimate of beta1 ie 0.082744. This means that the average hourly wage increases by 8.3 percent for every additional year of education.

#CEO salary and Firm Sales Data
load("/Users/myichael/Google Drive/Course/Stat 151A/Data/ceosal1.RData")
load("ceosal1.RData")
# load("/Users/myichael/Google Drive/Course/Stat 151A/Data/ceosal1.RData")
ceo = data
ceo.desc = desc
dim(ceo)
head(ceo)
#Salary is in thousands of dollars.
#Sales is in millions of dollars.
#Linear regression of salary on sales is probably not a good idea. Better is to regress log(salary) on log(sales).
y = log(ceo$salary)
x = log(ceo$sales)
plot(x, y, xlab = "Education", ylab = "Log Hourly Wages")
reg = lm(y ~ x)
summary(reg)
abline(reg)
#The estimate of beta1 is 0.25667. This means that a 1 percent increase in firm sales increases CEO salary by about 0.257 percent (NOT 2.57 percent). 

