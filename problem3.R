load('C:/Users/Chien/Documents/MEGAsync/EE303¡G°jÂk¤ÀªR(EE5324701)/drive-download-20170420T051134Z-001/meap93.RData')
y = data$math10
x = log(data$expend)
#x = data$expend
n = length(x)
b1 = sum((x - mean(x))*(y - mean(y)))/sum((x - mean(x))^2)
b0 = mean(y) - b1 * mean(x)
sigma.hat = sqrt((sum((y - b0 - b1*x)^2))/(n-2))
se0 = sigma.hat * sqrt(sum(x^2)/(n*sum((x-mean(x))^2)))
se1 = sigma.hat / sqrt(sum((x-mean(x))^2))
cat('beta_0 is estimated as ',b0,' with standard error ',se0,
    'nnbeta_1 is estimated as ',b1,' with standard error ',se1)

math10.target = 100
log.expend.target = (math10.target - b0)/b1
expend.target = exp(log.expend.target)
cat('Minimum expenditure per student required to achieve 100% pass rate: $',expend.target)
percent = (0.1*b1)/mean(y)
## beta_0 is estimated as -69.34116 with standard error 26.53013
## beta_1 is estimated as 11.1644 with standard error 3.169011