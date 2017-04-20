load('C:/Users/Chien/Documents/MEGAsync/EE303¡G°jÂk¤ÀªR(EE5324701)/drive-download-20170420T051134Z-001/meap93.RData')
y = data$math10
x = data$lnchprg
n = length(x)
b1 = sum((x - mean(x))*(y - mean(y)))/sum((x - mean(x))^2)
b0 = mean(y) - b1 * mean(x)
sigma.hat = sqrt((sum((y - b0 - b1*x)^2))/(n-2))
se0 = sigma.hat * sqrt(sum(x^2)/(n*sum((x-mean(x))^2)))
se1 = sigma.hat / sqrt(sum((x-mean(x))^2))
cat('beta_0 is estimated as ',b0,' with standard error ',se0,
    'nnbeta_1 is estimated as ',b1,' with standard error ',se1)
covXY = sum((x - mean(x))*(y - mean(y)))/n


## beta_0 is estimated as 32.14271 with standard error 0.9975824
## beta_1 is estimated as -0.3188643 with standard error 0.03483933