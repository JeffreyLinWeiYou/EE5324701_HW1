library(datasets)

a <- anscombe
fitlse = function(x,y){
res = NULL
res$b = sum((y-mean(y))*(x-mean(x)))/sum((x-mean(x))^2)
res$a = mean(y) - res$b * mean(x)
return(res)
}
library(datasets)
a <- anscombe
par(mfrow=c(2,2))
f1 = fitlse(a$x1,a$y1)
f2 = fitlse(a$x2,a$y2)
f3 = fitlse(a$x3,a$y3)
f4 = fitlse(a$x4,a$y4)
plot(a$x1,a$y1, main=paste("Dataset One"))
abline(a=f1$a,b=f1$b)
plot(a$x2,a$y2, main=paste("Dataset Two"))
abline(a=f2$a,b=f2$b)
plot(a$x3,a$y3, main=paste("Dataset Three"))
abline(a=f3$a,b=f3$b)
plot(a$x4,a$y4, main=paste("Dataset Four"))
abline(a=f4$a,b=f4$b)

par(mfrow=c(1,1))
x = a$x2
y = a$y2
x_sorted = sort.int(x,index.return=T)
x = x_sorted$x
y = y[x_sorted$ix]
z = x^2
fit2 = lm(y ~ x + z)
plot(a$x2,a$y2, main=paste("Dataset Two_FIT"))
lines(x = x,y=fitted(fit2))

pred1 = f1$a + f1$b * 10
pred2 = f2$a + f2$b * 10
pred3 = f3$a + f3$b * 10
pred4 = f4$a + f4$b * 10
cat('value:10 in Dataset 1 =',pred1)

cat('value:10 in Dataset 2 =',pred2)

cat('value:10 in Dataset 3 =',pred3)

cat('value:10 in Dataset 4 =',pred4)


