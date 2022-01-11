data(iris)
head(iris)

idx=c(97, 142, 121,  82, 108, 112,  71,  39,  23,  45)
iris_subset=iris[idx,]

plot(iris_subset$Petal.Width, iris_subset$Petal.Length, 
     main='Iris data: scatterplot', xlab='Petal width', ylab='Petal length')

x=iris_subset$Petal.Width
y=iris_subset$Petal.Length
n=nrow(iris_subset)

#compute b1 and b0
((n-1)/n)*cov(x,y)

MXY=mean(x*y)
MY=mean(y)
MX=mean(x)
MXsq=mean(x^2)
MYsq=mean(y^2)
VX=MXsq-MX^2
VY=MYsq-MY^2

CovXY=MXY-(MX*MY)

b_1=CovXY/VX
b_0=MY-b_1*MX

#estimated regression line
y_hat=b_0+b_1*x


#draw the regression line
plot(iris_subset$Petal.Width, iris_subset$Petal.Length, 
     main='Iris data: scatterplot', xlab='Petal width', ylab='Petal length')

curve(b_0+b_1*x, add=T)
#R^2
Rsq=(CovXY^2)/(VX*VY)
  
#residuals squared
ei_sq=(y-y_hat)^2
sum(ei_sq)
sum(ei_sq)/(n-2)
s_epsilon=sqrt(sum(ei_sq)/(n-2))


#hat SE(B_0)
se_b0=sqrt((s_epsilon^2/n)*(1+(MX^2/VX)))
  
#hat SE(B_1)
se_b1=sqrt((s_epsilon^2/n)*(1/VX))


##CI
b_0-qt(0.975, n-2)*se_b0
b_0+qt(0.975, n-2)*se_b0

b_1-qt(0.975, n-2)*se_b1
b_1+qt(0.975, n-2)*se_b1


##TEST
t0_obs=b_0/se_b0

t1_obs=b_1/se_b1

#p-value
pt(-t0_obs, 8)+(1-pt(t0_obs, 8))
pt(-t1_obs, 8)+(1-pt(t1_obs, 8))


fit.model <- lm(iris_subset$Petal.Length ~ iris_subset$Petal.Width)
summary(fit.model)

residuals(fit.model)
fit.model$fitted.values


hist(residuals(fit.model))




