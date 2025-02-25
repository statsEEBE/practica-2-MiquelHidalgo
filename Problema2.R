#Codigo para problema 2
mis_dades <- iris
mis_dades
mean(mis_dades$Sepal.Length)
sd(mis_dades$Sepal.Length)#desviacion
dim(mis_dades)
names(mis_dades)
hist(mis_dades$Sepal.Length)

x <- mis_dades$Petal.Length
x
y <- mis_dades$Sepal.Length
y

plot(x,y)

# m=(sum(x-mean(x))*(y-mean(y)))/(sum(x-mean(x))^2) -> pendent recta
m <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m
#b = intersecció amb l'eix y
b <- mean(y)-m*mean(x)
b
#pregunta 3
m*1.5+b

lm(y~x)
mod <- lm(y~x)
summary(mod)

xpred <- data.frame(x=1.5)
xpred <- data.frame(x=1:7)
xpred <- data.frame(x=x)
xpred
ypred <- predict(mod,xpred)
ypred

plot(x,y)
lines(x,ypred) #regressio
#R^2 = (sum(ypred-mean(y))^2)/(sum(y-mean(y))^2)
Rsq <- sum((ypred-mean(y)^2))/sum((y-mean(y)^2))#calculo de R^2
Rsq <- sum((ypred-mean(y)^2))/sum((y-mean(y)^2))
Rsq
#coeficiente de correlacion de pearson = sqrt(R^2)
