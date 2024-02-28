# uniform dist
u = runif(1000)
hist(u, prob=T)
curve(dunif(x,0,1),add=T, col="red")
curve(punif(x), col="blue")
summary(u)

#normal dist
x = rnorm(100)
mean(x)
sd(x)
y = rnorm(100, mean=10, sd = 3)
mean(y)
sd(y)
pnorm(1)
qnorm(0.975)

hist(x,probability=T,col=gray(.9), ylim=c(0,0.45))
curve(dnorm(x),-5,5, add=T, col="red")

x.all = seq(-3, 3, by = 0.01)
y.all = dnorm(x.all)
plot(x.all, y.all, lwd = 2, type = "l", col="red")
x.p = seq(-3, qnorm(.05), length = 100)
y.p = dnorm(x.p)
polygon(c(-3, x.p, qnorm(.05)), c(0, y.p, 0), col = "gray")
x.p.up = seq(qnorm(.95), 3, length = 100)
y.p.up = dnorm(x.p.up)
polygon(c(qnorm(.95), x.p.up, 3), c(0, y.p.up, 0), col = "gray")

# student T
curve(dnorm, -5, 5, col="blue")
curve(dt(x, df=30), -5, 5, col="purple", add=T)
curve(dt(x, df=10), -5, 5, col="red", add=T)
curve(dt(x, df=1), -5, 5, col="orange", add=T)

# TLC
numsims = 1000
tam = 10
values = rnorm(numsims * tam)
m = matrix(values, numsims, tam)
mean(m)
sd(m)
medias = apply(m, 1, mean)
mean(medias)
sd(medias)
1 / sqrt(tam)

values.u = runif(numsims * tam)
m.u = matrix(values.u, numsims, tam)
mean(m.u)
sd(m.u)
1/sqrt(12)
medias.u = apply(m.u, 1, mean)
mean(medias.u)
sd(medias.u)
1/sqrt(12*tam)

## QQ plots
x = rnorm(100, 10, 5)
qqnorm(x,main='normal(10,5)', pch=19, col="blue")
qqline(x, col="red", lwd=3)

x = runif(100)
qqnorm(x,main='uniforme(0,1)', pch=19, col="blue")
qqline(x, col="red", lwd=3)

# amostragem
sample(1:6, 5, replace = T)
sample(c("cara","coroa"),4,replace=TRUE)
sample(1:49, 6)
cartas = paste(rep(c("A",2:10,"J","Q","K"),4), c("C","P","E","O"), sep="")
sample(cartas, 3)
sample(1:10,10)

# int conf
x = c(175,176,173,175,174,173,173,176,173,179)
mean(x)
se = sd(x) / sqrt(10)
se
qt(0.975,9)
intconf = mean(x) + c(-se*qt(0.975,9), se*qt(0.975,9))
intconf

t.test(x)

# testes medias / medianas
t.test(x, mu=175)
t.test(x)

wilcox.test(x, conf.int=T)

drug = c(15, 10, 13, 7, 9, 8, 21, 9, 14, 8)
placebo = c(15, 14, 12, 8, 14, 7, 16, 10, 15, 12)
t.test(drug, placebo, alt="less", var.equal=TRUE)

diag_med1 = c(3, 0, 5, 2, 5, 5, 5, 4, 4, 5)
diag_med2 = c(2, 1, 4, 1, 4, 3, 3, 2, 3, 5)
t.test(diag_med1, diag_med2, paired=T)

# testes distribuicoes

freq_dados = c(22,21,22,27,22,36)
probs = rep(1/6,6)
probs
chisq.test(freq_dados, p=probs)

freqs_letras = c(100,110,80,55,14)
probs = c(29, 21, 17, 17, 16)/100
chisq.test(freqs_letras, p=probs)

ks.test(as.vector(scale(iris$Petal.Length)), "pnorm")
ks.test(as.vector(scale(iris$Sepal.Length)), "pnorm")

# testes dist normal
n = rnorm(100)
shapiro.test(n)

n = runif(100)
shapiro.test(n)

hist(iris$Sepal.Width, col = "cyan", breaks = 10, prob = T)
lines(density(iris$Sepal.Width), col = "red", lwd = 2)
shapiro.test(iris$Sepal.Width)

qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width, col = "red", lwd = 2)

# testes multiplos
set.seed(12345)
m = matrix(rnorm(100*60),100,60)
tt = function(x) t.test(x[1:30],x[31:60])$p.value
pvalues = apply(m, 1, tt)
sum(pvalues < 0.05)
adj.bonf = p.adjust(pvalues, "bonferroni")
sum(adj.bonf < 0.05)
adj.bh = p.adjust(pvalues, "BH")
sum(adj.bh < 0.05)

set.seed(12345)
m1 = matrix(rnorm(50*60),50,60)
m2 = cbind(matrix(rnorm(50*30),50,30), matrix(rnorm(50*30, mean = 1),50,30) )
mt = rbind(m1, m2)
pvalues.2 = apply(mt, 1, tt)
res.verd = c(rep("nao rej",50), rep("rej",50))
table(pvalues.2 < 0.05, res.verd)
adj.bonf.2 = p.adjust(pvalues.2, "bonferroni")
table(adj.bonf.2 < 0.05, res.verd)
adj.bh.2 = p.adjust(pvalues.2, "BH")
table(adj.bh.2 < 0.05, res.verd)

# ANOVA

fligner.test(values ~ ind, data= notas_st)
prof1 = c(4,3,4,5,2,3,4,5)
prof2 = c(4,4,5,5,4,5,4,4)
prof3 = c(3,4,2,4,5,5,4,4)
notas = data.frame(prof1, prof2, prof3)
notas_st = stack(notas)
notas_st
oneway.test(values ~ ind, data=notas_st, var.equal=T)

anv = aov(values ~ ind, data=notas_st)
anv
summary(anv)
anv$coeff

kruskal.test(values ~ ind, data= notas_st)

anvf = aov(iris$Petal.Length ~ iris$Species)
summary(anvf)
TukeyHSD(anvf)

# regressao linear

library(UsingR)
data(galton)
names(galton)
hist(galton$child,col="blue",breaks=100)
abline(v=mean(galton$child), col = "red", lwd = 4)
hist(galton$parent,col="blue",breaks=100)
abline(v=mean(galton$parent), col = "red", lwd = 4)

plot(galton$parent,galton$child,pch=19,col="blue")
cor(galton$child, galton$parent)
plot(jitter(galton$parent,factor=2),jitter(galton$child,factor=2), pch=19,col="blue")

lm1 <- lm(child ~ parent, data=galton)
lm1$coefficients
plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent,lm1$fitted,col="red",lwd=5)

attach(galton)
beta1 = cor(child,parent) * sd(child)/ sd(parent)
beta0 = mean(child) - beta1* mean(parent)
c(beta0, beta1)

plot(galton$parent,lm1$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=3)

summary(lm1)
cor(child, parent) ^2
anova(lm(child ~ parent))
confint(lm1)

predict(lm1, data.frame(parent=c(64, 65, 66, 67, 68)))
predict(lm1, data.frame(parent=c(64, 65, 66, 67, 68)), interval = "confidence")

# regressao multipla
fit1 = lm(iris$Petal.Length ~ iris$Petal.Width + iris$Sepal.Length)
fit1
summary(fit1)

anv2 = aov(iris$Petal.Length ~ iris$Petal.Width + iris$Sepal.Length)
summary(anv2)

fit2 = lm(iris$Petal.Length ~ iris$Petal.Width * iris$Sepal.Length)
fit2
summary(fit2)

fit3 = lm(iris$Petal.Length ~ iris$Species)
fit3
summary(fit3)

fit4 = lm(iris$Petal.Length ~ iris$Petal.Width + iris$Species)
fit4
summary(fit4)

