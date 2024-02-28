y = 3.1
z = 6.2
y / z
w = y * z + 2
w

y=5
y^2
y%%2
y%/%2

v1 = c(1,5,8,10)
v1
v2 = c(3,v1)
v2

1:10
10:2

seq(1, 2, 0.1)
seq(1, 10)

x = c(2,4,6)
rep(x, times=5)
rep(x, each=5)

x = 1:10
2*x+1
x/2
x-5

vm = 1:5
vn = 1:10
vm+vn

vm = 1:3
vn = 1:10
vm+vn

x[2]
x[x>5]

v1 = c(1,5,8,10)
v1
ind=2:3
v1[ind]
v1[-2]
v1[-(2:3)]
names(v1) = c("azul","amarelo","verde","vermelho")
v1
v1["verde"]

v =c(-1,2,3,-4)
abs(v)
sqrt(abs(v))
sin(pi/2*v)
nr = c(1.23, 2.321, 4.07654, 3, 2.345)
round(nr, 1)
p10 = c(10,100,1000,10000)
log10(p10)
mapply(sqrt, abs(v))
mapply("/",p10,10)

v = c(2,4,7,6,3,1)
rev(v)
sort(v)
sort(v, decreasing=TRUE)
order(v)
v[order(v)]
unique(c(1,2,1,2,3))
cumsum(v)
cumprod(v)
diff(v)

vs = c(3,5,7,9)
sum(vs)
mean(vs)
min(v)
max(v)
which.max(v)
which.min(v)
range(v)

vb1 = c(TRUE,TRUE,FALSE,TRUE,FALSE)
vb1
vb1[3]
v = 1:10
vb2 = v > 5
vb2
vb2 | vb1
vb1 & !vb2
all(vb1)
any(vb1)
which (vb1)

nome = "paulo"
apelido = "silva"
toupper(nome)
paste(nome, apelido)
substr(apelido,2,4)
nchar(nome)
sub("lo","la",nome)
str = "abracadabra"
gsub("a","x",str)
chartr("abc","ABC",str)

nomes = c("joao", "joaquim", "jose")
apelidos = c("silva", "sousa")
paste(nomes, apelidos, sep=" ")
toupper(nomes)
mapply(nchar,nomes)
sub("j","J",nomes)

racas = c("bulldog", "rafeiro", "doberman", "rafeiro", "bulldog", "rafeiro", "rafeiro", "doberman")
fr = factor(racas)
fr
levels(fr)
table(fr)
pesos = c(12, 15, 35, 10, 20, 8, 13, 25)
tapply(pesos,fr,mean)

mat = matrix(1:20, 4, 5)
mat
mat[2,]
mat[,1]
mat[2:3,]
mat[2,3]
mat[1:2,4:5]
dim(mat)
nrow(mat)
ncol(mat)

m1 <- cbind(x, x^2, x^3)
m1
rbind(m1, c(20, 40, 60))

mean(mat)
sqrt(mat[2:3,3:4])
apply(mat, 1, sum)

A = matrix(1:4,2,2)
B = matrix(4:1,2,2)
C= A%*%B
C
t(C)
det(C)

A = matrix(1:4,2,2)
b = c(1,3)
solve (A,b)

auto = list(marca="ford", modelo="fiesta", nportas=5, velocMax=155, consumos=c(6,7.1,9.3))
auto
is.list(auto)
auto[[1]]
auto$marca

library(MASS)
data(Cars93)
head(Cars93)

r = c("bulldog", "rafeiro", "doberman", "rafeiro", "bulldog", "rafeiro", "rafeiro", "doberman")
p = c(12, 15, 35, 10, 20, 8, 13, 25)
t = c("medio", "medio", "grande", "pequeno", "grande", "pequeno", "medio", "grande")
df = data.frame(racas=r, tamanhos=t, pesos=p)
df
df$tamanhos
df$pesos[1:4]
df[2,2]
df[1:3,]
df[,2]
df[df$racas=="bulldog",]
df[df$racas=="bulldog" & df$tamanhos=="medio",]
vl = df$pesos > 12
vl
df[vl,]

"quadrado" = function(x) x^2
quadrado(3)

"volume.esfera" = function (r)
# esta função calcula o volume de uma esfera
{
  res = 4/3 * pi * r^3
  res
}
volume.esfera(2)

"distancia" = function(x1, y1, x2, y2)
{
  r = (x1-x2)^2 + (y1-y2)^2
  sqrt(r)
}
distancia(0,0,1,1)
distancia(0,2,2,0)

for (k in 1:5){ 
  print(k) 
}

i <- 1 
while (i < 6) {
  print(i) 
  i = i+1 
}

x <- -5 
if(x > 0){
  print("Non-negative number")
} else { 
  print("Negative number") 
}


