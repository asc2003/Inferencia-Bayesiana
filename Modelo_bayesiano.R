library(readr)
data <- read_csv("OECD PISA data.csv")
str(data)
data2<-data
colT<-which(data2$SUBJECT=="TOT") #701 observacions del total
data2<-data2[-colT,] #ens quedarem amb les 1386 que son de homes dones

#################SUBJECT###########################
data2$SUBJECT<-factor(data2$SUBJECT) # BOY 1 GIRL 2
data2$SUBJECTnum<-as.numeric(data2$SUBJECT)
data2$SUBJECTnum[data2$SUBJECTnum==1]<-0 #0 hombres
data2$SUBJECTnum[data2$SUBJECTnum==2]<-1 #1 mujeres

##################INDICATOR, materia###################
data2$INDICATOR<-factor(data2$INDICATOR)
niveles_INDICATOR<-levels(data2$INDICATOR)
data2$INDICATOR<-as.numeric(data2$INDICATOR)

################LOCATION#######################
#qw<-names(which(table(data2$LOCATION)==36)) #ens quedem amb els països que han participat mes cops
#filas<-data2$LOCATION%in%qw #estas son las filas con las que nos quedamos
#data2<-data2[filas,] #nos quedamos con 936 observaciones, 26 paises

data2$LOCATION2<-factor(data2$LOCATION)
niveles<-levels(data2$LOCATION2) #para saber que es cada numero
data2$LOCATION2<-as.numeric(data2$LOCATION2)

#data2$TIME2<-((data2$TIME-2000)/3)+1
#data_math<-data2[data2$INDICATOR==1,]
#data_read<-data2[data2$INDICATOR==2,]
#data_science<-data2[data2$INDICATOR==3,]

str(data2)
################TIME########################
#7 años, 2000-2018

#data3$LOCATION3<-rep(0,length(data3$LOCATION2))
#data3$LOCATION3<-data3[data3$LOCATION2==1,8]


boxplot(data2$Value~data2$LOCATION) #veiem que canvia segons la localització
boxplot(data2$Value~data2$SUBJECT) #no es veuen moltes diferencies 
boxplot(data2$Value~data2$TIME)
library(R2jags)
prova.bug<- "
model {
    for (i in 1:n){
        y[i]~dnorm(mu[i],tau)
        
        mu[i] <-b0+b1[x[i]]+b2*x2[i]+b3*x3[i]+(b1a[x[i]]+b2a*x2[i]+b3a*x3[i])*xa[i]+(b1b[x[i]]+b2b*x2[i]+b3b*x3[i])*xb[i]
        xa[i]<-(x4[i]==2)
        xb[i]<-(x4[i]==3)
    }
    
    b0~dnorm(500,1/10000)
    b3~dnorm(0,1.0E-8)
    for (k in 1:47){
      b1[k]~dnorm(0,tau2)
    }
    tau2~dgamma(0.01,0.001)
    b2~dnorm(0,1.0E-8)
    tau~dgamma(0.01,0.001)
    
   
    b3a~dnorm(0,1.0E-8)
    for (k in 1:47){
      b1a[k]~dnorm(0,tau2a)
    }
    tau2a~dgamma(0.01,0.001)
    b2a~dnorm(0,1.0E-8)
    

    b3b~dnorm(0,1.0E-8)
    for (k in 1:47){
      b1b[k]~dnorm(0,tau2b)
    }
    tau2b~dgamma(0.01,0.001)
    b2b~dnorm(0,1.0E-8)

}
"
# diferencia todas las variables por asignatura
# hombres 0 dones 1 hecho
# poner los asignaturas como variable trinaria para ver progression lo de añadir tres categorias
# buscar alumnos por poblacion
# 1/100 en tau general sumar
# poner años,b4 añadirlos como efectos aleatorios como b1
# tau de b0 hacerla mas pequeña
#tau 3 poner que la esperança sea mas o menso entre 50 A 150
#añadir todos los paises

Iter <- 10000 #i farem inferencia amb les 10000 seguents
Burn <- 1000 #tirarem les 1000 primeres simulacions
Chain <- 2 #farem dues cadenes

data3<-data2

n<-length(data3$Value)
data <- list(n=n, y = data3$Value, x = data3$LOCATION2, x2 = data3$SUBJECTnum,x3=data3$TIME,x4=data3$INDICATOR)

initial <- list(list(tau=1,tau2=1, b0=0, b1=rep(0,47), b2=10,b3=0,tau2a=1, b1a=rep(0,47), b2a=10,b3a=0,tau2b=1, b1b=rep(0,47), b2b=10,b3b=0),list(tau=2,tau2=2, b0=10, b1=rep(10,47), b2=-10,b3=2,tau2a=2, b1a=rep(10,47), b2a=-10,b3a=2,tau2b=2, b1b=rep(10,47), b2b=-10,b3b=2))

parameters <- c("mu","b0", "b1", "b2","b3","tau2","tau", "b1a", "b2a","b3a","tau2a", "b1b", "b2b","b3b","tau2b")

model <- jags(data, initial, parameters.to.save=parameters, 
               model=textConnection(prova.bug),                 
               n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)

sqrt(sum((model$BUGSoutput$mean$mu-data3$Value)^2)/1386)
print(model2)
traceplot(model, mfrow = c(1,1), varname = c("b0a", "b2"), col=c("black","red")) #veiem que han convergit
traceplot(model, mfrow = c(1,1), varname = c("b3"), col=c("black","red")) #aquestes tambe

#b0 y b2 y b3 si convergen
#b1 parece que tambien
#tau2 parece que si
#tau5 no convergen
attach.jags(model)
tau2<-tau2
tau2a<-tau2a
tau2b<-tau2b
b0<-b0
b1<-b1
b2<-b2
b3<-b3
tau<-tau
b1a<-b1a
b2a<-b2a
b3a<-b3a
b1b<-b1b
b2b<-b2b
b3b<-b3b
detach.jags()
sigma<-1/tau
sigma2<-1/tau2
sigma2a<-1/tau2a
sigma2b<-1/tau2b

par(mfrow=c(2,2))
plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
plot(density(b1[,13], adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)")), xlab= "" ); abline(v=quantile(b1[,13],c(0.025,0.975)),lty=3)
plot(density(b1a[,13], adjust = 1.5), main = expression(paste(pi,"(",beta[1],"a|y)")), xlab= "" ); abline(v=quantile(b1a[,13],c(0.025,0.975)),lty=3)
plot(density(b1b[,13], adjust = 1.5), main = expression(paste(pi,"(",beta[1],"b|y)")), xlab= "" ); abline(v=quantile(b1b[,13],c(0.025,0.975)),lty=3)

par(mfrow=c(2,2))
plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)")), xlab= "" ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
plot(density(b2a, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"a|y)")), xlab= "" ); abline(v=quantile(b2a,c(0.025,0.975)),lty=3)
plot(density(b2b, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"b|y)")), xlab= "" ); abline(v=quantile(b2b,c(0.025,0.975)),lty=3)

par(mfrow=c(2,2))
plot(density(b3, adjust = 1.5), main = expression(paste(pi,"(",beta[3],"|y)")), xlab= "" ); abline(v=quantile(b3,c(0.025,0.975)),lty=3)
plot(density(b3a, adjust = 1.5), main = expression(paste(pi,"(",beta[3],"a|y)")), xlab= "" ); abline(v=quantile(b3a,c(0.025,0.975)),lty=3)
plot(density(b3b, adjust = 1.5), main = expression(paste(pi,"(",beta[3],"b|y)")), xlab= "" ); abline(v=quantile(b3b,c(0.025,0.975)),lty=3)

par(mfrow=c(2,2))
plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)
plot(density(sigma2, adjust = 1.5), main = expression(paste(pi,"(",sigma[2],"|y)")), xlab= "" ); abline(v=quantile(sigma2,c(0.025,0.975)),lty=3)
plot(density(sigma2a, adjust = 1.5), main = expression(paste(pi,"(",sigma[2],"a|y)")), xlab= "" ); abline(v=quantile(sigma2a,c(0.025,0.975)),lty=3)
plot(density(sigma2b, adjust = 1.5), main = expression(paste(pi,"(",sigma[2],"b|y)")), xlab= "" ); abline(v=quantile(sigma2b,c(0.025,0.975)),lty=3)

# prediction
M<-length(b0)
y_AUS_mujeres_math_2000<-rnorm(M,b0+b1[,13]+b2*0+b3*2018,sigma) #MValor esperat homes pais AUS en math
a<-max(y_AUS_mujeres_math_2000)
b<-min(y_AUS_mujeres_math_2000) 
par(mfrow=c(1,1))
plot(density(y_AUS_mujeres_math_2000),main="p(y|b1=AUS,b2=0,b3=math,2000)",xlim=c(b,a)) #veiem distribucio per la prediccio del pes del cervell d'un animal que pesa 100kg
#es la distribució predictiva a posteriori amb el cas x=100
mean(y_AUS_mujeres_math_2000)
quantile(y_AUS_mujeres_math_2000,c(0.025,0.975))

#DE MOMENTO EL QUE TIENE DIC MEJOR (CONTRA MAS BAJO MEJOR)
#ESTIMACIONES BASTANTE CERTERAS
