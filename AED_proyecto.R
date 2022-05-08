#PCA
idh_1_[86,6]<-0.92
Data = idh_1_[2:8]

S = cov(Data)
S = cov2cor(S)
cov_eigen <- eigen(S)
vecs = cov_eigen$vectors
vals = cov_eigen$values

PCA <-princomp(scale(Data))
summary(PCA)

#Comp 3 -> Economia
#Comp 2 y 4 -> Salud y escolaridad
#Comp 1 -> Idh


#Regresion lineal 


#obs 
n=124

#vars
r=7

#confianza
alpha=0.05


z1 <- idh_1_$`esperanza de vida`
z2 <- idh_1_$`Tasa mortalidad`
z3 <- idh_1_$`Tasa Natalidad`
z4 <- idh_1_$`Indice Fecundidad`
z5 <- idh_1_$PIB
z6 <- idh_1_$`Indice Primaria`

y <- idh_1_$IDH


#vec de 1s de tamano n 
ones <- replicate(n, 1)

#matriz de obs de las vars predictoras
z<- matrix(c(ones,z1,z2,z3,z4,z5,z6), nrow = n, byrow = FALSE)

#estimadores de B
beta_hat <- solve(t(z)%*%z)%*%t(z)%*%y

#Estimador de la prediccion 
y_hat <- z%*%beta_hat

#Estimador Error
err_hat <- y- y_hat

#Creacion modelo
model <- lm(y~z1+z2+z3+z4+z5+z6)
summary(model)

m <- lm(y~z5)
summary(m)

#Estimate: B (intercept B0)
#t val y Pr(val p) asoc a PH: Bi = 0 
#* es significancia
#Rsq : variacion de prediccion explicada por model 



#IC Betas

interv<-c()

s_sq <- (t(err_hat)%*%err_hat)/(n-r-1)

var_bh <- s_sq[1]*solve(t(z)%*%z)

#Para B1 y B2
for(i in 2:3){
  interv[2*(i-1)-1] <- beta_hat[i]-sqrt(var_bh[i, i])*sqrt((r+1) * qf(alpha,df1 = r+1, df2 = n-r-1, lower.tail = FALSE))
  interv[2*(i-1)] <- beta_hat[i]+sqrt(var_bh[i, i])*sqrt((r+1) * qf(alpha,df1 = r+1, df2 = n-r-1, lower.tail=FALSE))
}


#E[Y] para vals ze1 y ze2
ze1 = 6
ze2 = 4

E_y = beta_hat[1] + ze1* beta_hat[2] + ze2*beta_hat[3]


#I.Pred de E[Y]

z0 = c(1, ze1, ze2)

int_y = c()
int_y[1] = t(z0)%*%beta_hat - qt(alpha/2, df= n-r-1, lower.tail=FALSE)*sqrt(s_sq * (1+t(z0)%*%solve(t(z)%*%z)%*%z0))
int_y[2] = t(z0)%*%beta_hat + qt(alpha/2, df= n-r-1, lower.tail=FALSE)*sqrt(s_sq * (1+t(z0)%*%solve(t(z)%*%z)%*%z0))

#I.conf de E[Y]

int_Ey = c()
int_Ey[1] = t(z0)%*%beta_hat - qt(alpha/2, df= n-r-1, lower.tail=FALSE)*sqrt(s_sq * (t(z0)%*%solve(t(z)%*%z)%*%z0))
int_Ey[2] = t(z0)%*%beta_hat + qt(alpha/2, df= n-r-1, lower.tail=FALSE)*sqrt(s_sq * (t(z0)%*%solve(t(z)%*%z)%*%z0))



