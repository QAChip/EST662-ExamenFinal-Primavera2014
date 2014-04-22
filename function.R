# ---- X* ----
randx=function(n, j, mu, sigma){
  #n=número de filas
  #j=número de columnas
  X=matrix(ncol=j, nrow=n)
  uno=rep(1,n)
    for(i in 1:n){
    for(j in 1:j){
      X[i,j]=rnorm(1, mean=(mu-j), sd=sigma)
    }#cierra for(j in 1:j){
  }#cierra for(i in 1:n){
  return(X=cbind(uno,X))
}

# ---- x ----
X=randx(n=25, j=10,mu=11,sigma=1)

# ---- beta ----
beta=c(5,seq(1,10,1))/10

# ---- mu ----
mu=X%*%beta

# ---- Yi ----
yi=rlnorm(25, meanlog=mu, sdlog=1)

# ---- censura ----
c=sort(yi)[20]
cen=yi>c # qué datos son sensurados
scen=yi<=c # qué datos no son cesurados
yi[cen]=exp(25)

# ---- particion ----
Xc=X[cen,] ##No se usa
Xsc=X[scen,] ##No se usa

# ---- trans ---- 
y=log(yi)

# ---- em.censurada ----
em.censurada=function(y,x,c,n){
  #y vector de datos censurados ~\sim N(mu_i,sigma)
  #x matriz de datos [1,x*]
  #c valor en el que están truncadas las observaciones
  cen=y>=c
  no.cen=y<c
  ly=lm(y[no.cen]~X[no.cen,-1])
  b=coef(ly)
  sigma=sqrt(deviance(ly)/(sum(no.cen)))
  for(i in 1:n){
    media=c(X%*%b)
    z=(y-media)/sigma
    sesgo=sigma*dnorm(z)/(1-pnorm(z))
    y[cen]=c(media+sesgo)[cen]
    ly=lm(y~X[,-1])
    b=coef(ly)
    suma=sum((z*sesgo*sigma+sigma^2-sesgo^2)[cen])
    sigma=sqrt((deviance(ly)+suma)/nrow(X))
  if(i==n){
    return(ly)
  }#cierra if(i==n){
  } #for(i in 1:n){
}#cierra function(y,x,c,n)

# ---- input ----
m=2
yc=em.censurada(y, X, 25, n=m) #n=1 el sesgo es muy peqeño

# ---- coeficientes
beta=matrix(yc$coefficients,nrow=1)
colnames(beta)=c("b0","b1","b2","b3","b4","b5","b6",
                                "b7","b8","b9","b10")
xtable(beta,caption="Coeficientes estimados tras $m$ iteraciones",
       label="tab:coef",digits=4)

# ---- ycen ----
yc$fit[cen]

# ---- input2 ----
m=10
yc=em.censurada(y, X, 25, n=m) #n=1 el sesgo es muy peqeño

# ---- ycen2 ----
xtable(matrix(yc$fit[cen],ncol=5),
       caption="Estimación de las observaciones $y_i$ truncadas")