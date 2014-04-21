# ---- X* ----
randx=function(n, j, mu, sigma){
  X=matrix(ncol=j, nrow=n)
  uno=rep(1,n)
    for(i in 1:n){
    for(j in 1:j){
      X[i,j]=rnorm(1, mean=(mu-j), sd=sigma)
    }#cierra for(j in 1:j){
  }#cierra for(i in 1:n){
##Aplicar la función para generar X y Y  
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
c=sort(yi)[20]

# ---- censura ----
c=sort(yi)[20]
cen=yi>c
scen=yi<=c
yi[cen]=exp(25)

# ---- particion ----
Xc=X[cen,]
Xsc=X[scen,]

y=log(yi)
c=25
# ---- imput----
em.censurada=function(y,x,c,n){
  #y vector de datos censurados ~\sim N(mu_i,sigma)
  #x matriz de datos [1,x*]
  #c valor en el que están truncadas las observaciones
  cen=y>=c
  no.cen=y<c
  on=cen*1
  ly=lm(y[no.cen]~X[no.cen,-1])
  b=coef(ly)
  sigma=deviance(ly)/df.residual(ly)
  for(i in 1:n){
    media=c(X%*%b)
    z=c(y-media)/sigma
    sesgo=c(sigma*dnorm(z)/(1-pnorm(z)))
    #y.old=y.new
    y.new=c(media+sesgo)*on + y*(1-on)
    ly=lm(y.new~X[,-1])
    b=coef(ly)
    suma=sum((z*sesgo*sigma+sigma^2-sesgo^2)
              *on)
    sigma=(deviance(ly)+suma)/df.residual(ly)
    cat(media[cen],fill=T)
  if(i==n){
    return(list(coefficients=b, variance=sigma,y=y.new))
  }#cierra if(i==n){
  } #for(i in 1:n){
}#cierra function(y,xc,xsc)

y
# ---- imput ----
em.censurada(y,X, 25, 9)