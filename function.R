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
mu=X[1:20,]%*%beta

# ---- Yi ----
yi=rlnorm(20, meanlog=mu, sdlog=1)
yi=c(yi,rep(exp(25),5))
yi/exp(25)
plot(yi)
