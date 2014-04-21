# ---- X* ----
randx=function(n, j, mu, sigma){
  X=matrix(ncol=j, nrow=n)
  uno=rep(1,n)
  for(i in 1:n){
    for(j in 1:j){
      X[i,j]=rnorm(1, mean=(mu-j), sd=sigma)
    }#cierra for(j in 1:j){
  }#cierra for(i in 1:n){
  return(cbind(uno,X))
}

# ---- beta ----
beta=c(5,seq(1,10,1))

# ---- mu ----
mu=X%*%beta

# ---- Yi ----
yi=rlnorm(20, meanlog=mu, sdlog=1)


