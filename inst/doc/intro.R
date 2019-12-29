## -----------------------------------------------------------------------------
LRL<-function(x,y,lambda=1,epsilon=0.1){
  Sc<-function(B1,B0,a1){
    sc<-list()
    for (i in 1:4){
      sc<-c(sc,list(B1[[i]]+(a1-1)/a1*(B1[[i]]-B0[[i]])))
    }
    return(sc)
  }

  Gradient<-function(x,y,S,i){
    n<-dim(x)[1]
    p<-matrix(0,64,64)
    for ( j in 1:n){
      p<-p+x[j,i]^2*S[[i]]-x[j,i]*(y[[j]]-x[j,1]*S[[1]]-x[j,2]*S[[2]]-x[j,3]*S[[3]]
                                   -x[j,4]*S[[4]]+x[j,i]*S[[i]])
    }
    p/n
  }
  QB<-function(B_hat,x,y,lambda=0){
    v<-list()
    n<-dim(x)[1]
    mse<-numeric(n)
    for (i in 1:n){
      a<-y[[i]]-x[i,1]*B_hat[[1]]-x[i,2]*B_hat[[2]]-x[i,3]*B_hat[[3]]-x[i,4]*B_hat[[4]]
      mse[i]<-sum(a^2)
    }
    v$MSE<-mean(mse)/2
    m<-length(B_hat)
    d<-numeric(m)
    for ( i in 1:m){
      d[i]<-sum(svd(B_hat[[i]])$d)
    }
    v$Bnorm<-lambda*sum(d)
    v$QB<- v$MSE+v$Bnorm
    v$norm<-sum(d)
    return(v)
  }
  delta<-length(y)/max(svd(t(x)%*%x)$d)
  B1<-B0<-list(matrix(0,64,64),matrix(0,64,64),
               matrix(0,64,64),matrix(0,64,64))
  a0<-0;a1<-1;C<-1
  while (isTRUE(C>epsilon)){
    S1<-Sc(B1,B0,a1)
    B0<-B1
    for (i in 1:4){
      A<-S1[[i]]-delta*Gradient(x,y,S1,i)
      sv<-svd(A)
      b<-sv$d-lambda*delta
      b<-(abs(b)+b)/2
      Bp<-sv$u%*%diag(b)%*%t(sv$v)
      B1[[i]]<-Bp
    }
    a1<-(1+sqrt(1+(2*a1)^2))/2
    C<-abs(QB(B1,x,y,lambda)$QB-QB(B0,x,y,lambda)$QB)

  }
  return(B1)
}

## -----------------------------------------------------------------------------
data(xy)
x<-xy$X
y<-xy$Y
B<-xy$B
B_hat<-LRL(x,y)
image_B<-function(B_hat){
  image(B_hat[[1]])
  image(B_hat[[2]])
  image(B_hat[[3]])
  image(B_hat[[4]])
}

## -----------------------------------------------------------------------------
image_B(B)

## -----------------------------------------------------------------------------
image_B(B_hat)

## -----------------------------------------------------------------------------
EMg<-function(e1,X,t_max=100){
  f<-function(e,X){
    sigma1<-as.matrix(e$sigma[[1]])
    sigma2<-as.matrix(e$sigma[[2]])
    M<-matrix(0,dim(X)[1],dim(X)[2])
    for (i in 1:dim(X)[1]){
      X1<-X[i,]
      m1<-dmvnorm(X1,e$mu[[1]],sigma1)
      m2<-dmvnorm(X1,e$mu[[2]],sigma2)
      M[i,1]<-e$lambda[1]*m1
      M[i,2]<-e$lambda[2]*m2
    }
    return(M)
  }
  n<-dim(X)[1]
  for (i in 1:t_max){
    p<-f(e1,X)
    p1<-p/apply(p,1,sum)
    e1$lambda<-c(sum(p1[,1])/n,sum(p1[,2])/n)
    e1$mu<-list(as.vector(p1[,1]%*%X/sum(p1[,1])),as.vector(p1[,2]%*%X/sum(p1[,2])))
    q1<-apply(X,1,function(X) X-e1$mu[[1]])
    sigma1<-q1%*%diag(p1[,1])%*%t(q1)/sum(p1[,1])
    q2<-apply(X,1,function(X) X-e1$mu[[2]])
    sigma2<-q2%*%diag(p1[,2])%*%t(q2)/sum(p1[,2])
    e1$sigma<-list(sigma1,sigma2)
  }
  return(e1)
}

## -----------------------------------------------------------------------------
library(mvtnorm)
e1<-list()
e1$mu<-list(c(5,62),c(6,85))
e1$lambda<-c(.3,.7)
e1$sigma<-list(diag(2),diag(2))
e_hat<-EMg(e1,as.matrix(faithful),100)
e_hat


