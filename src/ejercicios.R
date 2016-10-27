# ejercicio de vectores
1:20
20:1
c(1:20,19:1)
tmp <- c(4,6,3)
rep(tmp,10)
rep(tmp,l=31)
rep(tmp,times=c(10,20,30))

#ejercicio de vectores de caracteres
paste("etiqueta", 1:30)
paste("fn", 1:30, sep="")

#ejercicio de matrices
1. (a)
m <- matrix( c(1,5,-2,1,2,-1,3,6,-3),nr=3)
m
m%*%m%*%m
m[,3] <- m[,2] + m[,3]