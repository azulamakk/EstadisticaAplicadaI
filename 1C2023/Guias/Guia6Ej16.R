cinco <- c(9, 3, 2, 7, 5, 7, 6, 3, 7)
ocho <- c(8, 2, 4, 9)
seis <- c(6, 3, 1, 4)
tres <- c(9, 7, 4, 8)

X <- cbind(cinco, ocho, seis, tres)

SCDParciales <- apply(X, 2, function(i) (i-mean(i))^2)

SCD <- sum(SCDParciales)
