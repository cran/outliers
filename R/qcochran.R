"qcochran" <-
function(p,n,k) {

f <- qf((1-p)/k,n*(k-1),n);
c <- 1/(1+(k-1)*f)

return(c)

}

