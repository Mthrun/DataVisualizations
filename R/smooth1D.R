smooth1D = function(Y, lambda) {
#  Z = smooth1D(Y,lambda)
# apply the smoother lineweise to a histogram given in a matrix Y:
# Z is the smoother matrix  see equation 8 in 04 Eilers
  if(is.vector(Y)){
    warning("smooth1D expected matrix. Calling as.matrix()")
    Y=as.matrix(Y)
  }

  dd = dim(Y)
  m = dd[1] #m  zeilennlaenge von Y
  #n = dd[2]
  #[m,n] = size(Y)
  E = diag(nrow = m,ncol = m) # E(1:m,1:m) Einheitsmatrix
  D1 = (diff(E, 1)) #  row differences: [X(2:m,:)-X(1:m-1,:)]
  D2 = (diff(D1, 1))# D2(1:m-2,1:m) mit 1 -2 1 auf der Diagonale
  #muss das ein skalar sein?
  #matlab: P = lambda.^2 .* D2'*D2 + 2.*lambda .* D1'*D1;
  P = lambda ^ 2 * ( t(D2) %*% D2) + 2 * lambda * ( t(D1) %*% D1) #penalty 
  Z= solve((E+P),Y)  # Gleichung 8 aus 04 Eilers aufgeloest nach Z
  return(Z)
}
# end function Z = smooth1D(Y,lambda)
