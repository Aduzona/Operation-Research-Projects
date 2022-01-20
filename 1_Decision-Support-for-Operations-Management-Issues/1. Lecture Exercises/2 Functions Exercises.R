#EXERCISES
# A. Formulate the EOQ in R,   C= (1/2 * cl * q+ d/q * co)
# B. Derive a function for calculating weighted Euclidean distance between two points.


#(A)
# Axsäter inventory control, equation 4.1, page 46 by Chapter 4 Single Echelon Systems
#EOQ = Economic Order Quantity
#C = cost per time unit
#d = demand per time unit
#q= batch quantity
#cl = holding cost per unit and time
#co = ordering or setup cost
#d/q = average numeber of orders per time.

EOQ <- function(cl, q, d, co){
  
  tmp <- ((1/2) * cl *q + (d/q) * co)
  return(tmp)
}
Quantity = EOQ(2,4,2,4)
Quantity


#(B)
#point A(x1, y1)  point B(x2,y2)
# d(A,B)= sqrt(sum(wi(Ai-Bi)^2))
#Without looping 
Weighted_Eucledian <- function(x1,y1,w1,x2,y2,w2){
  
  tmp <- sqrt(w1*(x1-x2)^2+w2*(y1-y2)^2)
  return(tmp)
}
WEuclid=Weighted_Eucledian(2,3,1,4,5,2)
WEuclid

?factorial
choose(4,1)

exp(-2)
GeoPoi <- function(n, lambda,p){
  var <- 0
  for(k in 1:n){
    var  <- var + (exp(-lambda)*((lambda^k)/factorial(k))*(1-p)^(n-k)*(p^k)*(factorial(n-1)/((factorial(k-1)*factorial(n-k)))))
    
     }
  return(var)
}

GeoPoi(10,2,60)

for(k in 1:5){
  print(k)
}

