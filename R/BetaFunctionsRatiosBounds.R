## Theorem 3.9

them_3_9 = function(x1,x2){
  if(x1 > 0 && x2 >0 ){
    if( x1 <= x2 ){
      return(c("beta_ratio_a =", beta(x2,x2)/beta(x1,x1), "upper bound = ", (1/4)^(x2-x1)  ))
    }
    else ( x1 > x2 )
    return(c("beta_ratio_b =", beta(x2,x2)/beta(x1,x1), "lower bound = ", (1/4)^(x2-x1)  ))
  }
  else{
    return("Try x1 >0 and x2 > 0")
  }
}


# x1 <= x2
#them_3_9(0.5,0.75)

# x1 > x2
#them_3_9(1.5,1) 


## Theorem 3.12

them_3_12 = function(y, x1,x2){
  if(y > 0 && x1 > 0 && x1 <= x2 && x2 < Inf ){
    return(c("beta_ratio_a =", beta(x2,y)/beta(x1,y),
             "upper bound = ", x2^x2 *(1/x1)^x1 * (1/(x2 + y))^(x2 + y) * (x1 + y)^(x1 + y)  ))
  }
  else{
    return("Try y > 0,  x1 > 0 and x1 <= x2 < Inf")
  }
}


## Corollary 1

corollary_ratio_1 = function(x1,x2, y1, y2){
  g_abc = function(a, b, c) b^b * (1/a)^a * (1/(b+c))^(b+c) * (a+c)^(a+c)
  if( y1 > 0 && y1 <= y2 && y2 < Inf  && x1 > 0 && x1 <= x2 && x2 < Inf ){
    return(c("beta_ratio_xy =", beta(x2,y2)/beta(x1,y1), 
             "upper bound = ", g_abc(x1, x2, y2)* g_abc(y1, y2, x1)  ))
  }
  else{
    return("Try y1 > 0 and y1 <= y2 < Inf,  x1 > 0 and x1 <= x2 < Inf")
  }
}

#corollary_ratio_1(1, 1.5,2, 3)




## Theorem 3.13


them_3_13 = function(y, x1,x2){
  if(y > 0 && x1 > 0 && x1 <= x2 && x2 < Inf ){
    return(c("beta_ratio_a =", beta(x2,y)/beta(x1,y), "upper bound = ", ((x1+y)/(x2+y))^y  ))
  }
  else{
    return("Try y > 0 && x1 > 0 && x1 <= x2 && x2 < Inf")
  }
}


# y > 0 && x1 > 0 && x1 <= x2 && x2 < Inf 
#them_3_13(1, 1.5,2)



## Theorem 3.16

them_3_16 = function(y, x1,x2){
  if(y > 0 && x1 > 0 && x1 < x2 && x2 < Inf ){
    return(c("beta_ratio_a =", beta(x2,y)/beta(x1,y), 
             "lower bound = ", ((x1*(x2+y))/(x2*(x1+y)))* x2^x2 * (1/x1)^x1 * (1/(x2 +y))^(x2 +y) *(x1 +y)^(x1+y)  ))
  }
  else{
    return("Try y > 0 && x1 > 0 && x1 < x2 && x2 < Inf")
  }
}

#them_3_16(1, 1.5,2)



## Theorem 3.17

them_3_17 = function(y, x1,x2){
  c = 1  +  (y*log( (x2+y)/(x1 + y) ))/(x1 - x2)
  a1 = log(c) - 1
  a2 = 1/c
  if(y > 0 && x1 > 0 && x1 < x2 && x2 < Inf ){
    return(c("beta_ratio_a =", beta(x2,y)/beta(x1,y),
             "upper bound = ", exp((a1 + a2)*(x2 - x1)) * ( (x1 + y)/(x2 + y) )^(a2 * y)  ))
  }
  else{
    return("Try y > 0 && x1 > 0 && x1 < x2 && x2 < Inf")
  }
}

#them_3_17(1, 1.5,2)
