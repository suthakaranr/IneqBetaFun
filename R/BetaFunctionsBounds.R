## Theorem 3.1

them_3_1 = function(x,y){
  cond_1 = x/(x+1) + y/(y+1)
  cond_2 = x*y
  if(x > 0 && y > 0 && cond_1 <=1 && cond_2 <1){
    return(c("beta_xy =", beta(x,y), 
             "lower bound = ", (x+y)*(1 - sqrt(x*y))^2 / (x*y*(x+1)*(y+1))))
  }
  else{
    return("Try x > 0 & y > 0 & x/(x+1) + y/(y+1) <=1 & x*y < 1")
  }
}

#them_3_1(1/2,1/2)


## Theorem 3.2


them_3_2 = function(x,y){
  cond_1 = x/(x+1) + y/(y+1)
  z = (1 - x/(x+1) - y/(y+1))/ sqrt(x/((x+1)^2 * (x+2)) + y/((y+1)^2 * (y+2)) )
  if(cond_1 <=1 && x > 0 && y >0){
    return(c("beta_xy =", beta(x,y), "lower bound = ", (x+y)/x * z^2 /(y*(z^2 + 1)) ) )
  }
  else{
    return("Try x >0 & y > 0 & x/(x+1) + y/(y+1) < 1")
  }
}

#them_3_2(1/2,1/2)


## Theorem 3.3

them_3_3 = function(x,y){
  cond_1 = x/(x+1) + y/(y+1)
  if(cond_1 <=1 && x > 0 && y >0){
    return(c("beta_xy =", beta(x,y), "lower bound = ",  (x+y)/x * (1 - x/(x+1) - y/(y+1))  ))
  }
  else{
    return("Try x >0 & y > 0 & x/(x+1) + y/(y+1) <= 1")
  }
}

#them_3_3(1/2,1/2)


## Theorem 3.4

them_3_4 = function(x,y){
  cond_1 = x/(x+1) + y/(y+1)
  cond_2 = x*y
  s = 0.5 * log(1/(x*y))
  w_xy = exp(-s) * (1 + s * (x/(x+1)) 
                    + (exp(s) - 1- s)* x/(x+2) ) * (1 + s * (y/(y+1)) + (exp(s) - 1- s)* y/(y+2) )
  if(cond_1 <=1 && x > 0 && y >0 && cond_2 <=1){
    return(c("beta_xy =", beta(x,y), "lower bound = ",  (x+y)/x * (1 - w_xy)  ))
  }
  else{
    return("Try x >0 & y > 0 & x/(x+1) + y/(y+1) <= 1 & x*y < 1")
  }
}

#them_3_4(1/2,1/2)


## Theorem 3.5

them_3_5 = function(x,y){
  cond_1 = 1/(x+1) + 1/(y+1)
  k_xy = (1 - (x/(x+1) + y/(y+1)) )/ sqrt(x/((x+1)^2 * (x+2)) + y/((y+1)^2 * (y+2)) )
  p_xy = 1/(1 + k_xy^2)
  if(cond_1 <=1 && x > 0 && y >0){
    return(c("beta_xy =", beta(x,y), "upper bound = ",  (x+y)/(x*y) * p_xy  ))
  }
  else{
    return("Try x >0 & y > 0 & 1/(x+1) + 1/(y+1) <= 1")
  }
}

#them_3_5(2,2)



## Theorem 3.6

them_3_6 = function(x,y){
  cond_1 = (x+1)/x
  k_xy = (1/y - x/(x+1) )/ sqrt(x/((x+1)^2 * (x+2)) + y/((y+1)^2 * (y+2)) )
  p_xy = 1/(1 + k_xy^2)
  if(x > 0 && y >cond_1 && cond_1>1){
    return(c("beta_xy =", beta(x,y), "upper bound = ", 1/x * p_xy  ))
  }
  else{
    return("Try x > 0 and y > (x+1)/x > 1")
  }
}

#them_3_6(2,2)



## Theorem 3.7

them_3_7 = function(x,y){
  if(x > 0 && y >0 && x <= y){
    if(x < 1 && x + y <= 1){
      return(c("beta_xy_a =", beta(x,y), "lower bound = ", (1-x)^(x+y-1) * pi/sin(pi*x)  ))
    }
    else if(x < 1 && x + y >=2){
      return(c("beta_xy_a =", beta(x,y), "lower bound = ", (1-x)^(x+y-1) * pi/sin(pi*x)  ))
    }
    else  (x < 1 && x + y > 1 &&  x + y < 2)
    return(c("beta_xy_b =", beta(x,y), "upper bound = ", (1-x)^(x+y-1) * pi/sin(pi*x)  ))
  }
  else{
    return("Try x > 0 and y> 0")
  }
}

#them_3_7(0.25,0.5)
#them_3_7(0.5,1)



## Theorem 3.8

them_3_8 = function(x,y){
  if(x > 1 && y > 1){
    return(c("beta_xy =", beta(x,y), 
             "upper bound = ", ((x-1)/(x+y-2))^(x-1) * ((y-1)/(x+y-2))^(y-1)  ))
  }
  else{
    return("Try x > 1 and y > 1")
  }
}

#them_3_8(2,2)



## Theorem 3.10

them_3_10 = function(x,y){
  u_xy = function(x,y) (1/y) * ( (1/(1+y))^(x-1) 
                                 +  (0.5 * (x-1) * ( (1+y)^(x-2) -1)) / ( (y-1)^(x-1) * (y+2) ))
  if(x >= 3 && y > 0 && y < 3){
    return(c("beta_xy_a =", beta(x,y), "upper bound = ",  u_xy(x,y) ))
  }
  else if (x >= 3 && y >= 3)
    return(c("beta_xy_b =", beta(x,y), "upper bound = ",  min(u_xy(x,y), u_xy(y,x)) ))
  else{
    return("Try other values")
  }
}

# x >= 3 && y > 0 && y < 3
#them_3_10(4,2)

# x >= 3 && y >= 3
#them_3_10(4,3.5)



## Theorem 3.11

them_3_11 = function(x,y){
  u_xy = function(x,y) (1/y) * ( (1/(1+y))^(x-1) +  (0.5 * (x-1) * y) / ( (y+1)^(x-1) * (y+2) ))
  if(x >= 3 && y > 0 && y <3){
    return(c("beta_xy_a =", beta(x,y), "lower bound = ",  u_xy(x,y) ))
  }
  else if (x >= 3 && y >= 3)
    return(c("beta_xy_b =", beta(x,y), "lwoer bound = ",  max(u_xy(x,y), u_xy(y,x)) ))
  else{
    return("Try other values")
  }
}

# x >= 3 && y > 0 && y <3
#them_3_11(4,2)

# x >= 3 && y >= 3
#them_3_11(5,5)



## Corollary 2

corollary_2 = function(x,y){
  u_xy = function(x,y) (1/y) * x^x * (1/(x+y))^(x+y) * (y+1)^(y+1)
  if( x >=1 && y>=1 && x<=y ){
    return(c("beta_xy_a =", beta(x,y), "upper bound = ", min(u_xy(x,y), u_xy(y,x)) ))
  }
  else{
    return("Try x >=1 && y>=1 && x<=y ")
  }
}

#corollary_2(1, 1.5)




## Theorem 3.14

them_3_14 = function(x,y){
  if(x > 0 && y > 0 ){
    return(c("beta_xy =", beta(x,y), "lower bound = ", (x/(x+y))^x * (exp(-x)/sqrt(x))*sqrt(2*pi)  ))
  }
  else{
    return("Try x > 0 && y > 0")
  }
}

#them_3_14(1,2)




## Theorem 3.15

them_3_15 = function(x,y){
  if(x > 0 && y > 0 &&  x <=y && x< Inf && y < Inf ){
    return(c("beta_xy =", beta(x,y), 
             "lower bound = ", (x^x * y^y) /(x + y)^(x + y) * sqrt(2*pi/x)  ))
  }
  else{
    return("Try x > 0 && y > 0")
  }
}

#them_3_15(1,2)

