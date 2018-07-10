## Functions used in the Shiny app 
## 2/07/2018 : transformation of the server file- moving fucntions here


f.prod <- function(x, 
                   beta0 = 0, 
                   beta1 = 15,
                   a1 = 0.30,
                   beta2 = 30,
                   a2 = -0.10,
                   state.nat = 0)
{
  y <- beta0 + beta1 * x^a1 + beta2 * (x ^a2) *state.nat
  y[is.infinite(y)] <- 0 
  return(y)
}


## Step 3 profit fucntion 


f.prof <- function(x,  # inputs
                   y,  # prod
                   cx = 1,  #unit cost
                   px = 11 # unit price
)
{
  w = px * y - cx * x 
  return(w)
}

# Test
#f.prof(x, f.prod(x, state.nat = e.good))


## Step 3: Utility  functions
#####   CRRA 
U.CRRA <- function(w, 
                   r = 1)
{
  if (r == 1) {
    # for both cases: If w <=0 , we impute _NA_real_ which is compatible with real operations
    u <- ifelse(w>0, log(w), NA_real_) 
  }
  else {
    u <- ifelse(w>0, (1/(1-r)) * w^(1-r), NA_real_)
  }
  return(u)
}

## test 
# plot(x, U.CRRA(x, r=1))

### step 4  Expected Utility Function 

f.EU <- function(x,
                 beta0 = 0,
                 beta1 = 15,
                 a1 = 0.30,
                 beta2 = 30,
                 a2 = -0.10,
                 r = 0,
                 cx = 1,
                 px =11,
                 eps.bad,
                 eps.avg,
                 eps.good,
                 ...)
{
  
  # profit
  pi.bad <- px* f.prod(x, state.nat = eps.bad , a1 = a1, a2 = a2 ) - cx*x
  pi.avg <- px* f.prod(x, state.nat = eps.avg, a1 = a1, a2 = a2 ) - cx*x
  pi.good <- px* f.prod(x, state.nat = eps.good, a1 = a1, a2 = a2 ) - cx*x
  
  #profit utility
  U.bad <- U.CRRA(pi.bad, r = r)
  U.avg <- U.CRRA(pi.avg, r = r)
  U.good <- U.CRRA(pi.good, r = r)
  
  
  # Expected utility
  EU <-  0.2* U.bad + 0.6* U.avg + 0.2* U.good
  return(EU)
}

## test 
# plot(x, f.EU(x, eps.bad = -2, eps.good = 3, eps.avg = 0))


### CAS CPT  #### 


# Weighting function 

Pweight <-function(p,
                   w, 
                   w0 =0,
                   gamaplus = 0.61 ,
                   gamamoins = 0.69)
{
  
  Pplus  <-  (p^gamaplus)/ (p^gamaplus + (1-p)^gamaplus )^(1/gamaplus) 
  Pmoins <-  (p^gamamoins)/(p^gamamoins + (1-p)^gamamoins )^(1/gamamoins)
  P.weight <- ifelse(w >= w0, Pplus, Pmoins) 
  return(P.weight)
}


#  CPT (defined according to Tversky and Kahneman (1992))
U.CPT <-function(w,
                 w0 = 0,
                 aplus = 0.88,
                 amoins = 0.88,
                 lambda = 2.25 )
{
  u <- ifelse(w >=w0, (w-w0)^aplus ,
              ifelse(w < w0, -lambda*((w0-w)^amoins),NA_real_)
  )
  return(u)
}


#  Utility function with weights CPT (defined according to Tversky and Kahneman (1992))

f.EU.CPT <-function(x,
                    w0 = 0,
                    aplus = 0.88,
                    amoins = 0.88,
                    lambda = 2.25,
                    gamaplus = 0.61 ,
                    gamamoins = 0.69 ,
                    P.b = 0.20,
                    P.a = 0.60,
                    P.g = 0.20,
                    cx = 1,
                    px =11,
                    eps.bad,
                    eps.avg,
                    eps.good,
                    ...)
{
  # profit
  pi.bad <- px* f.prod(x, state.nat = eps.bad ) - cx*x
  pi.avg <- px* f.prod(x, state.nat = eps.avg ) - cx*x
  pi.good <- px* f.prod(x, state.nat = eps.good ) - cx*x
  
  EU <- Pweight(p = P.g, w = pi.good) * U.CPT(w =pi.good) +    # on good state
    (Pweight(p = P.g + P.a, w = pi.avg) - Pweight(p = P.g, w = pi.avg)) * U.CPT(w = pi.avg) +
    (1 - Pweight(p = P.g + P.a, w = pi.bad)) * U.CPT(w = pi.bad) 
  
  
  return(EU)
}


