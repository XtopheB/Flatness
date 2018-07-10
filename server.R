### ---  Flatness project - ShinyFlat -- 
# 26/06 Shiny file application working with only risk aversion parameter 
# 26/062018  : Compatibilty with Flatness function file... 

library(shiny)
library(data.table)
# My functions are loaded in the global file (global.R) ...

#source(global.R)


####  Functions 
f.EU <- function(x, 
                 beta0 = 0, 
                 beta1 = 15,
                 a1 = 0.30,
                 beta2 = 30,
                 a2 = -0.10,
                 r = 0,
                 type.U = "CRRA",
                 cx = 1, 
                 px = 11,
                 eps.bad = -1.35,
                 eps.avg = 0,
                 eps.good = 1.35,
                 ...)
{
  
  # profit
  pi.bad <- px* f.prod(x, state.nat = eps.bad , a1 = a1, a2 = a2 ) - cx*x
  pi.avg <- px* f.prod(x, state.nat = eps.avg, a1 = a1, a2 = a2 ) - cx*x
  pi.good <- px* f.prod(x, state.nat = eps.good, a1 = a1, a2 = a2 ) - cx*x
  
  #profit utility 
  
  if (type.U == "CRRA"){
    U.bad <- U.CRRA(pi.bad, r = r)
    U.avg <- U.CRRA(pi.avg, r = r)
    U.good <- U.CRRA(pi.good, r = r)
  }
  else if (type.U == "CPT") {
    U.bad <- U.CPT(pi.bad)
    U.avg <- U.CPT(pi.avg)
    U.good <- U.CPT(pi.good)
    
  }
  
  # Expected utility
  EU <-  0.2* U.bad + 0.6* U.avg + 0.2* U.good 
  return(EU)
}








#Non dynamic stuff
suppmax <- 400  #defines the support of inputs in [0, suppmax ]
Ns <- 100

##  Step 1 : Generation of equally distributed Xs and of nature state

x <- as.data.frame(seq(from = 0, to = suppmax, length.out = Ns+1 ))
x<- x[-1,]  #  to avoid start at x=0

# Distribution of nature states
set.seed(31415)
Neps <- 200
eps.norm <- rnorm(n = Neps)

# Values of espsilon for 3 states of nature (Fixed for all Xs !!)

e.bad  <- median(subset(eps.norm, eps.norm < quantile(eps.norm, 0.20)))
e.avg  <-  median(eps.norm)
e.good  <- median(subset(eps.norm, eps.norm > quantile(eps.norm, 0.80)))

df<- data.frame(x)

# Expected Profit with fixed probabilities for the 3 states
df$EW <- 0.20*f.prof(x, f.prod(x, state.nat = e.bad)) +
  0.60*f.prof(x, f.prod(x, state.nat = e.avg)) +
  0.20*f.prof(x, f.prod(x, state.nat = e.good))


# Define server computations of elements
shinyServer(function(input, output) {
  ### step 4  Expected Utility Function 
  
   
  output$FullTable <- renderTable({
    # Production function
    if (input$Risk == "CRRA"){
      df$EU <- f.EU(x, 
                    a1 = input$alpha.f,
                    a2 = input$alpha.g,
                    px =  input$p.level,
                    r = input$r.level, 
                    eps.bad = e.bad, 
                    eps.avg = e.avg, 
                    eps.good = e.good)
    
    }
    else if (input$Risk == "CPT"){
      df$EU <- f.EU.CPT(x, 
                    a1 = input$alpha.f,
                    a2 = input$alpha.g,
                    px =  input$p.level,
                    w0 = input$w0, 
                    aplus = input$aplus,
                    amoins = input$amoins,
                    lambda = input$lambda,
                    gamaplus = input$gammaplus,
                    gamamoins = input$gamamoins,
                    eps.bad = e.bad, 
                    eps.avg = e.avg, 
                    eps.good = e.good 
                    )
      
    }
    
    # Maximums
    i.star <- which.max(df$EU)
    x.star  <- df[which.max(df$EU),]$x
    EU.star <- df[i.star,]$EU
    
    # Risk premium
    fopti <- function(RP, type.U = input$Risk, 
                      ...)
    {
      if (type.U == "CRRA") {
        abs(EU.star - U.CRRA(df[i.star,]$EW - RP, r = input$r.level))
      }
      else if (type.U == "CPT") {
        abs(EU.star - U.CPT(df[i.star,]$EW - RP, 
                            w0 = input$w0, 
                            aplus = input$aplus,
                            amoins = input$amoins,
                            lambda = input$lambda))
      }
        
    }
    
    RP.sol <- optimize( fopti, interval=c(0,100), maximum = FALSE)
    RP <- round(RP.sol$minimum,digits = 2)
    
    # Certainty equivalent
    CE <- df[i.star,]$EW - RP
    
    #Certainty  equivalent at X0 (risk neutral)
    #  need to recompute EW .. Always with CRRA
    
    # Maximum for r = 0
    i.0 <- which.max(f.EU(x, r = 0, type.U = "CRRA"))
    x.0  <- df[i.0,]$x
    EU.0 <-df[i.0,]$EU
    
    # RP at x0
    fopti.0 <- function(RP)
    {
      abs(EU.0 - U.CRRA(df[i.0,]$EW - RP, r = input$r.level))
    }
    
    RP.0.sol <- optimize( fopti.0, interval=c(0,1000), maximum = FALSE)
    RP.0 <- round(RP.0.sol$minimum,digits = 2)
    
    # Certainty equivalent at x0
    CE.0 <- df[i.0,]$EW - RP.0
    
    DeltaCE <- round((CE.0 - CE)*100/CE.0 , digits = 3)
    
    ### OUTPUT TABLE 
    if (input$Risk == "CRRA") {
      out.table <- cbind(input$p.level, input$alpha.f, input$alpha.g, input$r.level, x.star, RP, CE, CE.0, DeltaCE)
      colnames(out.table) <- c("output price", "alpha.f", "alpha.g", "r", "x*", "RP(x*)", "CE(x*)", "CE(x0*)", "DeltaCE")
      data.table(out.table)
    }
    else if (input$Risk == "CPT") {
      out.table <- cbind(input$p.level, input$alpha.f, input$alpha.g, 
                        input$w0, 
                         input$aplus,
                         input$amoins,
                         input$lambda,
                         input$gammaplus,
                         input$gammamoins,
                         x.star, RP, CE, CE.0, DeltaCE)
      colnames(out.table) <- c("output price", "alpha.f", "alpha.g", "w0", "a+", "a-", "lambda", "gamma+", "gamma-", "x*", "RP(x*)", "CE(x*)", "CE(x0*)", "DeltaCE")
      data.table(out.table)
     
    } 
    
  })
  
})
