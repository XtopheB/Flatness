### ---  Flatness project - ShinyFlat -- 
# 26/06 Shiny file application working with only risk aversion parameter 
# 26/062018  : Compatibilty with Flatness function file... 

library(shiny)
library(data.table)
library(directlabels)
library(ggplot2)

## My functions are loaded in the global file (global.R) ...

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


# Define server computations of elements
shinyServer(function(input, output) {

output$FullTable <- renderTable({
    
    # Expected Profit with fixed probabilities for the 3 states
    
    df$EW <- 0.20*f.prof(x,px = input$p.level, 
                         f.prod(x,  
                                beta0 = 0, 
                                beta1 = input$beta.f,
                                a1 = input$alpha.f,
                                beta2 = input$beta.g,
                                a2 = input$alpha.g,
                                state.nat = e.bad )) +
            0.60*f.prof(x,px = input$p.level, 
                        f.prod(x,  
                               beta0 = 0, 
                               beta1 = input$beta.f,
                               a1 = input$alpha.f,
                               beta2 = input$beta.g,
                               a2 = input$alpha.g,
                               state.nat = e.avg)) +
            0.20*f.prof(x,px = input$p.level,
                        f.prod(x,  
                               beta0 = 0, 
                               beta1 = input$beta.f,
                               a1 = input$alpha.f,
                               beta2 = input$beta.g,
                               a2 = input$alpha.g,
                               state.nat = e.good))
    
    # Expected utility 
    if (input$Risk == "CRRA"){
      df$EU <- f.EU(x,
                    beta0 = 0, 
                    beta1 = input$beta.f,
                    a1 = input$alpha.f,
                    beta2 = input$beta.g,
                    a2 = input$alpha.g,
                    px =  input$p.level,
                    r = input$r.level, 
                    eps.bad = e.bad, 
                    eps.avg = e.avg, 
                    eps.good = e.good)
    
    }
    else if (input$Risk == "CPT"){
      df$EU <- f.EU.CPT(x, 
                        beta0 = 0, 
                       beta1 = input$beta.f,
                        a1 = input$alpha.f,
                        beta2 = input$beta.g,
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
    
    # Maximums (whatever the model)
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
    
    # Solution and Risk premium 
    RP.sol <- optimize( fopti, interval=c(0,100), maximum = FALSE)
    RP <- round(RP.sol$minimum,digits = 2)
    
    # Certainty equivalent
    CE <- df[i.star,]$EW - RP
    
    #Certainty  equivalent at X0 (risk neutral)
    #  need to recompute EW .. Always with CRRA
    
    # Maximum of EU function with  r = 0
    #Index of the maximum
    i.0 <- which.max(f.EU(x, 
                          beta0 = 0, 
                         beta1 = input$beta.f,
                          a1 = input$alpha.f,
                         beta2 = input$beta.g,
                          a2 = input$alpha.g,
                          px =  input$p.level,
                          r = 0, 
                          eps.bad = e.bad, 
                          eps.avg = e.avg, 
                          eps.good = e.good))
      
    # level of X at the maximum 
     x.0  <- df[i.0,]$x
    
    # Expected utility at the level of intrant if risk neutral
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
      out.table <- cbind(input$p.level, input$alpha.f, input$alpha.g, 
                         input$r.level, 
                         x.star, RP, CE, CE.0, DeltaCE)
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

output$ProdGraph <- renderPlot({
  df.prod <- data.frame(x)
  df.prod$Ybad <-  f.prod(x,
                          beta0 = 0,
                          beta1 = input$beta.f,
                          a1 = input$alpha.f,
                          beta2 = input$beta.g,
                          a2 = input$alpha.g,
                          state.nat = e.bad )

  df.prod$Yavg <-  f.prod(x,
                          beta0 = 0,
                          beta1 = input$beta.f,
                          a1 = input$alpha.f,
                          beta2 = input$beta.g,
                          a2 = input$alpha.g,
                          state.nat = e.avg )
  df.prod$Ygood <-  f.prod(x,
                           beta0 = 0,
                           beta1 = input$beta.f,
                           a1 = input$alpha.f,
                           beta2 = input$beta.g,
                           a2 = input$alpha.g,
                           state.nat = e.good )


  myalpha <- 0.4
  ggplot(df.prod)+
    geom_line(aes(x, Ybad), colour = "red", size=2, alpha= myalpha) +
    geom_dl(aes(x, Ybad,label = " Bad"), method = list(dl.combine( "last.points"), cex = 1, colour = "red", alpha= myalpha))+
    geom_line(aes(x, Yavg), colour = "black", size=2, alpha= myalpha ) +
    geom_dl(aes(x, Yavg,label = " Avg"), method = list(dl.combine( "last.points"), cex = 1, colour = "black", alpha= myalpha))+
    geom_line(aes(x, Ygood), colour = "blue", size=2, alpha= myalpha) +
    geom_dl(aes(x, Ygood,label = " Bad"), method = list(dl.combine( "last.points"), cex = 1, colour = "blue", alpha= myalpha))+
    ggtitle(label = paste("Production function under the 3 states of nature (N=", Ns,")") ,
            subtitle =  paste("Production function =",input$beta.f," * x^", input$alpha.f, " +",input$beta.g,"* x^",input$alpha.g," * eps" ))+
    labs( x= "Input (???/ha)" ,
          y = "Yield (Q/ha)") +
    theme_minimal()
  #plot(x,x)
})

output$ProfGraph <- renderPlot({
  df.prof <- data.frame(x)
  df.prof$EW <-  0.20*f.prof(x,px = input$p.level,
                             f.prod(x,
                                    beta0 = 0,
                                    beta1 = input$beta.f,
                                    a1 = input$alpha.f,
                                    beta2 = input$beta.g,
                                    a2 = input$alpha.g,
                                    state.nat = e.bad )) +
                0.60*f.prof(x,px = input$p.level,
                            f.prod(x,
                                   beta0 = 0,
                                   beta1 = input$beta.f,
                                   a1 = input$alpha.f,
                                   beta2 = input$beta.g,
                                   a2 = input$alpha.g,
                                   state.nat = e.avg)) +
                0.20*f.prof(x,px = input$p.level,
                            f.prod(x,
                                   beta0 = 0,
                                   beta1 = input$beta.f,
                                   a1 = input$alpha.f,
                                   beta2 = input$beta.g,
                                   a2 = input$alpha.g,
                                   state.nat = e.good))

  myalpha <- 0.4
  ggplot(df.prof)+
    geom_line(aes(x, EW), colour = "red", size=2, alpha= myalpha) +
    geom_dl(aes(x, EW,label = " Expected Prof."), method = list(dl.combine( "last.points"), cex = 1, colour = "red", alpha= myalpha)) +
    ggtitle(label = paste("Expected Profit") ,
            subtitle =  paste("Production function =",input$beta.f," * x^", input$alpha.f, " +",input$beta.g,"* x^",input$alpha.g," * eps" ))+
    labs( x= "Input (???/ha)" ,
          y = "Profit (euro/ha)") +
    theme_minimal()
 # plot(x,x)
})



output$ModelProd <- renderText({
  paste("Production function =",input$beta.f," * x^", input$alpha.f, " +",input$beta.g,"* x^",input$alpha.g," * eps" )
})
  
})
