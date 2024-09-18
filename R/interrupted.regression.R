
  library('groundhog')
  pkgs=c('car','sandwich','lmtest','emmeans')
  date='2024-07-01'
  groundhog.library(pkgs,date)
  
  n=500
  x1=rep(c(0,1),n)
  z1=rnorm(2*n)
  y1.raw=ifelse(z1<0,0,z1*x1)
  e1=rnorm(2*n,sd=sd(y1.raw))
  y1=y1.raw+e1
  df.obs=data.frame(x1,z1,y1)
  
#Function
  eval2 <- function(s)  eval(parse(text=s),  parent.frame()) 
  

zc=0

dv='y1'
cond='x1'
mod='z1'
mod.cut=0
data='df.obs'

  lm.inte = function(dv,cond,mod,mod.cut,data)
  {
    
    #localize
      df=eval2(data)
      x=df[,cond]
      y=df[,dv]
      z=df[,mod]
      zc=mod.cut
    
    #Generate split
      zH=ifelse(z>=zc,1,0)
      
      
    #Model with and without interaction within  
      lm1=lm(y~x*zH)
      lm2=lm(y~x*z*zH)
      
    #Robust vcov
      v1 <- sandwich::vcovHC(lm1, type = "HC3")
      v2 <- sandwich::vcovHC(lm2, type = "HC3")
      
    #Corrected SE
      coe1 <- lmtest::coeftest(lm1, vcov = v1)
      coe2 <- lmtest::coeftest(lm2, vcov = v2)

    #Average effects of x
      #low
        avgL   = coe1[2,1]
        avgL.p = coe1[2,4]
        
      #high
        avgH   =  coe1["x","Estimate"] + coe1["x:zH","Estimate"]
        avgH.p = car::linearHypothesis(lm1, "x + x:zH = 0")
        
    #Interactions 
       #Low 
        intL   = coe2["x:z", "Estimate"]
        intL.p = coe2["x:z", "Pr(>|t|)"]
        
        intH   = coe2["x:z", "Estimate"] +  coe2["x:z:zH","Estimate"]
        intH.p = car::linearHypothesis(lm2, "x:z + x:z:zH1 = 0")

     #paste0("When ", mod, " is below ", modc," the effect of ", cond, " on ", dv,.)
        
  }


# Step 1: Fit the initial linear model with interaction
model <- lm(y ~ x * z)

# Step 2: Use the segmented() function to introduce breakpoints for both z and the interaction (x*z)
seg_model <- segmented(model, seg.Z = ~z + I(x*z), psi = list(z = 0, "I(x*z)" = 0))

# Step 3: View the summary of the segmented model
summary(seg_model)

# Optional: Plot the segmented relationship
plot(seg_model)

# Optional: Plot the segmented relationship
plot(seg_model)
