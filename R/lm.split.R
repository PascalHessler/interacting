#'Interrupted regression to assess robustness of GAM-probing of an interaction
#' 
#' After a pattern of interest is documented with GAM probing, one conducts
#' an interrupted regression estimating the effect of the key predictor, x, and 
#' the moderator, z, on y, allowing for different slopes and interactions across two 
#' range of values of the moderator. This approach is a valid robustness check only
#' in situations where we can expect r(x,z)=0, so primarily when either x or z 
#' are randomly assigned.
#'  
#'@param x dichotomous variable indicating randomly assigned condition. 
#'Can be the name of a variable (e.g., x='treatment') when 
#'providing a data argument, or a vector with the x values. 
#'@param z the moderator. Same options as for `x`
#'@param y the dependent variable. Same options as for `x`.
#'@param data an optional data frame containing the focal predictor, moderator and 
#'dependent variable. If `data` is specified, `x`, `z` and `y` must be names of 
#'variables in that dataframe.
#'@param zc cutoff point for the moderator where the two  regression lines will be interrupted.
#'@param quiet set to TRUE to reduce the output shown on console as lm.split() executes.
#'@export

  lm.split = function(x,z,y,zc,data,quiet=FALSE)
    {
    
    
  #VALIIDATE AND PREPARE DATA
    #0 Get var names
      xvar <- clean_string(deparse(substitute(x)))
      zvar <- clean_string(deparse(substitute(z)))
      yvar <- clean_string(deparse(substitute(y)))
      
    #1 Validate data 
      #1.1  Data.frame check
          data_name = ''
          if (!missing(data) && class(data)!='data.frame') {
            data_name = clean_string(deparse(substitute(data)))
            exit("lm.split() says: the argument data must be a data.frame, but '",data_name,"' is instead defined as '",class(data),"'")
           }
    
      
      #1.2 has all the vars 
          if (!missing(data))
          {
            
            n1=names(data)
            if (!xvar %in% n1) exit("lm.split() says the focal variable x ('",xvar,    "') is not in the dataset '",data_name,"'.")
            if (!zvar %in% n1) exit("lm.split() says the moderator variable z ('",zvar,"') is not in the dataset '",data_name,"'.")
            if (!yvar %in% n1) exit("lm.split() says the dependent variable y ('",yvar,"') is not in the dataset '",data_name,"'.")
          }
      
      #2 Validate x,z,y if data not provided
         if (missing(x))  exit("lm.split() says the argument x was not specified")
         if (missing(z))  exit("lm.split() says the argument z was not specified")
         if (missing(y))  exit("lm.split() says the argument y was not specified")

      #3 Validate zc
        check1(zc,'zc', length.check=1, type.check='numeric')  #see check1.R
    
      
      #4 if data not provided, convert x,z,y to dataframe
          if (missing(data))
          {
            data.text = paste0("data = data.frame(",xvar,",", zvar,",", yvar,")")
            data=eval2(data.text)
          }
    
      
    #-------------------------------------------------------------------------------
       
    #Localize
      x=data[,xvar]  
      z=data[,zvar]  
      y=data[,yvar]  
        
      
    #Check x  has 2 values
      nux=length(unique(x))
      if (nux!=2) exit("lm.split() says: the predict x ('",xvar,"') must have exactly two possible values but it has ",nux)
        
    #Check xz are uncorrelated
      rxz=cor.test(x,z)
      
    #if p<.15 or r>.3 show warning
      if (rxz$p.value<.15 | rxz$estimate>.3 & quiet==FALSE) {
        msg=paste0("Warning. The variables x & z must be uncorrelated for lm.split ",
                    "to be valid. Here '", xvar,"' and '",zvar,"' are correlated ", 
                    "r=",round_smart(rxz$estimate), ", ",cleanp(rxz$p.value),". ",
                    "If '",xvar,"' was not randomly assigned, and the effect",
                    "of '",zvar,"' on y is not linear, the results from the interrupted ",
                    "linear regression would be misleading and/or invalid.")
      message(format_msg(msg,width=70, header='lm.split() says:', pre="| "))
      }
      
  #_--------------------------------------------------------------------
      
    #Generate split
      zH=ifelse(z>=zc,1,0)
      
    #Count observations
      freqs = data.frame(table(x,z<=zc))
      
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
          avgH.p = car::linearHypothesis(lm1, "x + x:zH = 0")$`Pr(>F)`[2]
        
        
    #Interactions
        
         #Low 
          intL   = coe2["x:z", "Estimate"]
          intL.p = coe2["x:z", "Pr(>|t|)"]
          
        #high
          intH   = coe2["x:z", "Estimate"] +  coe2["x:z:zH","Estimate"]
          intH.p = car::linearHypothesis(lm2, "x:z + x:z:zH = 0")$`Pr(>F)`[2]
          
    #Draft message
        #Format vars
          avgL = round_smart(avgL)   #see utils.R #12
          avgH = round_smart(avgH)
          intL = round_smart(intL)
          intH = round_smart(intH)
          
          avgL.p=cleanp(avgL.p)      #see utils.R #11
          avgH.p=cleanp(avgH.p)
          intL.p=cleanp(intL.p)
          intH.p=cleanp(intH.p)

     #Text for each result   
                  
          msg1 = paste0("The estimated average effect of '", xvar, "' on '" , yvar, "', when \n")
          msg2 = paste0("   '",zvar, "' <= ", zc," is ",avgL,", ",avgL.p,"  (sample sizes: n1 = ",freqs[3,3], " n2 = ",freqs[4,3],")\n")
          msg3 = paste0("   '",zvar, "' > ", zc,"  is ",avgH,", ",avgH.p,"  (sample sizes: n3 = ",freqs[1,3], " n4 = ",freqs[2,3],")\n\n")
  
          msg4 = paste0("The estimated interaction effect of '", xvar, "'*'",zvar,"' on '" , yvar, "', when \n")
          msg5 = paste0("   '",zvar, "' <= ", zc," is ",intL,", ",intL.p,"  (sample sizes: n1 = ",freqs[3,3], " n2 = ",freqs[4,3],")\n")
          msg6 = paste0("   '",zvar, "' > ", zc,"  is ",intH,", ",intH.p,"  (sample sizes: n3 = ",freqs[1,3], " n4 = ",freqs[2,3],")\n\n")
          
      #put together
          msg = paste0(msg1,msg2,msg3,msg4,msg5,msg6,collapse='')   
      
      #message it
          if (quiet==FALSE)  message(msg)
          
      #Table of contents for output
          object=c('reg1','reg2','robust reg1','robust reg2','text output','table output','cor(x,z)')
          description=c(paste0("Interrupted linear regression without xz interaction: lm(",yvar,"~",xvar,"*zH)  where zH=1 if ",zvar,">",zc," and 0 otherwise",collapse=''),
                        paste0("Interrupted linear regression _with_  xz interaction: lm(",yvar,"~",xvar,"*",zvar,"*zH)  where zH=1 if ",zvar,">",zc," and 0 otherwise",collapse=''),
                        "Corrects SE for 'reg1' using robust SE, HC3",
                        "Corrects SE for 'reg2' using robust SE, HC3",
                        "Output shown on the console when the lm.split() executes",
                        "Output shown on screen formatted onto a data.frame",
                        paste0("Correlation between x & z (",xvar," , ",zvar,")",collapse=''))
                        
          
      #Return full results
          invisible(list(reg1=lm1,
                         reg2=lm2,
                         'robust reg1'=coe1,
                         'robust reg2'=coe2,
                         'text output'=msg,
                         'table output'= data.frame(zc, avgL, avgH, avgL.p, avgH.p, intL, intH, intL.p,  intH.p),
                         'cor(x,z)'=rxz,
                         table_of_contents_for_this_list=data.frame(object, description)
                         ))
  }

  
  

