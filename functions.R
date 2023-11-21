library('groundhog')
pkgs=c('mgcv')
date='2023-08-01'  
groundhog.library(pkgs,date)


x=rnorm(1000)
z=rnorm(1000)
e=rnorm(1000)
y=x*z+e


g=gam(y~s(x)+s(z)+ti(x,z))

f1='s(x)+s(z)'
simple.slopes=function(g)
  {
  #Extract the x and z of the first interaction
    f1=as.character(formula(g))[3]
    reg_ti <- "ti\\(([^,]+),([^)]+)\\)"
    ti_1 <- regmatches(f1, regexpr(reg_ti, f1))[1]  #get the first ti
    
    
    
  #If a ti() is present
    if (is.na(ti_1))
    {
    ti_1 <- trimws(gsub("ti\\(|\\)", "", ti_1))             #drop 'ti(' and ')'
    xz=strsplit(ti_1,",")
    x.text = trimws(xz[[1]][1])
    z.text = trimws(xz[[1]][2])
    }
  
  
  
  
}
  
  
  gsub(t_1)
  strsplit(matches,",")
  

  
# Extract the variable names
  if (length(matches) > 0 && length(matches[[1]]) >= 3) {
      x <- matches[[1]][2]
      z <- matches[[1]][3]
      cat("x =", x, "and z =", z, "\n")
  } else {
      cat("No 'ti()' found or it does not match the expected format.\n")
  }