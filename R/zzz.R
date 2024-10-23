
    
        
  .onLoad <- function(libname, pkgname) {
  
     
  
    } #End of onLoad


#8. Attaching 
    
    .onAttach <- function(libname, pkgname) {
        
      #While developing:
         packageStartupMessage ("Attached: 'interacting' (version: ",packageVersion('interacting'),  ")\n",
                                "##################################################################\n",
                                "DEVELOPMENT VERSION - DOUBLE CHECK IF USING FOR PUBLISHABLE PAPERS\n",
                              "This Version 2024 10 21 - 09.45AM\n",
                                "***      Subject to breaking changes and possible errors      ***")

  } #End on attach

