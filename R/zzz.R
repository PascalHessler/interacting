
    
        
  .onLoad <- function(libname, pkgname) {
  
     
  
    } #End of onLoad


#8. Attaching 
    
    .onAttach <- function(libname, pkgname) {
        
      #While developing:
         packageStartupMessage ("Attached: 'interacting' (version: ",packageVersion('interacting'),  ")\n",
                                "##################################################################\n",
                                "DEVELOPMENT VERSION - DOUBLE CHECK IF USING FOR PUBLISHABLE PAPERS\n",
                              "This Version 2025 03 11 - 09.29AM\n",
                                "***      Subject to breaking changes and possible errors      ***")

  } #End on attach

