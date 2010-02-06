.onLoad=function(lib,pkg){
  cat("Package spectralGP (1.3) is loaded.\n===============================================================\n WARNING: spectralGP uses objects in the form of environments,\n thereby passing by reference.  Various method functions make\n changes to the input gp object as a side effect, thereby\n changing the object in the calling environment, and\n returning NULL.\n===============================================================",fill=FALSE)
}
