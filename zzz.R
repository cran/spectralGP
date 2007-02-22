".First.lib" <-
function (lib, pkg) 
{library.dynam("spectralGP",pkg, lib)
cat("spectralGP is loaded.  WARNING: spectralGP uses objects in the form of environments, thereby passing by reference.  Various functions make changes to the input gp object as a side effect, thereby changing the object in the calling environment, and returning NULL.",fill=TRUE)
}
