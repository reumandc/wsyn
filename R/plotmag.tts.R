#plotmag.tts will need roxygen2 documentation, not sure whether I can have that when these 
#other functions are defined in the same file

#***plots the magnitude
#exported
plotmag<-function(obj)
{
  UseMethod("plotmag",obj)
}

plotmag.default<-function(obj)
{
  stop("Error in plotmag: plotmag only defined for the tts class and classes inheriting from it")
}

#***DAN: Need to write this. It'll just be some sort of generic plotter, not a specific one for wt,
#wmf, wpmf objects (those classes will inherit from tts and will have their own plotmag methods)
#plotmag.tts<-function(obj)
#{
#  
#}