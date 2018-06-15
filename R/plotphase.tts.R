#plotphase.tts will need roxygen2 documentation, not sure whether I can have that when these 
#other functions are defined in the same file

#***plots the phase
#exported
plotphase<-function(obj)
{
  UseMethod("plotphase",obj)
}

plotphase.default<-function(obj)
{
  stop("Error in plotphase: plotphase only defined for the tts class and classes inheriting from it")
}

#plotphase.tts<-function(obj)
#{
#Plots the complex phase, somehow, against time and timescale  
#}

