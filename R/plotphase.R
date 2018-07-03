


plotphase<-function(obj)
{
  UseMethod("plotphase",obj)
}

plotphase.default<-function(obj)
{
  stop("Error in plotphase: plotphase only defined for the tts class and classes inheriting from it")
}