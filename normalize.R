mmnorm <-function(x,minx,maxx)
{
    z<-((x-minx)/(maxx-minx))
    return(z)
}
