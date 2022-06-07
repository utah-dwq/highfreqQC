intGap <- function(x,col,by,maxg){
  byt = paste0(by," mins")
  names(x)[names(x)==col] = "intv"
  y = data.frame(Date = seq(min(x$Date),max(x$Date),by=byt))
  z = merge(x,y,all=TRUE)
  z = z[order(z$Date),]
  z$Parameter = unique(x$Parameter)
  z$units = unique(x$units)
  z$value.int = zoo::na.approx(z$intv,maxgap = maxg)
  z$interpolation_note = ifelse(is.na(z$intv)&!is.na(z$value.int),"Interpolated",NA)
  names(z)[names(z)=="intv"] = col
  return(z)
}