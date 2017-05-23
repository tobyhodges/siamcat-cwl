# written by Georg Zeller EMBL Heidelberg 2012-2015
# version 0.1.0

##### auxiliary function to trim whitespace from string
# returns string without leading or trailing whitespace
trim = function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

### label.plot.horizontal() takes as input lists of (significantly) differentially abundant bacterial features and plots their names
### on the left side of a figur, parallel to each associated plot. inner.diff.x, inner.diff.y and outer.diff are numerical values that can be
### used to tweak the position of the text lines relatively to their plot. Specifically, inner.diff.y and inner.diff.x will shift the text 
###  alongside the y-axis. outer.diff on the other hand is used as a multiplication factor which changes the distance between each different 
### feature example combination globally. 



### plot.coords is vector containing values with x-axis coordinates of bars, z and y determine upper and lower boundary of barplot, respectively.

draw.error.bar <- function(plot.coords, z, y){
    g <- (max(plot.coords)-min(plot.coords))/(3*length(plot.coords)) 
    for (i in 1:length(plot.coords)) {
      lines(c(z[i],y[i]),c(plot.coords[i], plot.coords[i]))
      lines(c(z[i],z[i]),c(plot.coords[i]+g, plot.coords[i]-g))
      lines(c(y[i],y[i]),c(plot.coords[i]+g, plot.coords[i]-g))
  }  
}


label.plot.horizontal <- function(x, y, labels = NULL, x.suff, y.suff, inner.diff.x = NULL, inner.diff.y = NULL, outer.diff = NULL){
  stopifnot(length(labels) == dim(c)[1])
  if (!is.null(y) && !is.null(x.suff) && !is.null(y.suff)) {
    for (i in 1:dim(x)[1]) {
      mtext(paste(labels[i], x.suff), 2, line=1, at=i*outer.diff+inner.diff.x, las=1, cex=min(0.7, 1-(nrow(x)/70)))
      mtext(y.suff, 2, line=1, at=i*outer.diff+inner.diff.y, las=1, cex=min(0.7, 1-(nrow(x)/70)))
    }
  } else {
    for (i in 1:dim(x)[1]) {
      mtext(labels[i], 2, line=1, at=i*outer.diff+inner.diff.x, las=1, cex=min(0.7, 1-(nrow(x)/50)))
    }
  }
}


##### function to create different tints of a color based on the color's rgb specifications. Each column specifies one tint.
### Make sure that you specify your rgb values as rgb/255! Also, DO NOT call rgb() on your color vector!
create.tints.rgb <- function(color.rgb, nr.tints, tint.steps = 1/nr.tints) {
  tints <- matrix(rep(0,(3*(nr.tints))),nrow=3, ncol=nr.tints)
  for (i in 1:nr.tints){
    tints[1,i] = color.rgb[1] + (1 - color.rgb[1]) * (tint.steps*i)
    tints[2,i] = color.rgb[2] + (1 - color.rgb[2]) * (tint.steps*i)
    tints[3,i] = color.rgb[3] + (1 - color.rgb[3]) * (tint.steps*i)
  }
  return (tints)
}

##### function to parse the header of a label file
### label.header - string in the format: #<TYPE>:<L1>=<class1>;<L2>=<class2>[;<L3>=<class3>]
###   where <TYPE> is a string specifying the type of label variable such as
###   BINARY (for binary classification), CATEGORICAL (for multi-class classification), or CONTINUOUS (for regression)
###   <L1> is a short numeric label for the first class with description <class1> (similarly for the other classes)
parse.label.header = function(label.header) {
  s = strsplit(label.header, ':')[[1]]
  type = trim(s[1])
  if (substr(type, 1, 1) == '#')
    type = trim(substr(type, 2, nchar(type)))
  class.descr = unlist(strsplit(strsplit(trim(s[2]), ';')[[1]], '='))
  l = class.descr[seq(2,length(class.descr),2)]
  class.descr = as.numeric(class.descr[seq(1,length(class.descr)-1,2)])
  names(class.descr) = l

  label.info = list()
  label.info$type = type
  label.info$class.descr = class.descr
  return(label.info)
}

##### function to parse the header of a model file
### TODO documentation
parse.model.header = function(model.header) {
  s = strsplit(model.header, ':')[[1]]
  type = trim(s[1])
  if (substr(type, 1, 1) == '#')
    type = trim(substr(type, 2, nchar(type)))
  label.header = trim(paste(s[2:length(s)], collapse=':'))
  if (substr(label.header, 1, 1) == '[') {
    stopifnot(substr(label.header, nchar(label.header), nchar(label.header)) == ']')
    label.header = substr(label.header, 2, nchar(label.header)-1)
  }
  p = grep('\\(.*\\)', type)
  properties = NULL
  if (length(p) > 0) {
    stopifnot(length(p) == 1)
    stopifnot(substr(type, nchar(type), nchar(type)) == ')')
    properties = substr(type, p+1, nchar(type)-1)
    type = trim(substr(type, 1, p-1))
  }

  model.info = list()
  model.info$type = type
  model.info$properties = properties
  model.info$label.header = label.header
  return(model.info)
}


### TODO docu!
plot.data.range = function(x, y=NULL, x.col='black', y.col='black', labels=NULL, x.suff=NULL, y.suff=NULL) {
  if (is.null(y)) {
    p.m = min(x, na.rm=TRUE)
  } else {
    stopifnot(dim(x)[1] == dim(y)[1])
    p.m = min(c(min(x, na.rm=TRUE), min(y, na.rm=TRUE)))
  }
  plot(rep(p.m, dim(x)[1]), 1:dim(x)[1],
       xlab='', ylab='', yaxs='i', axes=FALSE,
       xlim=c(p.m, 0), ylim=c(0.5, dim(x)[1]+0.5), frame.plot=FALSE, type='n')
  for (v in seq(p.m,-1,1)) {
    abline(v=v, lty=3, col='lightgrey')
  }

  tck = floor(p.m):0
  axis(1, tck, formatC(10^tck, format='E', digits=0), las=1, cex.axis=0.7)

  x.q = apply(x, 1, function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE, names=FALSE))
  if (is.null(y)) {
    # inter-quartile range
    rect(x.q[2,], (1:dim(x)[1])-0.2, x.q[4,], (1:dim(x)[1])+0.2)
    # 90% interval
    segments(x.q[1,], 1:dim(x)[1], x.q[2,], 1:dim(x)[1])
    segments(x.q[4,], 1:dim(x)[1], x.q[5,], 1:dim(x)[1])
    segments(x.q[1,], y0=(1:dim(x)[1])-0.15, y1=(1:dim(x)[1])+0.15)
    segments(x.q[5,], y0=(1:dim(x)[1])-0.15, y1=(1:dim(x)[1])+0.15)
    # median
    segments(x.q[3,], y0=(1:dim(x)[1])-0.2, y1=(1:dim(x)[1])+0.2, lwd=2)
    # scatter plot on top
    for (i in 1:dim(x)[1]) {
      if (nchar(x.col) > 7) {
        # adjust alpha channel by reducing transparency
	a = substr(x.col,nchar(x.col)-1, nchar(x.col))
	a = 1 - (1 - as.numeric(paste('0x', a, sep=''))/255)/2
	x.col = gsub('..$', toupper(as.hexmode(round(a*255))), x.col)
      }
      points(x[i,], rep(i, dim(x)[2])+rnorm(ncol(x),sd=0.05), pch=16, cex=0.6, col=x.col)
    }
  } else {
    y.q = apply(y, 1, function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE, names=FALSE))
    # inter-quartile range
    rect(x.q[2,], 1:dim(x)[1], x.q[4,], (1:dim(x)[1])+0.3, col=x.col)
    rect(y.q[2,], (1:dim(y)[1])-0.3, y.q[4,], 1:dim(y)[1], col=y.col)
    # 90% interval
    segments(x.q[1,], 1:dim(x)[1], x.q[5,], 1:dim(x)[1])#, col=x.col)
    segments(y.q[1,], 1:dim(x)[1], y.q[5,], 1:dim(x)[1])#, col=x.col)
    segments(x.q[1,], y0=1:dim(x)[1], y1=(1:dim(x)[1])+0.2)
    segments(y.q[1,], y0=(1:dim(x)[1])-0.2, y1=1:dim(x)[1])
    segments(x.q[5,], y0=1:dim(x)[1], y1=(1:dim(x)[1])+0.2)
    segments(y.q[5,], y0=(1:dim(x)[1])-0.2, y1=1:dim(x)[1])
    # median
    segments(x.q[3,], y0=1:dim(x)[1], y1=(1:dim(x)[1])+0.3, lwd=3)#, col=x.col)
    segments(y.q[3,], y0=(1:dim(x)[1])-0.3, y1=1:dim(x)[1], lwd=3)#, col=y.col)
    # scatter plot on top
    for (i in 1:dim(x)[1]) {
      if (nchar(x.col) > 7) {
        # adjust alpha channel by reducing transparency
	a = substr(x.col,nchar(x.col)-1, nchar(x.col))
	a = 1 - (1 - as.numeric(paste('0x', a, sep=''))/255)/2
	x.col = gsub('..$', toupper(as.hexmode(round(a*255))), x.col)
      }
      if (nchar(y.col) > 7) {
        # adjust alpha channel by reducing transparency
	a = substr(y.col,nchar(y.col)-1, nchar(y.col))
	a = 1 - (1 - as.numeric(paste('0x', a, sep=''))/255)/2
	y.col = gsub('..$', toupper(as.hexmode(round(a*255))), y.col)
      }
      points(x[i,], rep(i+0.15, ncol(x))+rnorm(ncol(x),sd=0.03), pch=16, cex=0.6, col=x.col)
      points(y[i,], rep(i-0.15, ncol(y))+rnorm(ncol(y),sd=0.03), pch=16, cex=0.6, col=y.col)
    }
  }
}