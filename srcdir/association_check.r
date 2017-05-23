# written by Georg Zeller EMBL Heidelberg 2012-2015
# version 0.1.0


### parse commandline arguments
suppressMessages(library('optparse'))
suppressMessages(library('beanplot'))
# define arguments
option_list = list(
  make_option(c('-s', '--srcdir'), type='character', help='Source directory of this and other utility scripts'),
  make_option('--label_in', type='character', help='Input file containing labels'),
  make_option('--feat_in', type='character', help='Input file containing features'),
  make_option('--plot', type='character', help='Output pdf file which will contain resulting plots'),
  make_option('--col_scheme', type='character', default='RdYlBu', help='Color scheme'),
  make_option('--alpha', type='double', default=0.05, help='Significance level: only features with p-values < alpha will be reported'),
  make_option('--min_fc', type='double', default=0, help='Fold-change cutoff: only features with absolute log-10 fold change > min_fc will be reported'),
  make_option('--mult_test', type='character', default='fdr', help='Method to correct for multiple testing (one of \"fdr\", \"holm\", \"bonferroni\", \"BY\", or \"none\")'),
  make_option('--detect_limit', type='double', default=10^-8, help='Lower detection limit for feature values (for log-transform and plots)'),
  make_option('--max_show', type='integer', default=50, help='Maximum number of significant features to be shown in result plots'),
  make_option('--plot_type', type='character', default='bean', help = 'One of \"quantile.box\", \"box\", \"bean\", \"quantile.rect\"')
)
# parse arguments
opt = parse_args(OptionParser(option_list=option_list))
source.dir = opt$srcdir
fn.in.label = opt$label_in
fn.in.feat = opt$feat_in
fn.plot = opt$plot
color.scheme = opt$col_scheme
alpha = opt$alpha
min.fc = opt$min_fc
mult.corr = opt$mult_test
detect.lim = opt$detect_limit
max.show = opt$max_show
plot.type = opt$plot_type
cat('source.dir =', source.dir, '\n')
cat('fn.in.label =', fn.in.label, '\n')
cat('fn.in.feat =', fn.in.feat, '\n')
cat('fn.plot =', fn.plot, '\n')
cat('color.scheme =', color.scheme, '\n')
cat('alpha =', alpha, '\n')
cat('min.fc =', min.fc, '\n')
cat('mult.corr =', mult.corr, '\n')
cat('detect.lim =', detect.lim, '\n')
cat('max.show =', max.show, '\n')
cat('plot.type =', plot.type, '\n')
cat('\n')

### If variable source.dir does not end with "/", append "/" to end of source.dir
if (substr(source.dir, nchar(source.dir), nchar(source.dir)) != '/') {
  source.dir = paste(source.dir, '/', sep='')
}

start.time = proc.time()[1]

### further parameters (not exposed)
sort.by = 'pv' # 'fc' or 'pv' (fold change or p-value respectively)



### imports
suppressMessages(library('colorRamps'))
suppressMessages(library('RColorBrewer'))
source(paste(source.dir, 'utils.r', sep=''))
source(paste(source.dir, 'eval_utils.r', sep=''))


feat = as.matrix(read.table(file=fn.in.feat, sep='\t', header=TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE, quote='', comment.char='#'))
# labels (assuming the label file has 1 column)
### NOTE: The following command will only work if you execute data_validator.r prior to association_check.r (maybe fix?)
label = read.table(file=fn.in.label, sep='\t', header=FALSE, row.names=1, check.names=FALSE, quote='', comment.char='#')
n = rownames(label)
stopifnot(n == colnames(feat))
label = as.numeric(label[,1])
names(label) = n
con = file(fn.in.label, 'rt')
label.header = readLines(con, 1)
close(con)
# parse label description
label.info = parse.label.header(label.header)
stopifnot(label.info$type == 'BINARY')
PL = max(label.info$class.descr)
NL = min(label.info$class.descr)

### some color pre-processing
if (color.scheme == 'matlab') {
  color.scheme = matlab.like(100)
} else {
  # TODO check for valid param!
  color.scheme = rev(colorRampPalette(brewer.pal(11, color.scheme))(100))
}
col.p = color.scheme[length(color.scheme)-4]
col.n = color.scheme[1+4]

### Define set of vectors that have the indeces and "description" of all positively and negatively labeled training examples.

n.idx = label==NL
n.lab = gsub('[_.-]', ' ', names(label.info$class.descr)[label.info$class.descr==NL])
p.idx = label==PL
p.lab = gsub('[_.-]', ' ', names(label.info$class.descr)[label.info$class.descr==PL])


p.val = vector('numeric', nrow(feat))
fc = vector('numeric', nrow(feat))

### Loop over all features and calculate (feature-wise) the fold change by taking the median of log(10)-transformed values
### for positively and negatively associated feature values; take the difference of both medians.

for (i in 1:nrow(feat)) {
  fc[i] = median(log10(feat[i,p.idx] + detect.lim)) - median(log10(feat[i,n.idx] + detect.lim))
  p.val[i] = wilcox.test(feat[i,n.idx], feat[i,p.idx], exact = FALSE)$p.value
}

### Apply multihypothesis p-value change

if (mult.corr == 'none') {
  p.adj = p.val
} else if (tolower(mult.corr) == 'bonferroni') {
  p.adj = p.adjust(p.val, method='bonferroni')
} else if (tolower(mult.corr) == 'holm') {
  p.adj = p.adjust(p.val, method='holm')
} else if (tolower(mult.corr) == 'fdr' ) {
  p.adj = p.adjust(p.val, method='fdr')
} else if (tolower(mult.corr) == 'bhy') {
  p.adj = p.adjust(p.val, method='BY')
} else {
  stop('Unknown multiple testing correction method:', mult.corr)
}

cat('Found', sum(p.adj < alpha, na.rm=TRUE), 'significant associations at a significance level <', alpha, '\n')
### idx contains all those indexes of features which were found to be significantly changed in positive training examples compared to negative training examples.
idx = which(p.adj < alpha)
if (min.fc > 0) {
  idx = which(p.adj < alpha & abs(fc) > min.fc)
  cat('Found', length(idx), 'significant associations with absolute log10 fold change >', min.fc, '\n')
}



if (length(idx) > 0) {
  if (sort.by == 'fc') {
    idx = idx[order(fc[idx], decreasing=FALSE)]
  } else if (sort.by == 'pv') {
    idx = idx[order(p.adj[idx], decreasing=TRUE)]
  } else {
    cat('Unknown sorting option:', sort.by, 'order by p-value...\n')
    idx = idx[order(p.adj[idx], decreasing=TRUE)]
  }
  for (i in idx) {
    cat(sprintf('%-40s', rownames(feat)[i]), 'p-value:', format(p.adj[i], digits=4), '\n')
  }
  # truncated the list for the following plots
  if (length(idx) > max.show) {
    idx = idx[(length(idx)-max.show+1):length(idx)]
    cat('Truncating the list of significant associations to the top', max.show, '\n')
  }
  
  
  # compute single-feature AUCs
  cat('\nCalculating the area under the ROC curve for each significantly associated feature\n')
  aucs = vector('numeric', nrow(feat))
  for (i in idx) {
    f = feat[i,]
    ev = eval.classifier(f, label)
    aucs[i] = calc.auroc(ev)
    if (aucs[i] < 0.5) {
      aucs[i] = 1-aucs[i]
    }
  }
  for (i in idx) {
    cat(sprintf('%-40s', rownames(feat)[i]), aucs[i], '\n')
  }
  
  ### generate plots with significant associations between features and labels
  pdf(fn.plot, paper='special', height=8.27, width=11.69) # format: A4 landscape
  
  lmat = cbind(1,2,3,4)
  layout(lmat, widths=c(0.6,0.075,0.2,0.2))
  
  x = log10(as.matrix(feat[idx, p.idx, drop=FALSE]) + detect.lim)
  y = log10(as.matrix(feat[idx, n.idx, drop=FALSE]) + detect.lim)
  
  col = c(paste(col.n, '77', sep=''), paste(col.p, '77', sep=''), 'gray')
  if (plot.type == 'box') {
    par(mar=c(5.1, 25.1, 4.1, 0))
    box.colors <- rep(c(col[1],col[2]),nrow(x))
    plot.data <- data.frame()
    for (i in 1:nrow(x)){
      temp <- as.data.frame(rbind(cbind(x[i,],rep(paste(n.lab, rownames(x)[i]), length(x[i,]))), cbind(y[i,], rep(paste(p.lab, rownames(x)[i]), length((y[i,]))))))
      temp[,1] <- as.numeric(as.character(temp[,1]))
      plot.data <- rbind(plot.data, temp)
      if (i == nrow(x)) {
        plot(NULL, xlab='', ylab='',xaxs='i', yaxs='i', axes=FALSE,
             xlim=c(min(plot.data[,1]-0.2), max(plot.data[,1]) + 1), ylim=c(+0.5, length(idx)*2+0.5), type='n')
        boxplot(plot.data[,1] ~ plot.data[,ncol(plot.data)],horizontal=TRUE,
                names = c(""), show.names = FALSE, col = box.colors, axes = FALSE, outcol = c(col[1], col[2]), add = TRUE)
        mn = as.integer(c(min(plot.data[,1])))
        mx = as.integer(c(max(plot.data[,1])))
        ticks = mn:mx
        for (v in ticks) {
          abline(v=v, lty=3, col='lightgrey')
        }
        tick.labels = formatC(10^ticks, format='E', digits=0)
        axis(side=1, at=ticks, labels=tick.labels, cex.axis=0.7)
        ### function label.plot.horizontal has been written in utils.r.
        label.plot.horizontal(x, y, rownames(feat)[idx], x.suff=paste(' (', p.lab, ')', sep=''),
                              y.suff=paste(' (', n.lab, ')', sep=''), outer.diff = 2, inner.diff.x = 0, inner.diff.y = -1)
      }
    }
  }
  else if (plot.type == "quantile.box"){
    par(mar=c(5.1, 25.1, 4.1, 0))
    plot.data.range(x, y, rownames(feat)[idx], x.col=col[2], y.col=col[1])
    label.plot.horizontal(x, y, rownames(feat)[idx], x.suff=paste(' (', p.lab, ')', sep=''),
                          y.suff=paste(' (', n.lab, ')', sep=''), outer.diff = 1, inner.diff.x = 0.15, inner.diff.y = -0.15)
  }
  
  else if (plot.type == "quantile.rect"){
    par(mar=c(5.1, 25.1, 4.1, 0))
    quantiles.vector <- c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.9)
    x.q = apply(x, 1, function (x) quantile(x, quantiles.vector, na.rm=TRUE, names=FALSE))
    x.medians = apply(x,1,function (x) median(x))
    y.q = apply(y, 1, function (y) quantile(y, quantiles.vector, na.rm=TRUE, names=FALSE))
    y.medians = apply(y,1,function (y) median(y))
    
    p.m = min(c(min(x, na.rm=TRUE), min(y, na.rm=TRUE)))
    plot(rep(p.m, dim(x)[1]), 1:dim(x)[1],
        xlab='', ylab='', yaxs='i', axes=FALSE,
        xlim=c(min(x,y), max(x,y+2)), ylim=c(0, dim(x)[1]), frame.plot=FALSE, type='n')
    for (v in seq(p.m,0,1)) {
      abline(v=v, lty=3, col='lightgrey')
    }
    
    tck = floor(p.m):0
    axis(1, tck, formatC(10^tck, format='E', digits=0), las=1, cex.axis=0.7)
    for (i in 1:(nrow(x.q)/2)){
      if (i == 1) {
        rect(x.q[i,], 0.5:dim(x)[1], x.q[nrow(x.q)+1-i,], (0.5:dim(x)[1])+0.3, col = c("white"), border = c("black"), lwd = 0.9)
        rect(y.q[i,], 0.5:dim(y)[1], y.q[nrow(y.q)+1-i,], (0.5:dim(y)[1])-0.3, col = c("white"), border = c("black"), lwd = 0.9)
        
      }
      else {
        rect(x.q[i,], 0.5:dim(x)[1], x.q[nrow(x.q)+1-i,], (0.5:dim(x)[1])+0.3, col = col[2], border = c("black"), lwd = 0.9)
        rect(y.q[i,], 0.5:dim(y)[1], y.q[nrow(y.q)+1-i,], (0.5:dim(y)[1])-0.3, col = col[1], border = c("black"), lwd = 0.9)
      }
    }
    points(x.medians, y=(0.5:dim(x)[1])+0.15, pch=18, cex = min(35/nrow(x),4))
    points(y.medians, y=(0.5:dim(y)[1])-0.15, pch=18, cex = min(35/nrow(x),4))
    mtext('Quantiles', 3, line=0, at=1, adj = 1.675, padj = 0.45, las=1, cex=0.7)
    ### create.tints.rgb is in utils.r
    red.tints <- create.tints.rgb(col2rgb(col[2])/255, nr.tints=5, tint.steps = 1/5)
    blue.tints <- create.tints.rgb(col2rgb(col[1])/255, nr.tints=5, tint.steps = 1/5)
    legend(-1.75, nrow(x), legend = c("80%","60%","40%","20%","median","","","","",""),
           bty='n', cex=1, fill=c(
             rgb(matrix(red.tints[,5], ncol = 3)),
             rgb(matrix(red.tints[,3], ncol = 3)),
             rgb(matrix(red.tints[,2], ncol = 3)),
             rgb(matrix(red.tints[,1], ncol = 3)),
             0,
             rgb(matrix(blue.tints[,5], ncol = 3)),
             rgb(matrix(blue.tints[,3], ncol = 3)),
             rgb(matrix(blue.tints[,2], ncol = 3)),
             rgb(matrix(blue.tints[,1], ncol = 3)),
             0),
           lty = c(0,0,0,0,0,0,0,0,0,0),
           lwd = c(1.3,1.3,1.3,1.3,2,1.3,1.3,1.3,1.3,1.3), ncol = 2,
           border = c("black", "black","black","black","white","black","black","black","black","white"))
    legend(-1.675, nrow(x), legend = c("","","","",""),
           bty='n', lty = c(0,0,0,0,0),
           # cap legend size for diamond (should look symmetric to other symbols)
           pch = 18, cex = 1, pt.cex = c(0,0,0,0, min(35/nrow(x), 2.25)))
    
    label.plot.horizontal(x, y, rownames(feat)[idx], x.suff=paste(' (', p.lab, ')', sep=''),
                          y.suff=paste(' (', n.lab, ')', sep=''), outer.diff = 1, inner.diff.x = -0.3, inner.diff.y = -0.6)
  }
  else if (plot.type == "bean"){
    par(mar=c(5.1, 25.1, 4.1, 0))
    bean.data <- data.frame()
    for (i in 1:nrow(x)){
      temp <- as.data.frame(rbind(cbind(x[i, ], rep(paste(n.lab, rownames(x)[i]), length(x[i, ]))),
                                  cbind(y[i, ], rep(paste(p.lab, rownames(x)[i]), length((y[i, ]))))))
      temp[,1] <- as.numeric(as.character(temp[,1]))
      bean.data <- rbind(bean.data, temp)
      if (i == nrow(x)){
        plot(NULL, xlab='', ylab='',xaxs='i', yaxs='i', axes=FALSE,
             xlim = c(as.integer(min(x))-1.5,as.integer(max(x))+1), ylim=c(0.45, length(idx)+0.6), type='n')
        beanplot(bean.data[, 1] ~ bean.data[, ncol(bean.data)], side = "both", bw="nrd0", col = list(col[1],
                                                                                                     col[2]), horizontal = TRUE, names = c(""), show.names = FALSE, beanlines = "median", maxstripline = 0.2, what = c(FALSE,TRUE,TRUE,FALSE),
                 axes = FALSE, add = TRUE )
        mn = as.integer(c(min(bean.data[,1])-1.5))
        mx = as.integer(c(max(bean.data[,1])+1))
        ticks = mn:mx
        for (v in ticks) {
          abline(v=v, lty=3, col='lightgrey')
        }
        tick.labels = formatC(10^ticks, format='E', digits=0)
        axis(side=1, at=ticks, labels=tick.labels, cex.axis=0.7)
        label.plot.horizontal(x, y, rownames(feat)[idx], x.suff=paste(' (', p.lab, ')', sep=''),
                              y.suff=paste(' (', n.lab, ')', sep=''), outer.diff = 1, inner.diff.x = 0.15, inner.diff.y = -0.15)
      }
    }
  }
  else {
    print("plot type has not been specified properly; continue with quantileplot")
    plot.type = "quantile.box"
    par(mar=c(5.1, 25.1, 4.1, 0))
    plot.data.range(x, y, rownames(feat)[idx], x.col=col[2], y.col=col[1])
    label.plot.horizontal(x, y, rownames(feat)[idx], x.suff=paste(' (', p.lab, ')', sep=''),
                          y.suff=paste(' (', n.lab, ')', sep=''), outer.diff = 1, inner.diff.x = 0.15, inner.diff.y = -0.15)
  }
  
  p.val.annot = formatC(p.adj[idx], format='E', digits=2)
  if (sum(p.adj < alpha, na.rm=TRUE) <= max.show) {
    title(main='Differentially abundant features', xlab='Abundance (log10-scale)')
  } else {
    title(main=paste('Differentially abundant features\ntruncated to the top', max.show),
          xlab='Abundance (log10-scale)')
  }
  par(mar=c(5.1,0,4.1, 0))
  for (i in 1:length(p.val.annot)) {
    if (plot.type == 'box'){
      mtext(p.val.annot[i], 4, line=2, at=(2*i)-0.5, las=1, cex=min(0.7, 1-(length(idx)/100)))
    }
    else if (plot.type == "quantile.rect"){
      mtext(p.val.annot[i], 4, line=2, at=i-0.5, las=1, cex=min(0.7, 1-(length(idx)/100)))
    }
    else {
      mtext(p.val.annot[i], 4, line=2, at=i, las=1, cex=min(0.7, 1-(length(idx)/100)))
    }
  }
  plot(NULL, xlab='', ylab='',xaxs='i', yaxs='i', axes=FALSE,
       type='n', xlim=c(0,10), ylim=c(0,length(p.val.annot)+0.5))
  title(main='Adj. p-value')
  
  # plot fold changes
  par(mar=c(5.1, 2.1, 4.1, 2.1))
  bcol = ifelse(fc[idx] > 0, col[2], col[1])
  mn = floor(min(fc[idx]))
  mx = ceiling(max(fc[idx]))
  mx = max(abs(mn), abs(mx))
  if (!is.finite(mx)) {
    mx = 10
  }
  mn = -mx
  plot(NULL, xlab='', ylab='', xaxs='i', yaxs='i', axes=FALSE,
       xlim=c(mn, mx), ylim=c(0.2, length(idx)+0.2), type='n')
  barplot(fc[idx], horiz=TRUE, width=0.6, space=2/3, col=bcol, axes=FALSE, add=TRUE)
    
  ticks = mn:mx
  for (v in ticks) {
    abline(v=v, lty=3, col='lightgrey')
  }
  tick.labels = formatC(10^ticks, format='E', digits=0)
  axis(side=1, at=ticks, labels=tick.labels, cex.axis=0.7)
  title(main='Fold change', xlab='FC (log10-scale)')
  
  # plot single-feature AUCs
  par(mar=c(5.1, 1.1, 4.1, 3.1))
  plot(NULL, xlab='', ylab='',xaxs='i', yaxs='i', axes=FALSE,
       xlim=c(0.5,1), ylim=c(0.5, length(idx)+0.5), type='n')
  ticks = seq(0.5, 1.0, length.out=6)
  for (v in ticks) {
    abline(v=v, lty=3, col='lightgrey')
  }
  for (b in 1:length(idx)) {
    i = idx[b]
    points(aucs[i], b, pch=18, col=bcol[b])
    points(aucs[i], b, pch=5, col='black', cex=0.9)
  }
  axis(side=1, at=ticks, cex.axis=0.7)
  title(main='Feature AUCs', xlab='AU-ROC')
  # close pdf device
  tmp = dev.off()
  
}

cat('\nSuccessfully analyzed statistically significant associations between individual features and labels in ', proc.time()[1] - start.time, ' seconds\n', sep='')
