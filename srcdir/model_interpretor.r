# written by Georg Zeller EMBL Heidelberg 2012-2015
# version 0.1.0

# parameters that cannot be specified in the interface
# consens.thres = 0.5 # Exposed
norm.models = FALSE
# TODO determine these automatically
z.score.lim = c(-3,3)
fc.lim = c(-5,5)
detect.lim = 10^-8


suppressMessages(library('optparse'))
# define arguments
option_list = list(
  make_option(c('-s', '--srcdir'), type='character', help='Source directory of this and other utility scripts'),
  make_option('--label', type='character', help='Input file containing labels'),
  make_option('--feat', type='character', help='Input file containing features'),
  make_option('--origin_feat', type='character', help='Input file containing unnormalized/filtered features'),
  make_option('--meta', type='character', default='NULL', help='Input file containing metadata'),
  make_option('--model', type='character', help='Input file containing the trained classification model(s)'),
  make_option('--pred', type='character', help='Input file containing the trained classification model(s)'),
  make_option('--plot', type='character', help='Output file for plotting'),
  make_option('--col_scheme', type='character', default='RdYlBu', help='Color scheme'),
  make_option('--heatmap_type', type='character', default='zscore', help='which metric should be used to plot feature changes in heatmap? (zscore|fc)'),
  make_option('--consens_thres', type='double', default=0.5, help='specifies the minimal ratio of models incorporating a feature to include it into heatmap')
) 

# parse arguments
opt = parse_args(OptionParser(option_list=option_list))
source.dir = opt$srcdir
fn.label = opt$label
fn.feat = opt$feat
fn.origin.feat = opt$origin_feat
fn.meta = opt$meta
fn.model = opt$model
fn.pred = opt$pred
fn.plot = opt$plot
color.scheme = opt$col_scheme
heatmap.type = opt$heatmap_type
consens.thres = opt$consens_thres
cat('source.dir =', source.dir, '\n')
cat('fn.feat =', fn.feat, '\n')
cat('fn.origin.feat =', fn.origin.feat, '\n')
cat('fn.label =', fn.label, '\n')
cat('fn.model =', fn.model, '\n')
cat('fn.pred =', fn.pred, '\n')
cat('fn.meta =', fn.meta, '\n')
cat('fn.plot =', fn.plot, '\n')
cat('color.scheme =', color.scheme, '\n')
cat('consens.thres =', consens.thres, '\n')
cat('\n')
if (substr(source.dir, nchar(source.dir), nchar(source.dir)) != '/') {
  source.dir = paste(source.dir, '/', sep='')
}
# TODO (remove last check)
# optional parameters will be reset to NULL if specified as 'NULL', 'NONE' or 'UNKNOWN'
if (toupper(fn.meta)=='NULL' || toupper(fn.meta)=='NONE' || toupper(fn.meta)=='UNKNOWN' || fn.meta=='NULL.tsv') {
  fn.meta = NULL
  cat('fn.meta not given: no metadata to display\n')
}
start.time = proc.time()[1]

### imports
suppressMessages(library('colorRamps'))
suppressMessages(library('RColorBrewer'))
source(paste(source.dir, 'utils.r', sep=''))

### read features and labels
# features
feat = as.matrix(read.table(file=fn.feat, sep='\t', header=TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE, quote='', comment.char='#'))
origin.feat = as.matrix(read.table(file=fn.origin.feat, sep='\t', header=TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE, quote='', comment.char='#'))
# labels (assuming the label file has 1 column)
label = read.table(file=fn.label, sep='\t', header=FALSE, row.names=1, check.names=FALSE, quote='', comment.char='#')
n = rownames(label)
stopifnot(n == colnames(feat))
label = as.numeric(label[,1])
names(label) = n
con = file(fn.label, 'rt') 
label.header = readLines(con, 1)
close(con)
# parse label description
label.info = parse.label.header(label.header)
stopifnot(label.info$type == 'BINARY')
PL = max(label.info$class.descr)
NL = min(label.info$class.descr)
p.idx = as.vector(which(label == PL))
n.idx = as.vector(which(label == NL))



### load trained model(s)
model = NULL
model$W = read.table(file=fn.model, sep='\t', header=TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE, quote='')
num.models = dim(model$W)[2]
stopifnot(dim(model$W)[1]-1 == dim(feat)[1])
# parse model header
con = file(fn.model, 'r')
model.header = readLines(con, 1)
close(con)
model.header = parse.model.header(model.header)
# TODO compare label and model headers
#cat(label.header, '\n')
#cat(model.header$label.header, '\n')
#stopifnot(substr(label.header,2,length(label.header)) == model.header$label.header)

### load predictions
# TODO compare prediction and label headers
pred = read.table(file=fn.pred, sep='\t', header=FALSE, row.names=1, check.names=FALSE, quote='')
if (dim(pred)[2] > 1) {
  pred = read.table(file=fn.pred, sep='\t', header=TRUE, row.names=1, check.names=FALSE, quote='')
}
pred = as.matrix(pred)
#cat(dim(pred), '\n')

### make sure that label and prediction are in the same order 
stopifnot(all(names(label) %in% rownames(pred)) && all(rownames(pred) %in% names(label)))
m = match(names(label), rownames(pred))
pred = pred[m,,drop=FALSE]
stopifnot(all(names(label) == rownames(pred)))

### load metadata if provided
meta.data = NULL
if (!is.null(fn.meta)) {
  meta.data = read.table(file=fn.meta, sep='\t', header=TRUE, row.names=1, check.names=FALSE, quote='', comment.char='#')
  stopifnot(all(names(label) %in% rownames(meta.data)) && all(rownames(meta.data) %in% names(label)))
  m = match(names(label), rownames(meta.data))
  meta.data = meta.data[m,]
  stopifnot(all(names(label) == rownames(meta.data)))
}

### some color preprocessing
if (color.scheme == 'matlab') {
  color.scheme = matlab.like(100)
} else {
  # TODO check for valid param!
  color.scheme = rev(colorRampPalette(brewer.pal(11, color.scheme))(100))
}
col.p = color.scheme[length(color.scheme)-4]
col.n = color.scheme[1+4]


### preprocess models: discard bias term, (optionally normalize) and handle ones that are completely 0
keep.idx = grep('Bias', rownames(model$W), invert=TRUE)
model$W = model$W[keep.idx,]
sum.w = colSums(abs(model$W))
sum.w[sum.w == 0] = 1
if (norm.models) {
  for (m in 1:dim(model$W)[2]) {
    model$W[,m] = model$W[,m] / sum.w[m]
  }
  sum.w = rep(1, dim(model$W)[2])
}
sel.idx = which(rowSums(model$W != 0) / dim(model$W)[2] >= consens.thres)
sel.W = t(model$W[sel.idx,])

# sort by mean relative model weight
rel.model.weights = t(model$W) / rowSums(abs(t(model$W)))
sel.idx = sel.idx[sort(apply(rel.model.weights[,sel.idx], 2, median), decreasing=TRUE, index.return=TRUE)$ix]
rel.model.weights = rel.model.weights[,sel.idx]
sel.W = t(model$W[sel.idx,])
num.sel.f = length(sel.idx)

cat('Generating plot for a model with', num.sel.f, 'selected features\n')

### aggregate predictions of several models if more than one is given
agg.pred = matrix(data=NA, nrow=num.models, ncol=length(label))
for (s in 1:length(label)) {
  subj = names(label)[s]
  idx = which(rownames(pred) == subj)
  agg.pred[,s] = pred[idx,]
}
mean.agg.pred = agg.pred
if (dim(agg.pred)[1] > 1) {
  mean.agg.pred = colMeans(agg.pred)
}
#cat(mean.agg.pred, '\n')

### idx to sort samples according to their class membership and prediction score
srt.idx = sort(label+mean.agg.pred, index.return=TRUE)$ix


### start plotting model properties
pdf(fn.plot, paper='special', height=8.27, width=11.69) # format: A4 landscape

### plot layout
sel.f.cex = max(0.3, 0.8 - 0.01*num.sel.f)
lmat = rbind(c(8, 1, 5, 2), c(3, 4,0, 6), c(0, 7, 0, 0))
h_t = 0.10
h_m = ifelse(is.null(meta.data), 0.8, max(0.5, 0.7-0.01*dim(meta.data)[2]))
h_b = ifelse(is.null(meta.data), 0.1, 0.1+0.02*dim(meta.data)[2])
cat('Layout height values: ', h_t, ', ', h_m, ', ', h_b, '\n', sep='')
layout(lmat, widths=c(0.14, 0.58, 0.1, 0.14), heights=c(h_t, h_m, h_b))
par(oma=c(3, 4, 3, 4))

### header row

# field 1 will be plotted later together with feature heatmap

# field 2: header for feature heatmap
par(mar=c(0, 4.1, 3.1, 5.1))
hm.label = label[srt.idx]
plot(NULL, type='n', xlim=c(0,length(hm.label)), xaxs='i', xaxt='n', 
     ylim=c(-0.5,0.5), yaxs='i', yaxt='n', xlab='', ylab='', bty='n')
ul = unique(hm.label)
for (l in 1:length(ul)) {
  idx = which(ul[l] == hm.label)
  lines(c(idx[1]-0.8, idx[length(idx)]-0.2), c(0, 0))
  lines(c(idx[1]-0.8, idx[1]-0.8), c(-0.2, 0))
  lines(c(idx[length(idx)]-0.2, idx[length(idx)]-0.2), c(-0.2, 0))
  h = (idx[1] + idx[length(idx)]) / 2
  t = gsub('_', ' ', names(label.info$class.descr)[label.info$class.descr==ul[l]])
  t = paste(t, ' (n=', length(idx), ')', sep='')
  mtext(t, side=3, line=-0.5, at=h, cex=0.7, adj=0.5)
}
mtext('Metagenomic features', side=3, line=2, at=length(hm.label)/2, cex=1, adj=0.5)

# field 3: model header
par(mar=c(0, 6.1, 3.1, 1.1))
plot(NULL, type='n', xlim=c(-0.1,0.1), xaxt='n', xlab='',
     ylim=c(-0.1,0.1), yaxt='n', ylab='', bty='n')
mtext('Linear model', side=3, line=2, at=0.04, cex=1, adj=0.5)
mtext(paste('(|W| = ', num.sel.f, ')', sep=''), side=3, line=1, at=0.04, cex=0.7, adj=0.5)


### first data display row

  
med = apply(rel.model.weights, 2, median)
low.qt = apply(rel.model.weights, 2, quantile)[2,]
upp.qt = apply(rel.model.weights, 2, quantile)[4,]
# field 4: barplot of effect size associated with each feature
par(mar=c(0.1, 1.1, 0, 1.1))
mi = min(-med-(abs(low.qt-upp.qt)))
mx = max(-med+(abs(low.qt-upp.qt)))
barplot(-med, horiz = TRUE, width=1, space=0, yaxs='i',
                xlim=c(-max(abs(mi),abs(mx)), max(abs(mi), max(mx))), ylim=c(0, num.sel.f), xlab='', ylab='', yaxt='n')
# to change the background color of the plot, invoking barplot twice seems easiest
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='gray90', border=NA)
plot.coords <- barplot(-med, col='gray30', border='white', horiz=TRUE, width=1, space=0, yaxs='i', xlab='', ylab='', yaxt='n', add=TRUE)
draw.error.bar(plot.coords = plot.coords, -low.qt, -upp.qt)
box(lwd=1)
mtext('median relative feat. weight', side=1, line=2, at=0, cex=0.7, adj=0.5)
# robustness indicated as percentage of models including a given feature (to the right of the barplot)
for (f in 1:num.sel.f) {
  t = paste(format(100*rowSums(model$W[sel.idx[f],] != 0) / num.models, digits=1, scientific=FALSE), '%', sep='')
  mtext(t, side=4, line=2.5, at=(f-0.5), cex=sel.f.cex, las=2, adj=1)
  if (f == floor(num.sel.f/2)){
    mtext(gsub('_', ' ', names(label.info$class.descr)[label.info$class.descr==ul[1]]),
          side = 2,
          at = f,
          line = -2
    )
    mtext(gsub('_', ' ', names(label.info$class.descr)[label.info$class.descr==ul[2]]),
          side = 4,
          at = f,
          line = -2
    )
  }
}
mtext('effect size', side=3, line=1, at=(mx/2), cex=0.7, adj=1)
mtext('robustness', side=3, line=1, at=mx, cex=0.7, adj=0)

cat('  finished plotting feature weights.\n')


# field 5: feature heatmap with feature names to the right

par(mar=c(0.1, 4.1, 0, 5.1))
if (heatmap.type == 'zscore'){
  # data is  transposed and transformed to feature z-scores for display
  img.data = t(feat[sel.idx, srt.idx])
  m = apply(img.data, 2, mean)
  s = apply(img.data, 2, sd)
  for (c in 1:dim(img.data)[2]) {
    img.data[,c] = (img.data[,c] - m[c]) / s[c]
  }
  zlim = z.score.lim
} else if (heatmap.type == 'fc') {
  # extract only those features which are specified by sel.idx. Necessary since
  # the indices specified in sel.idx were generated on feat (and not on origin.feat)
  m = match(rownames(feat)[sel.idx], rownames(origin.feat))
  origin.feat.sel = origin.feat[m,]

  feat.ct.median = apply(origin.feat.sel[,label==NL], 1, median)
  img.data = log10(origin.feat.sel + detect.lim) - log10(feat.ct.median + detect.lim)
  # reorder columns
  img.data = img.data[,srt.idx]
  if (any(is.na(m))) {
    # this can be the case if meta-variables have been added as predictors 
    # (then there's no corresponding original feature)
    idx = which(is.na(m))
    img.data[idx,] = feat[sel.idx[idx],]
    rownames(img.data)[idx] = rownames(feat)[sel.idx[idx]]
  }
  # transpose for heatmap plot
  img.data = t(img.data)
  img.data = img.data
  zlim = fc.lim
} else {
  stop('unknown heatmap.type: ', heatmap.type)
}
# truncate extreme values for heatmap visualization
img.data[img.data < zlim[1]] = zlim[1]
img.data[img.data > zlim[2]] = zlim[2]
image(img.data, zlim=zlim, col=color.scheme, xaxt='n', yaxt='n', xlab='', ylab='', bty='n')
for (f in 1:num.sel.f) {
  mtext(colnames(sel.W)[f], side=4, line=1, at=(f-1)/(num.sel.f-1), cex=sel.f.cex, las=2,
        col=ifelse(med[f]>0, col.n, col.p))
}
box(lwd=1)
cat('  finished plotting feature heatmap.\n')

# additonally add to header row a corresponding color bar
par(mar=c(3.1, 1.1, 1.1, 1.1))
# TODO would be nice to overplot a histogram
#h = as.vector(img.data)
#h = h[h > z.score.lim[1] & h < z.score.lim[2]]
#h = hist(h, 100, plot=FALSE)$counts
# end TODO
barplot(as.matrix(rep(1,100)), col = color.scheme, horiz=TRUE, border=0, ylab='', axes=FALSE)
if (heatmap.type == 'fc') {
  key.ticks = seq(round(min(img.data), digits = 1), round(max(img.data), digits = 1), length.out=7)
  key.label = 'Feature fold change over controls'
} else if (heatmap.type == 'zscore') {
  key.ticks = seq(z.score.lim[1], z.score.lim[2], length.out=7)
  key.label = 'Feature z-score'
}
axis(side=1, at=seq(0, 100, length.out=7), labels=key.ticks)
mtext(key.label, side=3, line=0.5, at=50, cex=0.7, adj=0.5)



# field 6: boxplot displaying the poportion of weight per model that is actually shown
par(mar=c(0.1, 6.1, 0, 1.1))
boxplot(rowSums(abs(sel.W)) / sum.w, ylim=c(0,1))
mtext('proportion of', side=1, line=1, at=1, adj=0.5, cex=0.7)
mtext('weight shown', side=1, line=2, at=1, adj=0.5, cex=0.7)
cat('  finished plotting proportion of model weight shown.\n')


# empty field (left)

# field 7 (middle): heatmap showing predictions and metadata (if given)
par(mar=c(1.1, 4.1, 0.3, 5.1))
img.data = as.matrix(mean.agg.pred[srt.idx])
colnames(img.data) = 'Classification result'
if (!is.null(meta.data)) {
  img.data = cbind(meta.data[srt.idx, dim(meta.data)[2]:1], img.data)
}
### transform any categorial column into a numeric one
for (m in 1:dim(img.data)[2]) {
  img.data[,m] = (img.data[,m] - min(img.data[,m], na.rm=TRUE))
  if (max(img.data[,m], na.rm=TRUE) != 0) {
    img.data[,m] = img.data[,m] / max(img.data[,m], na.rm=TRUE)
  }
}

grays = rev(gray(seq(0, 1, length.out=length(color.scheme))))
image(as.matrix(img.data), col=grays, xaxt='n', yaxt='n', xlab='', ylab='', bty='n')
box(lwd=1)

meta.data.cex = max(0.3, 0.7 - 0.01*dim(img.data)[2])
for (m in 1:dim(img.data)[2]) {
  t = colnames(img.data)[m]
  t = gsub('\\.|_', ' ', t)
  mtext(t, side=4, line=1, at=(m-1)/(dim(img.data)[2]-1), cex=meta.data.cex, las=2)
}
# mark missing values
for (m in 1:dim(img.data)[2]) {
  idx = which(is.na(img.data[,m]))
  for (i in idx) {
    x = (i-1) / (dim(img.data)[1]-1)
    y = (m-1) / (dim(img.data)[2]-1)
    text(x, y, 'NA', col='red', cex=0.4)
  }
}
cat('  finished plotting classification result and additional metadata.\n')

# empty field (right)

# field 8  : header for  feature weight barplot
par(mar=c(0, 1.1, 3.1, 1.1))
plot(NULL, type='n', xlim=c(-0.1,0.1), xaxt='n', xlab='',
     ylim=c(-0.1,0.1), yaxt='n', ylab='', bty='n')
mtext('Feature Weights', side=3, line=2, at=0.04, cex=1, adj=0.5)

tmp = dev.off()

cat('\nSuccessfully interpreted model in ', proc.time()[1] - start.time,
    ' seconds\n', sep='')