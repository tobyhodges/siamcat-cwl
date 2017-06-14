# written by Georg Zeller EMBL Heidelberg 2012-2015
# version 0.1.0

### parse commandline arguments
suppressMessages(library('optparse'))
# define arguments
option_list = list(
  make_option('--feat_in', type='character', help='Input file containing features'),
  make_option('--feat_out', type='character', help='Output file to which features after selection are written'),
  make_option('--param_in', type='character', help='Input file from which normalization parameters will be loaded')
)
# parse arguments
opt = parse_args(OptionParser(option_list=option_list))
fn.in.feat = opt$feat_in
fn.out.feat = opt$feat_out
fn.in.param = opt$param_in
cat('fn.in.feat =', fn.in.feat, '\n')
cat('fn.out.feat =', fn.out.feat, '\n')
cat('fn.in.param =', fn.in.param, '\n')
cat('\n')

start.time = proc.time()[1]

### read feature data
feat = read.table(file=fn.in.feat, sep='\t', header=TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE, quote='', comment.char='#')

### read normalization parameters
con = file(fn.in.param, open = "r")
par.lines = NULL
while (length(line <- readLines(con, n=1, warn=FALSE)) > 0) {
  par.lines = c(par.lines, line)
}
close(con)
# parse parameter specification
par = list()
par.idx = which(substr(par.lines, 1, 7) == '###par:')
par.end = c(par.idx[2:length(par.idx)] - 1, length(par.lines))
for (p in 1:length(par.idx)) {
  pn = strsplit(par.lines[par.idx[p]], ':', fixed=TRUE)[[1]][[2]]
  pm = strsplit(par.lines[par.idx[p]], ':', fixed=TRUE)[[1]][[3]]
  if (par.end[p] - par.idx[p] >= 1) {
     if (pm == "character") {
       val = par.lines[(par.idx[p]+1):par.end[p]]
     } else if (pm == "numeric") { 
       val = as.numeric(par.lines[(par.idx[p]+1):par.end[p]])
     } else if (pm == "logical") {
       val = as.logical(par.lines[(par.idx[p]+1):par.end[p]])
     } else {
      stop(paste('unknown class', pc))
    }
    par[[pn]] = val
  } else {
    stop(paste('missing parameter specification for', pn))
  }
  cat('parsed ', pn, ' (of type ', pm, ') from file.\n', sep='')
}
stopifnot(!is.null(par$norm.method))
stopifnot(!is.null(par$retained.feat))


### select features according to those in teh original training data
num.orig.feat = nrow(feat)
m = match(par$retained.feat, rownames(feat))
stopifnot(!any(is.na(m)))
feat = feat[m,]
cat('Retained', nrow(feat), 'features that were used to trained the model (originally', num.orig.feat, 'features were available)\n')
# assert that feature matrix is free of NA values
stopifnot(!any(is.na(feat)))

### apply normalization
if (par$norm.method == 'rank.unit') {
  for (c in 1:ncol(feat)) {
    feat[,c] = rank(feat[,c], ties.method='average')
  }
  for (c in 1:ncol(feat)) {
    feat[,c] = feat[,c] / sqrt(sum(feat[,c]^2))
  }
#  stopifnot(!any(is.na(feat)))
} else if (par$norm.method == 'rank.std') {
  stopifnot(!is.null(par$feat.mean))
  stopifnot(!any(is.na(par$feat.mean)))
  stopifnot(!is.null(par$feat.adj.sd))
  stopifnot(!any(is.na(par$feat.adj.sd)))
  for (c in 1:ncol(feat)) {
    feat[,c] = rank(feat[,c], ties.method='average')
  }
  for (r in 1:nrow(feat)) {
    feat[r,] = (feat[r,] - par$feat.mean[r]) / (par$feat.adj.sd[r])
  }
#  stopifnot(!any(is.na(feat)))
} else if (par$norm.method == 'log.std') {
  stopifnot(!is.null(par$log.n0))
  stopifnot(!is.na(par$log.n0))
  stopifnot(!is.null(par$feat.mean))
  stopifnot(!any(is.na(par$feat.mean)))
  stopifnot(!is.null(par$feat.adj.sd))
  stopifnot(!any(is.na(par$feat.adj.sd)))
  feat = log10(feat + par$log.n0)
  for (r in 1:nrow(feat)) {
    feat[r,] = (feat[r,] - par$feat.mean[r]) / (par$feat.adj.sd[r])
  }
#  stopifnot(!any(is.na(feat)))
} else if (par$norm.method == 'log.unit') {
  stopifnot(!is.null(par$log.n0))
  stopifnot(!is.na(par$log.n0))
  stopifnot(!is.null(par$n.feature))
  stopifnot(!is.null(par$n.sample))
  stopifnot(!is.null(par$n.global))

  cat('Feature sparsity before normalization: ', 100*mean(feat==0), '%\n', sep='')
  feat = log10(feat + par$log.n0)
  if (par$n.feature) {
    stopifnot(!is.null(par$feat.norm.denom))
    stopifnot(!any(is.na(par$feat.norm.denom)) && all(par$feat.norm.denom != 0))
    for (r in 1:nrow(feat)) {
      feat[r,] = feat[r,] / par$feat.norm.denom[r]
    }
  }
  if (par$n.sample) {
    if (par$n.p == 1) {
      for (c in 1:ncol(feat)) {
        feat[,c] = feat[,c] / sum(feat[,c])
      }
    } else if (par$n.p == 2) {
      for (c in 1:ncol(feat)) {
        feat[,c] = feat[,c] / sqrt(sum(feat[,c]^2))
      }
    } else {
      stop('unknown norm!')
    }
  }
  if (!par$n.feature && !par$n.sample && par$n.global) {
    stopifnot(!is.null(par$global.norm.denom))
    stopifnot(!any(is.na(par$global.norm.denom)) && par$global.norm.denom != 0)
    feat = feat / par$global.norm.denom
  }
  cat('Feature sparsity after normalization: ', 100*mean(feat==0), '%\n', sep='')
#  stopifnot(!any(is.na(feat)))
} else {
  cat('\nunrecognized norm.method, exiting!\n')
  quit(save='no', status = 1)
}

### write output
write.table(feat, file=fn.out.feat, quote=FALSE, sep='\t', row.names=TRUE, col.names=NA)

cat('\nSuccessfully applied frozen normalizationto hold-out feature data in ', proc.time()[1] - start.time, ' seconds\n', sep='')
