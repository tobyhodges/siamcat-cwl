# written by Georg Zeller EMBL Heidelberg 2012-2015
# version 0.1.0

### parse commandline arguments
suppressMessages(library('optparse'))
# define arguments
option_list = list(
  make_option(c('-s', '--srcdir'), type='character', help='Source directory of this and other utility scripts'),
  make_option('--metadata_in', type='character', help='Input file containing meta-data'),
  make_option('--metadata_out', type='character', help='Output file to which validated meta-data is written'),
  make_option('--label_in', type='character', help='Input file containing labels'),
  make_option('--label_out', type='character', help='Output file to which validated label data is written'),
  make_option('--feat_in', type='character', help='Input file containing features'),
  make_option('--feat_out', type='character', help='Output file to which validated feature data is written')
)
# parse arguments
opt = parse_args(OptionParser(option_list=option_list))
source.dir = opt$srcdir
fn.in.meta = opt$metadata_in
fn.out.meta = opt$metadata_out
fn.in.label = opt$label_in
fn.out.label = opt$label_out
fn.in.feat = opt$feat_in
fn.out.feat = opt$feat_out
cat('source.dir =', source.dir, '\n')
cat('fn.in.meta =', fn.in.meta, '\n')
cat('fn.out.meta =', fn.out.meta, '\n')
cat('fn.in.label =', fn.in.label, '\n')
cat('fn.out.label =', fn.out.label, '\n')
cat('fn.in.feat =', fn.in.feat, '\n')
cat('fn.out.feat =', fn.out.feat, '\n')
cat('\n')
if (substr(source.dir, nchar(source.dir), nchar(source.dir)) != '/') {
  source.dir = paste(source.dir, '/', sep='')
}
# optional parameters will be reset to NULL if specified as 'NULL', 'NONE' or 'UNKNOWN'
if (is.null(fn.in.meta) || toupper(fn.in.meta)=='NULL' || toupper(fn.in.meta)=='NONE' || toupper(fn.in.meta)=='UNKNOWN') {
  fn.in.meta = NULL
  cat('fn.in.meta not specified\n')
}
if (is.null(fn.out.meta) || toupper(fn.out.meta)=='NULL' || toupper(fn.out.meta)=='NONE' || toupper(fn.out.meta)=='UNKNOWN') {
  fn.out.meta = NULL
  cat('fn.out.meta not specified: skipping evaluation\n')
}
if (is.null(fn.in.meta) || is.null(fn.out.meta)) {
  # assert that if one of in/out metadata file is not given the other one is unspecified too
  stopifnot(is.null(fn.in.meta) && is.null(fn.out.meta))
}

start.time = proc.time()[1]


### imports
source(paste(source.dir, 'utils.r', sep=''))


### read label, feature and meta- data
# TODO attempt multiple reading attempts!

# features
feat = read.table(file=fn.in.feat, sep='\t', header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, quote='')
#cat(dim(feat), '\n')
# labels
if (!is.null(fn.out.label)){
  label = read.table(file=fn.in.label, sep='\t', header=TRUE, row.names=NULL, stringsAsFactors = FALSE, check.names=FALSE, quote='')
  if (dim(label)[1] > dim(label)[2]){
    temp = names(label)
    names(label) = NULL
    label = rbind(temp,label)
    rownames(label) = label[,1]
    label[,1] = NULL
    label = t(label)
  }
  #cat(dim(label), '\n')
  stopifnot(names(label) == colnames(feat))
  label = as.numeric(label)
  names(label) = colnames(feat)
  
  # Check general suitablity of supplied dataset
  classes <- unique(label)
  for (i in classes){
    stopifnot(sum(label==i) >= 5)
    if (sum(label==i) < 10){
      cat("Data set has only",sum(label==i), "training examples of class",i," . Note that a dataset this small/skewed is not necessarily suitable for analysis in this pipe line." )
    }
  }
  
  
  
  
  # OLD STYLE with label file having 1 column
  #label = read.table(file=fn.in.label, sep='\t', header=FALSE, row.names=1, check.names=FALSE, quote='')
  #n = rownames(label)
  #label = as.vector(t(label))
  #names(label) = n
  
  #Check label header!
  
  con = file(fn.in.label, 'rt') 
  label.header = readLines(con, 1)
  if (substring(label.header,1,1) != "#"){
    stop("Label header seems to be missing or broken.")
  }
  close(con)
  # meta-data
  # %in% is the R code for set membership ("element of")
} else {
  print ("Not finding label file. Continuing. Pipe line will fail unless you are using the holdout functionality.")
}



if (!is.null(fn.in.meta)) {
  meta.data = read.table(file=fn.in.meta, sep='\t', header=TRUE, row.names=1, check.names=FALSE, quote='')
  stopifnot(all(names(label) %in% rownames(meta.data)) && all(rownames(meta.data) %in% names(label)))
  m = match(names(label), rownames(meta.data))
  meta.data = meta.data[m,]
  stopifnot(all(names(label) == rownames(meta.data)))
}

### run data checks / validation / format conversion
# TODO !!!

### write validated label, feature and meta-data
# labels
if (!is.null(fn.out.label)){
write(label.header, file=fn.out.label, append=FALSE)
write.table(label, file=fn.out.label, quote=FALSE, sep='\t', row.names=TRUE, col.names=FALSE, append=TRUE)
}
# features
write.table(feat, file=fn.out.feat, quote=FALSE, sep='\t', row.names=TRUE, col.names=NA)
# meta-data
if (!is.null(fn.out.meta)) {
  write.table(meta.data, file=fn.out.meta, quote=FALSE, sep='\t', row.names=TRUE, col.names=NA)
}
cat('\nSuccessfully validated data in ', proc.time()[1] - start.time, ' seconds\n', sep='')
