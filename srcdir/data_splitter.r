# written by Georg Zeller EMBL Heidelberg 2012-2015
# version 0.1.0

# parameters that cannot be specified in the interface
r.seed = 2013  # TODO expose

### parse commandline arguments
suppressMessages(library('optparse'))
# define arguments
option_list = list(
  make_option(c('-s', '--srcdir'), type='character', help='Source directory of this and other utility scripts'),
  make_option('--label_in', type='character', help='Input file containing labels'),
  make_option('--train_sets', type='character', help='Output file containing training sets'),
  make_option('--test_sets', type='character', help='Output file containing test sets'),
  make_option('--num_folds', type='integer', default=10, help='Number of cross-validation folds (i.e. subsets, needs to be >= 2)'),
  make_option('--resample', type='integer', default=0, help='Resampling rounds (values <= 1 deactivate resampling)'),
  make_option('--stratify', type='logical', default=TRUE, help='Should cross-validation be stratified such that an approx. equal proportion of positive examples are contained in each subset (only for binary labels)?'),
  make_option('--inseparable', type='character', default='NULL', help=''),
  make_option('--metadata_in', type='character', help='Input file containing metadata (only required if argument \'inseparable\' is specified)')
)
# parse arguments
opt = parse_args(OptionParser(option_list=option_list))
source.dir = opt$srcdir
fn.in.label = opt$label_in
fn.train.folds = opt$train_sets
fn.test.folds = opt$test_sets
num.folds = opt$num_folds
num.resample = opt$resample
stratify = opt$stratify
inseparable = opt$inseparable
fn.in.meta = opt$metadata_in
cat('source.dir =', source.dir, '\n')
cat('fn.in.label =', fn.in.label, '\n')
cat('fn.train.folds =', fn.train.folds, '\n')
cat('fn.test.folds =', fn.test.folds, '\n')
cat('num.folds =', num.folds, '\n')
cat('num.resample =', num.resample, '\n')
cat('stratify =', stratify, '\n')
cat('inseparable =', inseparable, '\n')
cat('fn.in.meta =', fn.in.meta, '\n')
cat('\n')
if (substr(source.dir, nchar(source.dir), nchar(source.dir)) != '/') {
  source.dir = paste(source.dir, '/', sep='')
}
source(paste(source.dir, 'utils.r', sep=''))
source(paste(source.dir, 'cv_utils.r', sep=''))

# optional parameters will be reset to NULL if specified as 'NULL', 'NONE' or 'UNKNOWN'
if (is.null(inseparable) || inseparable=='' || toupper(inseparable)=='NULL' || toupper(inseparable)=='NONE' || toupper(inseparable)=='UNKNOWN') {
  inseparable = NULL
  cat('inseparable not specified\n')
}

start.time = proc.time()[1]
set.seed(r.seed)

### read label and meta-data
# (assuming the label file has 1 column)
label = read.table(file=fn.in.label, sep='\t', header=FALSE, row.names=1, check.names=FALSE, quote='', comment.char='#')
n = rownames(label)
label = as.numeric(label[,1])
names(label) = n
con = file(fn.in.label, 'rt') 
label.header = readLines(con, 1)
close(con)
exm.ids = names(label)
# parse label description
label.info = parse.label.header(label.header)
classes = sort(label.info$class.descr)
# meta-data
if (!is.null(inseparable)) {
  meta.data = read.table(file=fn.in.meta, sep='\t', header=TRUE, row.names=1, check.names=FALSE, quote='', comment.char='#')
  stopifnot(all(names(label) %in% rownames(meta.data)) && all(rownames(meta.data) %in% names(label)))
  m = match(names(label), rownames(meta.data))
  meta.data = meta.data[m,]
  stopifnot(all(names(label) == rownames(meta.data)))
}


### check arguments
if (num.resample < 1) {
  cat('\nresetting num.resample = 1 (', num.resample, ' is an invalid number of resampling rounds)\n', sep='')
  num.resample = 1
}
if (num.folds < 2) {
  cat('\nresetting num.folds = 2 (', num.folds, ' is an invalid number of folds)\n', sep='')
  num.folds = 2
}
if (!is.null(inseparable) && stratify) {
  cat('Stratification is not supported when inseparable is given\n')
  stratify = FALSE
}
if (num.folds >= length(label)) {
  cat('Performing leave-one-out (LOO) cross-validation\n')
  if (stratify) {
    cat('Stratification is not possible with LOO cross-validation\n')
    stratify = FALSE
  }
  num.folds = length(label)
}

### generate files with example partitions, one line per fold
write('#Cross-validation training folds', file=fn.train.folds, append=FALSE)
write('#Cross-validation test folds', file=fn.test.folds, append=FALSE)
print("the write function, writing into empty fn.train.fold and fn.test.folds... ")


# in case of repeated CV, resample subsets several times

### num.resample specifies the amount of times each dataset is
### randomly mixed prior to generation of k-fold CV split.
### effectively, num.resample * num.folds specifies the total amount
### of cross-validation training- and test-sets.
### if(stratify) makes sure that each cv subset (in every iteration
### of resampling) has the same number of positive and negative training
### training examples.

for (r in 1:num.resample) {
  ### assign data to crossvalidation folds
  # reshuffle label (sample shuffles in this case)

  
  label = sample(label)
  foldid = rep(0, length(label))
  
  foldid = assign.fold(label = label, num.folds, stratified = stratify, inseparable = NULL, foldid = foldid)

  names(foldid) = names(label)
  stopifnot(length(label) == length(foldid))
  stopifnot(length(unique(foldid)) == num.folds)

  for (f in 1:num.folds) {
    # make sure each fold contains examples from all classes
    stopifnot(all(sort(unique(label[foldid==f])) == classes))

    # select test examples
    test.idx = which(foldid == f)
    train.idx = which(foldid != f)
    stopifnot(length(intersect(train.idx, test.idx)) == 0)
    
    cat('Fold ', f, ' contains ', sum(foldid==f), ' examples\n', sep='')

    # append training and test examples, one line per fold
    fold.name = paste('>cv_fold', ifelse(num.resample>1, paste(f, '_rep', r, sep=''), as.character(f)), sep='')
    write.table(t(c(fold.name, names(foldid)[train.idx])), file=fn.train.folds, quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, append=TRUE)
    write.table(t(c(fold.name, names(foldid)[test.idx])), file=fn.test.folds, quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, append=TRUE)
  }
}



  if (stratify) {
    cat('Successfully created data split for ', num.folds, '-fold stratified cross-validation', sep='')
  } else {
    cat('Successfully created data split for ', num.folds, '-fold cross-validation', sep='')
  }
if (num.resample > 1) {
  cat(' with ', num.resample, ' times repeated resampling\n', sep='')
} else {
  cat('\n')
}
