# written by Nicolai Karcher and Georg Zeller EMBL Heidelberg 2012-2016
# version 0.1.0

##### function to partition training set into cross-validation folds for model selection
### Works analogous to the function used in data_splitter.r
assign.fold = function(label, num.folds, stratified, inseparable = NULL, foldid) {
  classes = sort(unique(label))
  # stratify positive examples
  if (stratified) {
    first.c = 1
    for (c in classes) {
      num = sum(label == c)
      #cat('num =', num, '\n')
      r = rep(1:num.folds, length.out=num+num.folds)
      #cat(r, '\n')
      r = r[(which(r==first.c)[1]):length(r)]
      first.c = r[num+1]
      r = r[1:num]
      #cat(r, '\n')
      foldid[label==c] = r
    }
  } else {
      if (!is.null(inseparable)) {
        strata = unique(meta.data[,inseparable])
        sid = sample(rep(1:num.folds, length.out=length(strata)))
        foldid = rep(NA, length(label))
        for (s in 1:length(strata)) {
          idx = which(meta.data[,inseparable] == strata[s])
          foldid[idx] = sid[s]
        }
        stopifnot(all(!is.na(foldid)))
      } else {
        foldid = sample(rep(1:num.folds, length.out=length(label)))
      }
      
  }
  # make sure each fold contains examples from all classes
  for (f in 1:num.folds) {
    stopifnot(all(sort(unique(label[foldid==f])) == classes))
  }
  stopifnot(length(label) == length(foldid))
  return(foldid)
}

