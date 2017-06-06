cwlVersion: v1.0
class: Workflow

requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement

inputs:
  validator: string
  filter: string
  assocchecker: string
  normalizer: string
  splitter: string
  builder: string
  predictor: string
  assessor: string
  interpretor: string

  srcdir: string

  feat_in: File
  label_in: File
  metadata_in: File?

  filt_method: string
  rm_unmapped: boolean
  recomp_prop: boolean
  cutoff: float
  
  assoc_plot: string
  mult_test: string
  alpha: float
  min_fc: float
  detect_limit: float
  col_scheme: string
  plot_type: string

  norm_param_out: string
  norm_method: string
  log_n0: float
  sd_min_quantile: float
  norm_sample: boolean
  norm_global: boolean
  vector_norm: float

  train_sets: string
  test_sets: string
  num_folds_cv: float
  resample_cv: float
  stratify_cv: boolean
  inseparable_cv: string

  LMMETHOD: string
  trained_model: string
  num_folds_ms: float
  stratify_ms: boolean
  ms_criterion: string
  min_nonzero_coeff: float

  prediction: string

  eval_plots: string

  heatmap_type: string
  consens_threshold: float
  plot_metadata: boolean

  model_plots: string

outputs:
  assoc_plots_out:
    type: File
    outputSource: CHECKASSOC/assoc_plots
  eval_plots_out:
    type: File
    outputSource: EVAL/eval_plots_out
  model_plots_out:
    type: File
    outputSource: INTERPRET/model_plots_out

steps:
  VALIDATE:
    run: modules/validate.cwl
    in: 
      srcdir: srcdir
      validator: validator
      feat_in: feat_in
      label_in: label_in
      metadata_in: metadata_in
    out: [feat_out, label_out, metadata_out]

  FILTER:
    run: modules/filter.cwl
    in: 
      srcdir: srcdir
      filter: filter
      feat_in: VALIDATE/feat_out
      filt_method: filt_method
      cutoff: cutoff
      rm_unmapped: rm_unmapped
      recomp_prop: recomp_prop
    out: [feat_out]

  CHECKASSOC:
    run: modules/checkassoc.cwl
    in: 
      assocchecker: assocchecker
      srcdir: srcdir
      feat_in: FILTER/feat_out
      label_in: VALIDATE/label_out
      assoc_plot: assoc_plot
      mult_test: mult_test
      alpha: alpha
      min_fc: min_fc
      detect_limit: detect_limit
      col_scheme: col_scheme
      plot_type: plot_type
    out: [assoc_plots]

  NORM:
    run: modules/norm.cwl
    in:
      normalizer: normalizer
      srcdir: srcdir
      feat_in: FILTER/feat_out
      norm_param_out: norm_param_out
      norm_method: norm_method
      log_n0: log_n0
      sd_min_quantile: sd_min_quantile
      norm_sample: norm_sample
      norm_global: norm_global
      vector_norm: vector_norm
    out: [feat_out, norm_param]

  SPLIT:
    run: modules/split.cwl
    in:
      splitter: splitter
      srcdir: srcdir
      label_in: VALIDATE/label_out
      metadata_in: VALIDATE/metadata_out
      train_sets: train_sets
      test_sets: test_sets
      num_folds_cv: num_folds_cv
      resample_cv: resample_cv
      stratify_cv: stratify_cv
      inseparable_cv: inseparable_cv
    out: [train_sets_out, test_sets_out]

  TRAINLASSO:
    run: modules/trainlasso.cwl
    in:
      builder: builder
      LMMETHOD: LMMETHOD
      srcdir: srcdir
      feat_in: NORM/feat_out
      label_in: VALIDATE/label_out
      trained_model: trained_model
      train_sets: SPLIT/train_sets_out
      num_folds_ms: num_folds_ms
      stratify_ms: stratify_ms
      ms_criterion: ms_criterion
      min_nonzero_coeff: min_nonzero_coeff
    out: [trained_model_out]

  APPLYLASSO:
    run: modules/applylasso.cwl
    in:
      predictor: predictor
      LMMETHOD: LMMETHOD
      srcdir: srcdir
      feat_in: NORM/feat_out
      label_in: VALIDATE/label_out
      trained_model: TRAINLASSO/trained_model_out
      test_sets: SPLIT/test_sets_out
      prediction: prediction
    out: [pred_out]

  EVAL:
    run: modules/eval.cwl
    in:
      assessor: assessor
      srcdir: srcdir
      label_in: VALIDATE/label_out
      prediction: APPLYLASSO/pred_out
      eval_plots: eval_plots
    out: [eval_plots_out]

  INTERPRET:
    run: modules/interpret.cwl
    in:
      interpretor: interpretor
      srcdir: srcdir
      feat_in: NORM/feat_out
      origin_feat: feat_in
      label_in: VALIDATE/label_out
      metadata_in: VALIDATE/metadata_out
      trained_model: TRAINLASSO/trained_model_out
      prediction: APPLYLASSO/pred_out
      model_plots: model_plots
      col_scheme: col_scheme
      heatmap_type: heatmap_type
      consens_threshold: consens_threshold
    out: [model_plots_out]