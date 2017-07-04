cwlVersion: v1.0
class: Workflow

requirements:
  - class: InlineJavascriptRequirement

inputs:
# TODO: remove this
  srcdir: string

# Workflow input files
  feat_in: File
  label_in: File
  metadata_in: File?

# Filtering
  filt_method: string
  rm_unmapped: boolean
  recomp_prop: boolean
  cutoff: float

# Association
  mult_test: string
  alpha: float
  min_fc: float
  detect_limit: float
  col_scheme: string
  plot_type: string

# Normalization
  norm_method: string
  log_n0: float
  sd_min_quantile: float
  norm_sample: boolean
  norm_global: boolean
  vector_norm: int

# Data splitting
  num_folds_cv: int
  resample_cv: int
  stratify_cv: boolean
  inseparable_cv: string

# Model building
  num_folds_ms: int
  stratify_ms: boolean
  ms_criterion: string
  min_nonzero_coeff: int

# Model prediction

# Model evaluation

# Model interpretation
  heatmap_type: string
  consens_threshold: float
  plot_metadata: boolean

outputs:
  association_plots_out:
    type: File
    outputSource: CHECKASSOC/association_plots_out
  evaluation_plots_out:
    type: File
    outputSource: EVALUATE/evaluation_plots_out
  model_plots_out:
    type: File
    outputSource: INTERPRET/model_plots_out
  normalization_parameters_out:
    type: File
    outputSource: NORMALIZE/normalization_parameters_out

steps:
  VALIDATE:
    run: modules/validate.cwl
    in:
      srcdir: srcdir
      feat_in: feat_in
      label_in: label_in
      metadata_in: metadata_in
    out: [validated_feat, validated_label, validated_metadata]

  FILTER:
    run: modules/filter.cwl
    in:
      srcdir: srcdir
      feat_in: VALIDATE/validated_feat
      filt_method: filt_method
      cutoff: cutoff
      rm_unmapped: rm_unmapped
      recomp_prop: recomp_prop
    out: [filtered_feat]

  CHECKASSOC:
    run: modules/checkassoc.cwl
    in:
      srcdir: srcdir
      feat_in: FILTER/filtered_feat
      label_in: VALIDATE/validated_label
      mult_test: mult_test
      alpha: alpha
      min_fc: min_fc
      detect_limit: detect_limit
      col_scheme: col_scheme
      plot_type: plot_type
    out: [association_plots_out]

  NORMALIZE:
    run: modules/normalize.cwl
    in:
      srcdir: srcdir
      feat_in: FILTER/filtered_feat
      norm_method: norm_method
      log_n0: log_n0
      sd_min_quantile: sd_min_quantile
      norm_sample: norm_sample
      norm_global: norm_global
      vector_norm: vector_norm
    out: [feat_out, normalization_parameters_out]

  SPLIT:
    run: modules/split.cwl
    in:
      srcdir: srcdir
      label_in: VALIDATE/validated_label
      metadata_in: VALIDATE/validated_metadata
      num_folds_cv: num_folds_cv
      resample_cv: resample_cv
      stratify_cv: stratify_cv
      inseparable_cv: inseparable_cv
    out: [train_sets_out, test_sets_out]

  TRAINLASSO:
    run: modules/trainlasso.cwl
    in:
      srcdir: srcdir
      feat_in: NORMALIZE/feat_out
      label_in: VALIDATE/validated_label
      train_sets: SPLIT/train_sets_out
      num_folds_ms: num_folds_ms
      stratify_ms: stratify_ms
      ms_criterion: ms_criterion
      min_nonzero_coeff: min_nonzero_coeff
    out: [trained_model_out]

  APPLYLASSO:
    run: modules/applylasso.cwl
    in:
      srcdir: srcdir
      feat_in: NORMALIZE/feat_out
      label_in: VALIDATE/validated_label
      trained_model: TRAINLASSO/trained_model_out
      test_sets: SPLIT/test_sets_out
    out: [predictions_out]

  EVALUATE:
    run: modules/evaluate.cwl
    in:
      srcdir: srcdir
      label_in: VALIDATE/validated_label
      prediction: APPLYLASSO/predictions_out
    out: [evaluation_plots_out]

  INTERPRET:
    run: modules/interpret.cwl
    in:
      srcdir: srcdir
      feat_in: NORMALIZE/feat_out
      origin_feat: feat_in
      label_in: VALIDATE/validated_label
      metadata_in: VALIDATE/validated_metadata
      trained_model: TRAINLASSO/trained_model_out
      prediction: APPLYLASSO/predictions_out
      col_scheme: col_scheme
      heatmap_type: heatmap_type
      consens_threshold: consens_threshold
    out: [model_plots_out]
