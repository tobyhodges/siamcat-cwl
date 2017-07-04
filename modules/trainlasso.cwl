cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement

inputs:
  srcdir:
    type: string
    inputBinding:
      prefix: --srcdir
      position: 2
  feat_in:
    type: File
    inputBinding:
      prefix: --feat_in
      position: 2
  label_in:
    type: File
    inputBinding:
      prefix: --label_in
      position: 2
  train_sets:
    type: File
    inputBinding:
      prefix: --train_sets
      position: 2
  num_folds_ms:
    type: int
    inputBinding:
      prefix: --num_folds
      position: 2
  stratify_ms:
    type: boolean
    inputBinding:
      prefix: --stratify
      valueFrom: $(self.toString())
      position: 2
  ms_criterion:
    type: string
    inputBinding:
      prefix: --sel_criterion
      position: 2
  min_nonzero_coeff:
    type: int
    inputBinding:
      prefix: --min_nonzero_coeff
      position: 2

arguments:
    - position: 0
      valueFrom: $(inputs.srcdir)/lasso_trainer.r
    - position: 2
      prefix: --model
      valueFrom: trained_lasso_models.tsv

outputs:
  trained_model_out:
    type: File
    outputBinding:
      glob: trained_lasso_models.tsv
