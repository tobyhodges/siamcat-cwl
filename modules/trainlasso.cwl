cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement

inputs: 
  builder:
    type: string
    inputBinding: 
      valueFrom: $(inputs.srcdir)/$(inputs.LMMETHOD)_$(self)
      position: -1
  srcdir:
    type: string
    inputBinding:
      prefix: --srcdir
  feat_in:
    type: File
    inputBinding:
      prefix: --feat_in
  label_in: 
    type: File
    inputBinding:
      prefix: --label_in
  trained_model:
    type: string
    inputBinding:
      prefix: --model
  train_sets:
    type: File
    inputBinding:
      prefix: --train_sets
  num_folds_ms:
    type: float
    inputBinding:
      prefix: --num_folds
  stratify_ms:
    type: boolean
    inputBinding:
      valueFrom: --stratify $(self)
      shellQuote: false
  ms_criterion:
    type: string
    inputBinding:
      prefix: --sel_criterion
  min_nonzero_coeff:
    type: float
    inputBinding:
      prefix: --min_nonzero_coeff

outputs:
  trained_model_out:
    type: File
    outputBinding:
      glob: $(inputs.trained_model)