cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement

inputs: 
  splitter:
    type: string
    inputBinding: 
      valueFrom: $(inputs.srcdir)/$(self)
      position: -1
  srcdir:
    type: string
    inputBinding:
      prefix: --srcdir
  label_in: 
    type: File
    inputBinding:
      prefix: --label_in
  metadata_in:
    type: File?
    inputBinding:
      prefix: --metadata_in
  train_sets:
    type: string
    inputBinding:
      prefix: --train_sets
  test_sets:
    type: string
    inputBinding:
      prefix: --test_sets
  num_folds_cv:
    type: float
    inputBinding:
      prefix: --num_folds
  resample_cv:
    type: float
    inputBinding:
      prefix: --resample
  stratify_cv:
    type: boolean
    inputBinding:
      prefix: --stratify true
      shellQuote: false
  inseparable_cv:
    type: string
    inputBinding:
      prefix: --inseparable

outputs:
  train_sets_out:
    type: File
    outputBinding:
      glob: $(inputs.train_sets)
  test_sets_out:
    type: File
    outputBinding:
      glob: $(inputs.test_sets)