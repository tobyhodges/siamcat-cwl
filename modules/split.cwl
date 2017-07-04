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
  label_in:
    type: File
    inputBinding:
      prefix: --label_in
      position: 2
  metadata_in:
    type: File?
    inputBinding:
      prefix: --metadata_in
      position: 2
  num_folds_cv:
    type: int
    inputBinding:
      prefix: --num_folds
      position: 2
  resample_cv:
    type: int
    inputBinding:
      prefix: --resample
      position: 2
  stratify_cv:
    type: boolean
    inputBinding:
      position: 2
      prefix: --stratify
      valueFrom: $(self.toString())
  inseparable_cv:
    type: string
    inputBinding:
      prefix: --inseparable
      position: 2

arguments:
    - position: 0
      valueFrom: $(inputs.srcdir)/data_splitter.r
    - position: 2
      prefix: --train_sets
      valueFrom: train_sets.tsv
    - position: 2
      prefix: --test_sets
      valueFrom: test_sets.tsv

outputs:
  train_sets_out:
    type: File
    outputBinding:
      glob: train_sets.tsv
  test_sets_out:
    type: File
    outputBinding:
      glob: test_sets.tsv
