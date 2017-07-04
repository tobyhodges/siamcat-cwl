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
  trained_model:
    type: File
    inputBinding:
      prefix: --model
      position: 2
  test_sets:
    type: File
    inputBinding:
      prefix: --test_sets
      position: 2

arguments:
    - position: 0
      valueFrom: $(inputs.srcdir)/lasso_predictor.r
    - position: 2
      prefix: --pred
      valueFrom: lasso_predictions.tsv

outputs:
  predictions_out:
    type: File
    outputBinding:
      glob: lasso_predictions.tsv
