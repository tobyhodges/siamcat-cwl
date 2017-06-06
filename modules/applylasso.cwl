cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement

inputs: 
  predictor:
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
    type: File
    inputBinding:
      prefix: --model
  test_sets:
    type: File
    inputBinding:
      prefix: --test_sets
  prediction:
    type: string
    inputBinding:
      prefix: --pred

outputs:
  pred_out:
    type: File
    outputBinding:
      glob: $(inputs.prediction)