cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement

inputs: 
  assessor:
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
      prefix: --label
  prediction:
    type: File
    inputBinding:
      prefix: --pred
  eval_plots:
    type: string
    inputBinding:
      prefix: --plot

outputs:
  eval_plots_out:
    type: File
    outputBinding:
      glob: $(inputs.eval_plots)