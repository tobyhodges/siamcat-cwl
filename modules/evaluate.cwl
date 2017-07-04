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
      prefix: --label
      position: 2
  prediction:
    type: File
    inputBinding:
      prefix: --pred
      position: 2

arguments:
  - position: 0
    valueFrom: $(inputs.srcdir)/model_evaler.r
  - position: 2
    prefix: --plot
    valueFrom:   eval_plots.pdf

outputs:
  evaluation_plots_out:
    type: File
    outputBinding:
      glob: eval_plots.pdf
