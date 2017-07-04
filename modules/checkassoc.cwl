cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement

inputs:
  srcdir:
    type: string
    inputBinding:
      position: 2
      prefix: --srcdir
  feat_in:
    type: File
    inputBinding:
      position: 2
      prefix: --feat_in
  label_in:
    type: File
    inputBinding:
      position: 2
      prefix: --label_in
  mult_test:
    type: string
    inputBinding:
      position: 2
      prefix: --mult_test
  alpha:
    type: float
    inputBinding:
      position: 2
      prefix: --alpha
  min_fc:
    type: float
    inputBinding:
      position: 2
      prefix: --min_fc
  detect_limit:
    type: float
    inputBinding:
      position: 2
      prefix: --min_fc
  col_scheme:
    type: string
    inputBinding:
      position: 2
      prefix: --col_scheme
  plot_type:
    type: string
    inputBinding:
      position: 2
      prefix: --plot_type

arguments:
  - position: 0
    valueFrom: $(inputs.srcdir)/association_check.r
  - position: 2
    prefix: --plot
    valueFrom: assoc_plots.pdf

outputs:
  association_plots_out:
    type: File
    outputBinding:
      glob: assoc_plots.pdf
