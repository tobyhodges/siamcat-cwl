cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement

inputs: 
  assocchecker:
    type: string
    inputBinding: 
      position: -1
      valueFrom: $(inputs.srcdir)/$(self)
  srcdir:
    type: string
    inputBinding:
      prefix: --srcdir
      shellQuote: false
  feat_in: 
    type: File
    inputBinding:
      prefix: --feat_in
      shellQuote: false
  label_in: 
    type: File
    inputBinding:
      prefix: --label_in
      shellQuote: false
  assoc_plot:
    type: string
    inputBinding:
      prefix: --plot
  mult_test:
    type: string
    inputBinding:
      prefix: --mult_test
  alpha:
    type: float
    inputBinding:
      prefix: --alpha
      shellQuote: false
  min_fc:
    type: float
    inputBinding:
      prefix: --min_fc
      shellQuote: false
  detect_limit:
    type: float
    inputBinding:
      prefix: --min_fc
      shellQuote: false
  col_scheme:
    type: string
    inputBinding:
      prefix: --col_scheme
  plot_type:
    type: string
    inputBinding:
      prefix: --plot_type

outputs:
  assoc_plots: 
    type: File
    outputBinding:
      glob: $(inputs.assoc_plot)