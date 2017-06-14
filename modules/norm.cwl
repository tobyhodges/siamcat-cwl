cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement



inputs: 
  normalizer:
    type: string
    inputBinding: 
      position: -1
      valueFrom: $(inputs.srcdir)/$(self)
  feat_in: 
    type: File
    inputBinding:
      valueFrom: --feat_in $(self.path) --feat_out norm_$(self.basename)
      shellQuote: false
  norm_param_out:
    type: string
    inputBinding:
      prefix: --param_out
  norm_method:
    type: string
    inputBinding:
      prefix: --method
  log_n0:
    type: float
    inputBinding:
      prefix: --log_n0
      shellQuote: false
  sd_min_quantile:
    type: float
    inputBinding:
      prefix: --sd_min_quantile
      shellQuote: false
  norm_sample:
    type: boolean
    inputBinding:
      prefix: --norm_sample true
      shellQuote: false
  norm_global:
    type: boolean
    inputBinding:
      prefix: --norm_global true
      shellQuote: false
  vector_norm:
    type: float
    inputBinding:
      prefix: --vector_norm
      shellQuote: false

outputs:
  feat_out: 
    type: File
    outputBinding:
      glob: norm_$(inputs.feat_in.basename)
  norm_param:
    type: File
    outputBinding:
      glob: $(inputs.norm_param_out)