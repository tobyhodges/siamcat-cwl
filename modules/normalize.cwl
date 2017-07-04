cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement

inputs:
  feat_in:
    type: File
    inputBinding:
      prefix: --feat_in
      position: 2
  norm_method:
    type: string
    inputBinding:
      prefix: --method
      position: 2
  log_n0:
    type: float
    inputBinding:
      prefix: --log_n0
      position: 2
  sd_min_quantile:
    type: float
    inputBinding:
      prefix: --sd_min_quantile
      position: 2
  norm_sample:
    type: boolean
    inputBinding:
      prefix: --norm_sample
      position: 2
      valueFrom: $(self.toString())
  norm_global:
    type: boolean
    inputBinding:
      prefix: --norm_global
      position: 2
      valueFrom: $(self.toString())
  vector_norm:
    type: int
    inputBinding:
      prefix: --vector_norm
      position: 2
arguments:
  - position: 0
    valueFrom: $(inputs.srcdir)/generic_normalizer.r
  - position: 2
    prefix:  --feat_out
    valueFrom: norm_$(inputs.feat_in.basename)
  - position: 2
    prefix:  --param_out
    valueFrom: normalization_parameters.txt
outputs:
  feat_out:
    type: File
    outputBinding:
      glob: norm_$(inputs.feat_in.basename)
  normalization_parameters_out:
    type: File
    outputBinding:
      glob: normalization_parameters.txt
