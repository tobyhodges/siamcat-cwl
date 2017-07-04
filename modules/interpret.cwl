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
      prefix: --feat
      position: 2
  origin_feat:
    type: File
    inputBinding:
      prefix: --origin_feat
      position: 2
  label_in:
    type: File
    inputBinding:
      prefix: --label
      position: 2
  metadata_in:
    type: File?
    inputBinding:
      prefix: --meta
      position: 2
  trained_model:
    type: File
    inputBinding:
      prefix: --model
      position: 2
  prediction:
    type: File
    inputBinding:
      prefix: --pred
      position: 2
  col_scheme:
    type: string
    inputBinding:
      prefix: --col_scheme
      position: 2
  heatmap_type:
    type: string
    inputBinding:
      prefix: --heatmap_type
      position: 2
  consens_threshold:
    type: float
    inputBinding:
      prefix: --consens_thres
      position: 2

arguments:
  - position: 0
    valueFrom: $(inputs.srcdir)/model_interpretor.r
  - position: 2
    prefix: --plot
    valueFrom: model_plots.pdf

outputs:
  model_plots_out:
    type: File
    outputBinding:
      glob: model_plots.pdf
