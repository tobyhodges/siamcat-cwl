cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement

inputs: 
  interpretor:
    type: string
    inputBinding: 
      valueFrom: $(inputs.srcdir)/$(self)
      position: -1
  srcdir:
    type: string
    inputBinding:
      prefix: --srcdir
  feat_in: 
    type: File
    inputBinding:
      prefix: --feat
  origin_feat:
    type: File
    inputBinding:
      prefix: --origin_feat
  label_in: 
    type: File
    inputBinding:
      prefix: --label
  metadata_in: 
    type: File?
    inputBinding:
      prefix: --meta
  trained_model:
    type: File
    inputBinding: 
      prefix: --model    
  prediction:
    type: File
    inputBinding:
      prefix: --pred
  model_plots:
    type: string
    inputBinding:
      prefix: --plot
  col_scheme:
    type: string
    inputBinding:
      prefix: --col_scheme
  heatmap_type:
    type: string
    inputBinding:
      prefix: --heatmap_type
  consens_threshold:
    type: float
    inputBinding:
      prefix: --consens_thres

outputs:
  model_plots_out:
    type: File
    outputBinding:
      glob: $(inputs.model_plots)