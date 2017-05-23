cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
inputs: 
  validator:
    type: string
    inputBinding: 
      valueFrom: $(inputs.srcdir)/$(self)
      position: 1
  srcdir:
    type: string
    inputBinding:
      position: 2
      separate: false
      prefix: --scrdir=
  feat_in: 
    type: string
    inputBinding:
      valueFrom: --feat_in=$(self)
      position: 3
  label_in:
    type: string
    inputBinding:
      valueFrom: --label_in=$(self) --label_out=vld_$(self)
      position: 4
  metadata_in:
    type: string?
    inputBinding:
      valueFrom: --metadata_in=$(self) --metadata_out=vld_$(self)
      position: 5
outputs:
  feat_out: 
    type: File
    outputBinding:
      glob: vld_$(inputs.metadata_in)
