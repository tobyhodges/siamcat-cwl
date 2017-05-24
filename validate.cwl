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
      prefix: --scrdir=
      position: 2
      separate: false
  feat_in: 
    type: File
    inputBinding:
      prefix: --feat_in=
      position: 3
      separate: false
  label_in:
    type: File
    inputBinding:
      prefix: --label_in=
      position: 4
      separate: false
  metadata_in:
    type: File?
    inputBinding:
      prefix: --metadata_in=
      position: 5
      separate: false
  feat_out:
    type: string
    inputBinding:
      prefix: --feat_out=
      position: 6
      separate: false
  label_out:
    type: string
    inputBinding:
      prefix: --label_out=
      position: 7
      separate: false
  metadata_out:
    type: string?
    inputBinding:
      prefix: --metadata_out=
      position: 8
      separate: false
outputs:
  feat_out_file: 
    type: File
    outputBinding:
      glob: vld_$(inputs.metadata_in)
