cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
inputs: 
  validator:
    type: string
    inputBinding: 
      valueFrom: $(inputs.sourcedir)/$(self)
      position: 1
  sourcedir:
    type: string
    inputBinding:
      valueFrom: --scrdir=$(self)
      position: 2
      separate: false
  feat_in: 
    type: File
    inputBinding:
      valueFrom: --feat_in="$(self.basename)" --feat_out="vld_$(self.basename)"
      position: 3
  label_in:
    type: File
    inputBinding:
      valueFrom: --label_in="$(self.basename)" --label_out="vld_$(self.basename)"
      position: 4
  metadata_in:
    type: File?
    inputBinding:
      valueFrom: --metadata_in="$(self.basename)" --metadata_out="vld_$(self.basename)"
      position: 5
outputs:
  feat_out: 
    type: File
    outputBinding:
      glob: vld_$(inputs.metadata_in.basename)
