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
      prefix: --feat_in
      position: 2
  label_in:
    type: File
    inputBinding:
      prefix: --label_in
      position: 2
  metadata_in:
    type: File?
    inputBinding:
      position: 2
      prefix: --metadata_in
arguments:
  - position: 0
    valueFrom: $(inputs.srcdir)/data_validator.r
  - prefix: --feat_out
    position: 2
    valueFrom: vld_$(inputs.feat_in.basename)
  - prefix: --label_out
    position: 2
    valueFrom: vld_$(inputs.label_in.basename)
  - valueFrom: |
                ${
                  if (inputs.metadata_in){
                    return [ "--metadata_out", "vld_metadata.tsv" ];
                  } else {
                    return null;
                  }
                }
    position: 2

outputs:
  validated_feat:
    type: File
    outputBinding:
      glob: vld_$(inputs.feat_in.basename)
  validated_label:
    type: File
    outputBinding:
      glob: vld_$(inputs.label_in.basename)
  validated_metadata:
    type: File?
    outputBinding:
      glob: vld_metadata.tsv
