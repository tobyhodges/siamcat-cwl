cwlVersion: v1.0
class: Workflow

requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement

inputs:
  validator: string
  srcdir: string
  feat_in: File
  label_in: File
  metadata_in: File?

outputs:
  vld_feat:
    type: File
    outputSource: validate/feat_out
  vld_label:
    type: File
    outputSource: validate/label_out
  vld_metadata:
    type: File?
    outputSource: validate/metadata_out

steps:
  validate:
    run: validate.cwl
    in: 
      validator: validator
      srcdir: srcdir
      feat_in: feat_in
      label_in: label_in
      metadata_in: metadata_in
    out: [feat_out, label_out, metadata_out]
