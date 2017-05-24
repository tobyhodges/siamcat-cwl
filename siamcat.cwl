cwlVersion: v1.0
class: Workflow

requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement

inputs:
  validator: string
  filter: string
  
  srcdir: string

  feat_in: File
  label_in: File
  metadata_in: File?

  filt_method: string
  rm_unmapped: string
  recomp_prop: string
  cutoff: float

outputs:
  filt_feat:
    type: File
    outputSource: FILTER/feat_out
  vld_label:
    type: File
    outputSource: VALIDATE/label_out
  vld_metadata:
    type: File?
    outputSource: VALIDATE/metadata_out

steps:
  VALIDATE:
    run: validate.cwl
    in: 
      srcdir: srcdir
      validator: validator
      feat_in: feat_in
      label_in: label_in
      metadata_in: metadata_in
    out: [feat_out, label_out, metadata_out]

  FILTER:
    run: filter.cwl
    in: 
      srcdir: srcdir
      filter: filter
      feat_in: VALIDATE/feat_out
      filt_method: filt_method
      cutoff: cutoff
      rm_unmapped: rm_unmapped
      recomp_prop: recomp_prop
    out: [feat_out]
