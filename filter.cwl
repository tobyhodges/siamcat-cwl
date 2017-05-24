cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement
inputs: 
  filter:
    type: string
    inputBinding: 
      position: -1
      valueFrom: $(inputs.srcdir)/$(self)
  feat_in: 
    type: File
    inputBinding:
      valueFrom: --feat_in $(self.path) --feat_out filt_$(self.basename)
      shellQuote: false
  filt_method:
    type: string
    inputBinding:
      prefix: --method
      shellQuote: false
  cutoff:
    type: float
    inputBinding:
      prefix: --cutoff
      shellQuote: false
  rm_unmapped:
    type: string
    inputBinding:
      prefix: --rm_unmapped
      shellQuote: false
  recomp_prop:
    type: string
    inputBinding:
      prefix: --recomp_prop
      shellQuote: false

outputs:
  feat_out: 
    type: File
    outputBinding:
      glob: filt_$(inputs.feat_in.basename)