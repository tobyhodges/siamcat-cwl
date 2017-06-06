cwlVersion: v1.0
class: CommandLineTool
baseCommand: Rscript
requirements:
  - class: InlineJavascriptRequirement
  - class: ShellCommandRequirement
inputs: 
  validator:
    type: string
    inputBinding: 
      valueFrom: $(inputs.srcdir)/$(self)
      position: 1
  srcdir:
    type: string
    inputBinding:
      prefix: --srcdir
      position: 2
      shellQuote: false
  feat_in: 
    type: File
    inputBinding:
      valueFrom: --feat_in $(self.path) --feat_out vld_$(self.basename)
      position: 3
      shellQuote: false
  label_in: 
    type: File
    inputBinding:
      valueFrom: --label_in $(self.path) --label_out vld_$(self.basename)
      position: 4
      shellQuote: false
  metadata_in:
    type: File?
    inputBinding:
      valueFrom: |
                  ${
                    if (inputs.metadata_in){
                      return "--metadata_in "+self.path+" --metadata_out vld_"+self.basename;
                    } else  {
                      return false;
                    }
                  }
      position: 5
      shellQuote: false
outputs:
  feat_out: 
    type: File
    outputBinding:
      glob: vld_$(inputs.feat_in.basename)
  label_out: 
    type: File
    outputBinding:
      glob: vld_$(inputs.label_in.basename)
  metadata_out: 
    type: File?
    outputBinding:
      glob: | 
        ${
          if(inputs.metadata_in){
            return "vld_"+inputs.metadata_in.basename;
          } else {
            return false; 
          }
        }