#!/usr/bin/env nextflow

//author: Alex Mattausch, EMBL Heidelberg 2016-2017

// Instance names of the modular components of the pipeline
validator="data_validator.r"
selector="sample_selector.r"
metachecker="confounder_check.r"
filter="generic_filter.r"
assocchecker="association_check.r"
normalizer="generic_normalizer.r"
metapredadder="meta_predictor_adder.r"
splitter="data_splitter.r"
builder=params.LMMETHOD+"_trainer.r"
predictor=params.LMMETHOD+"_predictor.r"
assessor="model_evaler.r"
interpretor="model_interpretor.r"

//PIPELINE SETUP
println("Setting up pipeline...")
//input files
feat    = file(params.datadir+"/feat_"+params.tag+".tsv")
label   = file(params.datadir+"/label_"+params.tag+".tsv")
nummeta = file(params.datadir+"/nummeta_"+params.tag+".tsv")

//check wether metadata is available
if(nummeta.exists()){
    nummeta_exists = true
    println("Numeric metadata '"+nummeta.getName()+"' found!")
} else {
    nummeta_exists = false
    println("No metadata found, will continue anyway!")
}

//output directory
workdir = file(params.workdir+"/"+params.tag)
workdir.mkdir()
feat.copyTo(workdir)
label.copyTo(workdir)
if(nummeta_exists){nummeta.copyTo(workdir)}

//PIPELINE STEPS
println("Starting pipeline...")

//Data validation:
if (nummeta_exists){

    //Validate with metadata

    process VALIDATE{

        if(params.debug){
            echo true
        }

        storeDir workdir

        input:
        file feat
        file label
        file nummeta

        output:
        file "vld_${feat}" into vld_feat_FILTER
        file "vld_${label}" into vld_label_CHECKASSOC, vld_label_NORMALIZE, vld_label_SPLIT, vld_label_EVAL, vld_label_APPLYLASSO, vld_label_INTERPRET, vld_label_TRAINLASSO
        file "vld_${nummeta}" into vld_nummeta_INTERPRET, vld_nummeta_SPLIT

        script:
        """
        echo '${nummeta}: Metadata found!';
        ${params.pathtoR}/Rscript '${params.sourcedir}/${validator}' --srcdir='${params.sourcedir}' --feat_in='${feat}' --feat_out='vld_${feat}' --label_in='${label}' --label_out='vld_${label}' --metadata_in='${nummeta}' --metadata_out='vld_${nummeta}'
        """
    }

} else {

    //Validate without metadata

    process VALIDATE{

        if(params.debug){
            echo true
        }

        storeDir workdir

        input:
        file feat
        file label

        output:
        file "vld_${feat}" into vld_feat_FILTER
        file "vld_${label}" into vld_label_CHECKASSOC, vld_label_NORM, vld_label_SPLIT, vld_label_EVAL, vld_label_APPLYLASSO, vld_label_INTERPRET, vld_label_TRAINLASSO

        script:
        """
        echo '${nummeta}: No metadata available, will continue anyway!';
        ${params.pathtoR}/Rscript '${params.sourcedir}/${validator}' --srcdir='${params.sourcedir}' --feat_in='${feat}' --feat_out='vld_${feat}' --label_in='${label}' --label_out='vld_${label}'
        """

    }
}

//FILTER features in an unsupervised manner (i.e. without looking at the label information)
process FILTER{

    if(params.debug){
        echo true
    }

    storeDir workdir

    input:
    file vld_feat_FILTER

    output:
    file "filt_${vld_feat_FILTER}" into filt_vld_feat_CHECKASSOC, filt_vld_feat_NORM

    script:
    """
    ${params.pathtoR}/Rscript ${params.sourcedir}/${filter} --feat_in='${vld_feat_FILTER}' --feat_out='filt_${vld_feat_FILTER.getName()}' --method=${params.filt_method} --cutoff=${params.cutoff} --rm_unmapped=${params.rm_unmapped} --recomp_prop=${params.recomp_prop}
    """

}

//CHECK for significant ASSOCiations between features (abundances) and labels

process CHECKASSOC{

    if(params.debug){
        echo true
    }

    storeDir workdir

    input:
    file filt_vld_feat_CHECKASSOC
    file vld_label_CHECKASSOC

    output:
    file "assoc_plots_${params.tag}.pdf" into assoc_plots

    script:
    """
    ${params.pathtoR}/Rscript ${params.sourcedir}/${assocchecker} --srcdir=${params.sourcedir} --label_in='${vld_label_CHECKASSOC}' --feat_in='${filt_vld_feat_CHECKASSOC}' --plot='assoc_plots_${params.tag}.pdf' --mult_test=${params.mult_corr} --alpha=${params.alpha} --min_fc=${params.min_fc} --detect_limit=${params.detect_lim} --col_scheme=${params.COL} --plot_type=${params.plot_type}
    """

}

//NORMalize features (again without looking at the labels!)

process NORM{

    if(params.debug){
        echo true
    }

    storeDir workdir

    input:
    file filt_vld_feat_NORM

    output:
    file "norm_${filt_vld_feat_NORM}" into norm_filt_vld_feat_TRAINLASSO, norm_filt_vld_feat_APPLYLASSO, norm_filt_vld_feat_INTERPRET
file "normalization_parameters.txt" into norm_param_file

    script:
    """
    ${params.pathtoR}/Rscript ${params.sourcedir}/${normalizer} --feat_in=${filt_vld_feat_NORM} --feat_out=norm_${filt_vld_feat_NORM.getName()} --param_out='normalization_parameters.txt' --method=${params.norm_method} --log_n0=${params.log_n0} --sd_min_quantile=${params.sd_min_q} --norm_feature=${params.n_feat} --norm_sample=${params.n_sample} --norm_global=${params.n_global} --vector_norm=${params.norm}
    """

}

//SPLIT data for cross validation

if (nummeta_exists){

    process SPLIT{

        if(params.debug){
            echo true
        }

        storeDir workdir

        input:
        file vld_label_SPLIT
        file vld_nummeta_SPLIT

        output:
        file "train_sets.tsv" into train_sets_TRAINLASSO
        file "test_sets.tsv" into test_sets_APPLYLASSO

        script:
        """
        ${params.pathtoR}/Rscript ${params.sourcedir}/${splitter} --srcdir=${params.sourcedir} --label_in=${vld_label_SPLIT} --train_sets='train_sets.tsv' --test_sets='test_sets.tsv' --num_folds=${params.n_folds_cv} --resample=${params.n_repeats_cv} --stratify=${params.stratify_cv} --inseparable=${params.inseparable_cv} --metadata_in='${vld_nummeta_SPLIT}.tsv'
        """

    }

} else {

    process SPLIT{

        storeDir workdir

        if(params.debug){
            echo true
        }

        input:
        file vld_label_SPLIT

        output:
        file "train_sets.tsv" into train_sets_TRAINLASSO
        file "test_sets.tsv" into test_sets_APPLYLASSO

        script:
        """
        ${params.pathtoR}/Rscript ${params.sourcedir}/${splitter} --srcdir=${params.sourcedir} --label_in=${vld_label_SPLIT} --train_sets='train_sets.tsv' --test_sets='test_sets.tsv' --num_folds=${params.n_folds_cv} --resample=${params.n_repeats_cv} --stratify=${params.stratify_cv} --inseparable=${params.inseparable_cv}
        """

    }

}

//TRAIN a model for each data split

process TRAINLASSO{

    if(params.debug){
        echo true
    }

    storeDir workdir

    input:
    file norm_filt_vld_feat_TRAINLASSO
    file vld_label_TRAINLASSO
    file train_sets_TRAINLASSO

    output:
    file "trained_${params.LMMETHOD}_models_${params.tag}.tsv" into trained_model_APPLYLASSO, trained_model_INTERPRET

    script:
    """
    ${params.pathtoR}/Rscript ${params.sourcedir}/${builder} --srcdir=${params.sourcedir} --label_in='${vld_label_TRAINLASSO}' --feat_in='${norm_filt_vld_feat_TRAINLASSO}' --model='trained_${params.LMMETHOD}_models_${params.tag}.tsv' --train_sets=${train_sets_TRAINLASSO} --num_folds=${params.n_folds_ms} --stratify=${params.stratify_ms} --sel_criterion=${params.ms_criterion} --min_nonzero_coeff=${params.min_nonzero_coeff}
    """
}

//APPLY the model to make predictions

process APPLYLASSO{

    if(params.debug){
        echo true
    }

    storeDir workdir

    input:
    file norm_filt_vld_feat_APPLYLASSO
    file trained_model_APPLYLASSO
    file test_sets_APPLYLASSO
    file vld_label_APPLYLASSO

    output:
    file "${params.LMMETHOD}_predictions_${params.tag}.tsv" into pred_EVAL, pred_INTERPRET

    script:
    """
    ${params.pathtoR}/Rscript ${params.sourcedir}/${predictor} --srcdir=${params.sourcedir} --label_in='${vld_label_APPLYLASSO}' --feat_in='${norm_filt_vld_feat_APPLYLASSO}' --test_sets='${test_sets_APPLYLASSO}' --model='${trained_model_APPLYLASSO}' --pred='${params.LMMETHOD}_predictions_${params.tag}.tsv'
    """

}

//EVALuate model (test) performance

process EVAL{

    if(params.debug){
        echo true
    }

    storeDir workdir

    input:
    file vld_label_EVAL
    file pred_EVAL

    output:
    file "evaluation_plots_${params.tag}.pdf" into eval_plots

    script:
    """
    ${params.pathtoR}/Rscript ${params.sourcedir}/${assessor} --srcdir=${params.sourcedir} --label='${vld_label_EVAL}' --pred=${pred_EVAL} --plot='evaluation_plots_${params.tag}.pdf'
    """

}

//Extracts model properties for INTERPRETATION

if(nummeta_exists){

    process INTERPRET{

        if(params.debug){
            echo true
        }

        storeDir workdir

        input:
        file norm_filt_vld_feat_INTERPRET
        file vld_label_INTERPRET
        file trained_model_INTERPRET
        file pred_INTERPRET
        file vld_nummeta_INTERPRET
        file feat

        output:
        file "model_plots_${params.tag}.pdf" into model_plots

        script:
        """
        ${params.pathtoR}/Rscript ${params.sourcedir}/${interpretor} --srcdir=${params.sourcedir} --feat=${norm_filt_vld_feat_INTERPRET} --origin_feat='${feat}' --label=${vld_label_INTERPRET} --meta=${vld_nummeta_INTERPRET} --model=${trained_model_INTERPRET} --pred=${pred_INTERPRET} --plot='model_plots_${params.tag}.pdf' --col_scheme=${params.COL} --heatmap_type=${params.heatmap_type} --consens_thres=${params.consens_thres}
        """

    }

} else  {

    process INTERPRET{

        if(params.debug){
            echo true
        }

        storeDir workdir

        input:
        file norm_filt_vld_feat_INTERPRET
        file vld_label_INTERPRET
        file trained_model_INTERPRET
        file pred_INTERPRET
        file feat

        output:
        file "model_plots_${params.tag}.pdf" into model_plots

        script:
        """
        ${params.pathtoR}/Rscript ${params.sourcedir}/${interpretor} --srcdir=${params.sourcedir} --feat=${norm_filt_vld_feat_INTERPRET} --origin_feat='${feat}' --label=${vld_label_INTERPRET} --model=${trained_model_INTERPRET} --pred=${pred_INTERPRET} --plot='model_plots_${params.tag}.pdf' --col_scheme=${params.COL} --heatmap_type=${params.heatmap_type} --consens_thres=${params.consens_thres}
        """
    }

}
