Welcome to the TransitSHIME GitHub Page!

Here, you will find the codebase that was used to conduct the analyses presented in our published paper titled "<b>Gut microbiota response to <i>in vitro</i> transit time variation is mediated by microbial growth rates, nutrient use efficiency and adaptation to <i>in vivo</i> transit time</b>", a.k.a the <b>TransitSHIME</b>. We are strong supporters of the FAIR principles and find it important to share our data. Therefore, this repository contains the code and configurations that were employed to generate the results and insights discussed in the paper.

## About the Paper

In the paper "<b>Gut microbiota response to <i>in vitro</i> transit time variation is mediated by microbial growth rates, nutrient use efficiency and adaptation to <i>in vivo</i> transit time</b>", we attempted to investigate the effects of transit time on an <i>in vitro</i> gut microbial community. To gain a deeper understanding of the context and significance of the analyses performed here, we strongly encourage you to refer to the paper. You can access the paper via the following [Link](URL).

### Reference

If you would use or build upon this code, please refer to Minnebo <i> et al.</i>, (2023) Gut microbiota response to <i>in vitro</i> transit time variation is mediated by microbial growth rates, nutrient use efficiency and adaptation to <i>in vivo</i> transit time</b>. <i>Microbiome</i> vol. X, p X-X

## What are the Repository contents?

This GitHub repository is organised to provide easy access to the code used in the analyses:

- **Code**: This section holds the source code responsible for conducting the analyses detailed in the paper. Each script is thoroughly documented to help you follow the process step by step.
- **Supporting code**: The main code is supported by some own scripts and functions, such as ggplot altering scripts, a bundle of colours, calculations of effect sizes...
- **Datasets**: In cases where datasets were employed for these analyses, we have included them here. These datasets are crucial for reproducing the results and drawing further insights.

## How to Use

If you are interested in replicating the analyses or building upon them, you can start by navigating to the `Code` directory. Inside, you will find individual scripts and markdowns that correspond to different aspects of the analyses carried out in the paper. Each script contains comments and explanations to guide you through the implementation.

Just as a headsup for my code, I like to use `test` as a name for placeholders, temporary objects or dataframes. The permanent/definitive objects always get their own specific name. 

### Which markdowns contain the code to which figures/results?

As you can see, there are 3 markdowns in the code: `FCM_analyses.Rmd`, `NGS_analyses.Rmd` and `SCFA_analyses.Rmd`. Each of them contain codes to specific results and figures:
* <b>FCM_analyses</b> (Figure <b>1</b>, <b>S6, S7 and S8</b>): The code related to all flow cytometry analyses AND carbohydrate utilisation. The raw flow cytometry data that has been analysed is accessible via this [Link](http://flowrepository.org/id/FR-FCM-Z5K7). NOTE: If you want to use the same gating as in the paper, then you should skip the chunk `Gate samples split per SHIME run` (line 242) and continue on line 419. The gating csv's (e.g. SGPI_Backgroundgate1.csv and SGPI_DMGgate1.csv) have been provided in the `Datasets`.
* <b>NGS_analyses</b> (Figure <b>2</b>, <b>3</b>, <b>4</b>, <b>S1, S4, S5, S9, and S10</b>): In this chunk of code, the 16S rRNA gene amplicon sequencing data was analysed. The "start" data was mothur processed sequencing data, consisting of a read count table (`counttable.shared` in `datasets`) and the taxonomic annotation (`taxtable.taxonomy` in `datasets`). If you want to reproduce the mothur pipeline, the raw sequencing reads are available in the European Bioinformatics Institute’s (EBI) European Nucleotide Archive (ENA) with accession number ERP138715.
* <b>SCFA_analyses</b> (Figure <b>5</b>, <b>S2, S11, S13, S14, and S15</b>): The initial dataset (`dataframe_SCFA.csv`) already contains concentrations of the short-chain fatty acids, which have been externally converted into mM and adjusted for dilutions. If necessary, the more raw form of the data can be provided upon request.



Something to make your life easier: We've labeled the code sections relevant to each figure with descriptive titles. When you open the code outline, you'll immediately spot these markers, guiding you to the exact portions of code that generated the figures mentioned in the paper.



Feel free to experiment, modify, and expand upon the code as you see fit. Should you encounter any questions or obstacles, we encourage you to raise an Issue in this repository. 



Happy (re)analysing!

Yorick <i> et al.</i>
