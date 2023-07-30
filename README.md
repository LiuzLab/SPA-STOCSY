# SPA_STOCSY

SPA_STOCSY is an R pipeline for the automatic identification of both "known" and "unknown" metabolites in given samples.  
With given input samples, SPA_STOCSY offers automatic algorithms to estimate all the parameters and generate the final results.  
Here is an example of using SPA_STOCSY to analyze 10 NMR samples from brain homogenates of Drosophila melanogaster.

To apply SPA_STOCSY for NMR spectra with diversified resolutions:
- For high-resolution NMR spectra, we provide the low-dimension SPA-STOCSY tutorial. The metabolite clusters from SPA are represented by their average peak intensities, which is the input for the STOCSY step.

[Low-dimension SPA-STOCSY tutorial](https://liuzlab.github.io/SPA-STOCSY/tutorial/spa_stocsy_tutorial_lowd.html)

- For low-resolution NMR spectra, we provide the high-dimension SPA-STOCSY tutorial. For each metabolite cluster within SPA, all the local maximums are used as input for the STOCSY step, thus each cluster is represented by a list of peaks for correlation calculation, instead of the average value only.

[High-dimension SPA-STOCSY tutorial](https://liuzlab.github.io/SPA-STOCSY/tutorial/spa_stocsy_tutorial_highd.html)

## Data

All the data listed in the paper are stored in the input_data/folder. 
- 10_meta_simulation.csv: 50 synthesized samples from 10 metabolites
- htt_c12.csv: 10 NMR samples from brain homogenates of Drosophila melanogaster
- human_cell_data.csv: 22 NMR samples from human cells  
- chx_lib_trunc.csv: reference library for 257 metabolites with their cluster information  


## Source code

The main functions in SPA_STOCSY are stored in the source_codes/ folder.  
Load these R files before running the tool as in the example.


## Input

- The input file takes multiple NMR samples as columns. The first column should be specified as a list of chemical shift values.
- The NMR samples are highly recommended to get normalized on integral.

## Output

- The output location is defined by the user before running the tool as in the example
- The output files contain:
    - Multiple visualization plots during the running process
    - 'SPA_cluster_peaks.csv': peaks detected in each SPA cluster
    - 'STOCSY_corr_clusters.csv': correlated SPA clusters generated from STOCSY
    - 'auto_identify_metabolites.csv': list of identified metabolites with a set detection rate



