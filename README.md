README
================

# Post-clustering difference testing: valid inference and practical considerations with applications to ecological and biological data

Contains codes to reproduce simulations, figures and analysis of the
article *Post-clustering difference testing: valid inference and
practical considerations with applications to ecological and biological
data*.

## Organisation

### simulations-codes

Contains all simulations codes to reproduce the main Figures in the
article.

-   `simulation_figure1.R` produces the results displayed in Figure 1:
    Artificial differences created by clustering.

-   `simulation_figure2.R` produces the results displayed in Figure 2:
    Validity of p-values returned by our proposed tests, comparison with
    t-test.

-   `simulation_figure3.R` produces the results displayed in Figure 3:
    Statistical power of the proposed tests.

### simulations-results

Contains sub-folder with the simulations results of the
**simulations-codes** folder:

-   **results_figure1** contains `pval_pb.csv` illustrating
    post-clustering inference problem.

-   **results_figure2** contains:

    -   `simu_H0.csv`: results of the simulations under the no-clusters
        scenario.
    -   `simu_H1.csv`: results of the simulations under the
        three-clusters scenario.

-   **results_figure3** contains:

    -   `power_dip_delta.csv`: statistical power of the multimodality
        test as a function of mean differences $\delta$ between the two
        modes of a 2-components Gaussian mixture.
    -   `power_SI_2cl_delta.csv`: statistical power of the selective
        test as a function of mean differences $\delta$ between the two
        modes of a 2-components Gaussian mixture when $2$ clusters are
        estimated.
    -   `power_SI_4cl_delta.csv`: statistical power of the selective
        test as a function of mean differences $\delta$ between the two
        modes of a 2-components Gaussian mixture when $4$ clusters are
        estimated
    -   `power_merge_delta.csv`: statistical power of the merging test
        as a function of mean differences $\delta$ between the two modes
        of a 2-components Gaussian mixture when $4$ clusters are
        estimated

### applications-codes

Contains codes to reproduce the real data analysis.

-   `palmerpenguins_negative_data_analysis.R` contains the codes needed
    to reproduce the negative control data analysis (Web Table 1)

-   `palmerpenguins_positive_data_analysis` contains the codes needed to
    reproduce the positive control data analysis (Table 1)

-   `HIPC_data_analysis.R` contains the codes needed to reproduce HIPC
    T-cells data analysis (Table 2, Figure 5, Web Figure 7 )

### figures-codes

Contains codes to generate the figures presented in the article

### figures

Contains the main figures of the article generated using codes in the
**figures-codes** and **applications-codes** folders using the
**simulations-results** data.

### supplementary

Contains codes to reproduce simulations and figures of the Web Appendix.

#### simulations-codes

-   **codes_figureS1** contains simulation codes for Figure S1:
    Comparison of different multimodality tests implemented in the
    `multimode` package.

    -   `multimode_power_delta.R`: Monte-Carlo simulations to evaluate
        statistical power of multimodality tests implemented in the
        `multimode` package as a function of the mean differences
        $\delta$ between the two modes of a 2-components Gaussian
        mixture.
    -   `mulitmode_power_n.R`: Monte-Carlo simulations to evaluate
        statistical power of multimodality tests implemented in the
        `multimode` package as a function of the sample size $n$.
    -   `multimode_unimodal_case.R`: Simulations under the null of
        unimodality for Gaussian and Uniform distributed data

-   **codes_figureS2** contains simulation codes for Figure S2: Impact
    of the variance estimation on the distribution of p-values returned
    by the selective test under the null of no separation between
    clusters.

    -   `over_estim_var.R`
    -   `under_estim_var.R`

-   **codes_figureS4** contains simulation codes for Figure S3:
    Comparison of different merging functions for p-values aggregation
    proposed by Vovk and Wang.

    -   `adj_pval_comp.R`
    -   `power_comp_merge.R`

-   **codes_figureS5** contains simulation codes for Figure S5:
    Additional simulation studies to evaluate the impact of the sample
    size.

    -   `sample_size.R`

-   **codes_figureS7** contains simulation codes for Figure S7: Impact
    of a miss-specification of the distributional assumption under the
    null.

    -   `distributional_assumption.R`

-   **codes_sectionS3** contains simulations codes for all the
    Supplementary Section S3

#### simulations-results

-   **results_figureS1** contains:
    -   results of `codes_figureS1/multimode_unimodal_case.R`:
        -   `multimode_test_unimodale_gaussian.csv`
        -   `multimode_test_unimodale_uniform.csv`
    -   results of `codes_figureS1/multimode_power_delta.R`:
        -   `multimode_test_power.csv/`
    -   results of `codes_figureS1/mulitmode_power_n.R`:
        -   `multimode_test_power_n.csv`
        -   `time_multimode.csv`
-   **results_figureS2** contains:
    -   results of `codes_figureS2/over_estim_var.R`:
        -   `estim_var.csv`
    -   results of `codes_figuresS2/under_estim_var.R`:
        -   `estim_var_under.csv`
-   **results_figureS3** contains:
    -   `3clusters_C1C2_SI_problem.csv`
    -   `3clusters_C1C3_SI_problem.csv`
-   **results_figureS4** contains:
    -   results of `codes_figureS4/adj_pval_comp.R`:
        -   `pval_merge_bonf_H0.csv`
        -   `pval_merge_bonf_H1.csv`
        -   `pval_merge_geo_H0.csv`
        -   `pval_merge_geo_H1.csv`
        -   `pval_merge_harm_H0.csv`
        -   `pval_merge_harm_H1.csv`
    -   results of `codes_figureS4/power_comp_merge.R`:
        -   `power_comp_merge_bonf.csv`
        -   `power_comp_merge_geo.csv`
        -   `power_comp_merge_harm.csv`
-   **results_figureS5** contains:
    -   results of `codes_figureS5/sample_size.R`:
        -   `merge_selective_inference_4cl_d=0.csv`
        -   `mutlimod_d=0.csv`
        -   `mutlimod_d=5.csv`
        -   `selective_inference_2cl_d=0.csv`
        -   `selective_inference_2cl_d=5.csv`
        -   `selective_inference_4cl_d=0.csv`
        -   `selective_inference_4cl_d=5.csv`
        -   `ttest_d=0.csv`
        -   `ttest_d=5.csv`
-   **results_figureS7** contains:
-   results of `codes_figureS7/distributional_assumpution.R`:
    -   `pval_distri_H0_dip.csv`
    -   `pval_distri_H0_SI.csv`
-   **results_sectionS3** contains $3$ folders:
    -   `p=4` with the results of simulations where the number of
        dimensions was equal to $4$
    -   `p=10` with the results of simulations where the number of
        dimensions was equal to $10$
    -   `time` with the results of the computational times of each tests

#### figures-codes

Contains codes to generate the figures presented in the Web Appendix.

#### figures

Contains the main figures of the Web Appendix generated using codes in
the **figures-codes** folder using the **simulations-results** data.

## Notes

1.  The three proposed tests are implemented in a `R` package
    `VALIDICLUST`. The current version of the package could be installed
    using the `VALIDICLUST_0.1.0.tar.gz` file. The development version
    of `VALIDICLUST` is also available on
    [Github](https://github.com/benhvt/VALIDICLUST).

2.  Simulations were performed on CURTA from the *Mésocentre de Calcul
    Intensif Aquitain*:

<!-- -->

    R version 3.6.3 (2020-02-29)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: CentOS Linux 7 (Core)

3.  Data availability:

-   Penguins data could be found in the [`palmerpenguins`
    package](https://cran.r-project.org/web/packages/palmerpenguins/index.html)

-   HIPC T-cells data could be found in the [`cytometree`
    package](https://cran.r-project.org/web/packages/cytometree/index.html)

The two datasets could be load with the following instructions:

``` r
library(palmerpenguins)
library(cytometree)

# For palmerpenguins data
data("penguins")

# For HIPC T-cells data
data("HIPC")

sessionInfo()
```

    ## R version 4.2.1 (2022-06-23)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur ... 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] fr_FR.UTF-8/fr_FR.UTF-8/fr_FR.UTF-8/C/fr_FR.UTF-8/fr_FR.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] cytometree_2.0.2     Rcpp_1.0.9           palmerpenguins_0.1.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.2.1     pillar_1.8.1       tools_4.2.1        mclust_6.0.0      
    ##  [5] digest_0.6.31      evaluate_0.19      lifecycle_1.0.3    tibble_3.1.8      
    ##  [9] gtable_0.3.1       pkgconfig_2.0.3    rlang_1.0.6        igraph_1.3.5      
    ## [13] cli_3.4.1          rstudioapi_0.14    yaml_2.3.6         xfun_0.35         
    ## [17] fastmap_1.1.0      stringr_1.5.0      dplyr_1.0.10       knitr_1.41        
    ## [21] generics_0.1.3     vctrs_0.5.1        cowplot_1.1.1      grid_4.2.1        
    ## [25] tidyselect_1.2.0   glue_1.6.2         R6_2.5.1           fansi_1.0.3       
    ## [29] GoFKernel_2.1-1    rmarkdown_2.18     ggplot2_3.4.0      magrittr_2.0.3    
    ## [33] scales_1.2.1       htmltools_0.5.4    colorspace_2.0-3   KernSmooth_2.23-20
    ## [37] utf8_1.2.2         stringi_1.7.8      munsell_0.5.0
