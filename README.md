# article-experiment

Contains codes to reproduce simulations, figures and analysis of the article 

## Organisation 

### simulations-codes

* **problem_pres.R** produces the results displayed in Figure 1

* **behaviour_pval_2D.R** produces the results displayed in Figure 2

* **power.R** produces the results displayed in Figure 3

### simulations-results

Contains the results of the simulations-codes folder : 

* **pval_pb.csv** : results of **problem_pres.R**

* **simu_H0.csv** and **simu_H1.csv** : results of **behaviour_pval_2D.R**

* **power_dip_delta.csv**, **power_SI_2cl_delta.csv**, **power_SI_4cl_delta.csv** and **power_merge_delta.csv** : results of **power.R**

### figures-codes
Contains codes to generate the figures presented in the article 

### figures
Contains the main figures of the article generated using codes in the figures-codes folder using the simulations-results data.

### applications-codes
Contains codes to reproduce the real data analysis :

* **negative_data.R** contains the codes needed to reproduce the negative control data analysis (Table 1)

* **positive_data.R** contains the codes needed to reproduce the positive control data analysis (Table 2)

* **HIPC_data.R** contains the codes needed to reproducte HIPC T-cells data analysis (Table 3, Figure 5, Supplementary Figure S7 and )

### supplementary 
Contains codes to reproduce simulations and figures of the supplementary material 

#### simulations-codes

* **multimode_power_delta.R**, **mulitmode_power_n.R** and **multimode_unimodal_case.R** produce the results displayed in Figure S1

* **over_estim_var.R** and **under_estim_var.R** produce the results displayed in Figure S2 

* **adj_pval_comp.R** and **power_comp_merge.R** produce the results displayed in Figure S4

* **time.R** produce the results displayed in Figure S5

* **distributional_assumpution.R** produce the results displayed in Figure S6

#### simulations-results 

* **multimode_test_unimodale_gaussian.csv** and **multimode_test_unimodale_uniform.csv** : results of **multimode_unimodal_case.R**

* **multimode_test_power.csv** : results of **multimode_power_delta.R**

* **multimode_test_power_n.csv** and **time_multimode.csv** : results of **mulitmode_power_n.R**

* **estim_var.csv** and **estim_var_under.csv** : results of **over_estim_var.R** and **under_estim_var.R**

* **suppS2_C1C2.csv** and **suppS2_C2C3.csv**: results of **FigureS1.R**

* **pval_merge_bonf_H0.csv**, **pval_merge_bonf_H1.csv**, **pval_merge_geo_H0.csv**, **pval_merge_geo_H1.csv**, **pval_merge_harm_H0.csv** and **pval_merge_harm_H1.csv** : results of **adj_pval_comp.R**

* **power_comp_merge_bonf.csv**, **power_comp_merge_geo.csv** and **power_comp_merge_harm.csv** : results of **power_comp_merge.R**

* **time_dip_2cl.csv**, **time_dip_4cl.csv**, **time_SI_2cl.csv**, **time_SI_4cl.csv** and  **time_merge_2cl.csv**,**time_merge_4cl.csv** : results of **time.R**

* **pval_distri_H0_dip.csv**, **pval_distri_H0_SI.csv** : results of **distributional_assumpution.R** 

#### figures-codes
Contains codes to generate the figures presented in the supplementary material 

#### figures
Contains the main figures of the supplementary materials generated using codes in the figures-codes folder using the simulations-results data.

## Notes 

1. Simulations were performed on CURTA 

2. Data availability: 

  - Penguins data could be found in the [`palmerpenguins` package](https://cran.r-project.org/web/packages/palmerpenguins/index.html)
  
  - HIPC T-cells data could be found in the [`cytometree` package](https://cran.r-project.org/web/packages/cytometree/index.html)