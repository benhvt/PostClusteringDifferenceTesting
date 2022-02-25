# article-experiment

Codes to reproduce simulations, figures and analysis of the article 

## Organisation 

### simulations-codes

**problem_pres.R** produces the results displayed in Figure 1

**behaviour_pval_2D.R** produces the results displayed in Figure 2

**power.R** produces the results displayed in Figure 3

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

## Notes 

1. Simulations were performe on CURTA 

2. Data used in the real analysis could be found in the [`palmerpenguins` package](https://cran.r-project.org/web/packages/palmerpenguins/index.html)
