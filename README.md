## Sampedro_etal_ERL

**Enhancing energy and food access through reduction of income inequality does not jeopardize the achievement of existing climate pledges**

Jon Sampedro<sup>1,2\*</sup>, Dirk-Jan Van de Ven<sup>1</sup>, Russell Horowitz<sup>1</sup>, Clàudia Rodés-Bachs<sup>1</sup>, Iñaki Arto<sup>1</sup>, Maria Victoria Román<sup>1</sup>, Manuel Tomás<sup>1</sup>, 
Kanishka Narayan<sup>3</sup>, Pralit Patel<sup>3</sup>, 

<sup>1 </sup> Basque Center for Climate Change, Leioa, Spain

<sup>2 </sup> IKERBASQUE, Basque Foundation for Science, Plaza Euskadi 5, 48009 Bilbao, Spain

<sup>3 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, United States of America

\* corresponding author:  jon.sampedro@bc3research.org

## Abstract
Reducing within-region income inequality is a global priority that must be urgently addressed to promote human development and enhance global access to energy and food. This could lead to higher energy-related emissions by lower-income consumers, potentially creating some conflicts with climate change mitigation objectives. Using an enhanced version of the Global Change Analysis Model, we simulate reductions of within-region income inequality that allow for narrowing differences in energy and food demand across deciles by 2050 in a Paris-compliant world. The energy demand increase by lower-income consumers is greater than the decline in demand by wealthier and more satiated income groups, leading to an overall rise in total demand. Nevertheless, the projected higher energy demand in the proposed more egalitarian societies only slightly increases the regional carbon prices and mitigation costs associated with the emission targets, therefore posing minimal impact on the achievement of the existing portfolio of climate pledges.      

## Code reference
The GCAM_GRAPHICS model used in this study is available at Zenodo: https://zenodo.org/records/16960260

jonsampedro. (2025). GCAM-GRAPHICS-7.0.0 (GCAM-GRAPHICS-v7.0.0). Zenodo. https://doi.org/10.5281/zenodo.16960260

## Contributing modeling software
| Model | Version | Repository Link 
|-------|---------|-----------------
| Global Change Analysis Model (GCAM) | GCAM_GRAPHICS| [https://github.com/jonsampedro/gcam-core](https://github.com/jonsampedro/gcam-core/tree/GCAM_GRAPHICS_v7_paperInequality) | 

| Component| Version | Repository Link 
|-------|---------|-----------------
| gcamdata | 1.0 | https://github.com/JGCRI/gcamdata | 
| rgcam | 1.2.0 | https://github.com/JGCRI/rgcam | 
| pridr | 0.1.0 | https://github.com/JGCRI/pridr | 
| rmap | 1.0.0 | https://github.com/JGCRI/rmap | 

## Reproduce the experiment
To reproduce the results and figures shown in Sampedro et al.,

1. Install `R` here - https://www.r-project.org/
2. Install `R studio` from here - https://www.rstudio.com/
3. Run the script called `process_results_inequality.R` chunk by chunk to generate the figures.

## Acknowledgments
<div align="center">
  <img src="./graphics-logo.png" alt="GRAPHICS logo" width="120" height="150"/>
</div>

This project has received funding from the European Union's Horizon research program under grant agreement 101060679 (GRAPHICS project).
