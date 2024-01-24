*Frost matters: Incorporating late-spring frost in a dynamic vegetation model regulates regional productivity dynamics in European beech forests"*

{targets} pipeline to reproduce the data analysis and results from Meyer et al. (2024).

### Running the code

The analysis is contained in a {targets} pipeline. This functions similar to ```make``` to run the individual analysis steps. The main script is `_targets.R`. Running this script will initialize the pipeline and *download the data for the analysis from Zenodo*. 

The functions for the individual analysis steps are located in the `R` sub-folder. Depending on your R setup you may or may not need to install some additional packages to run the analysis. 