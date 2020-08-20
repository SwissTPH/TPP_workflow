# TPP_workflow: A quantitative approach to guide development of novel disease interventions

## Description
This repository contains a new approach to quantitatively define target product profiles (TPPs) of novel tools against infectious diseases with machine learning and disease transmission models. We apply our approach to inform development of new therapeutics and novel vector control for malaria. Our analysis identifies and ranks key intervention characteristics and key health system factors which drive malaria prevalence reduction. Furthermore, it allows quantification of the minimum requirements in terms of coverage, efficacy and duration of protection of malaria interventions to reach defined health goals across different transmission settings. 

The workflow and associated analysis informing development of new therapeutics and novel vector control for malaria is presented in the manuscript:
	
Monica Golumbeanu, Guojing Yang, Flavia Camponovo, Erin M. Stuckey, Nicholas Hamon, Mathias Mondy, Sarah Rees, Nakul Chitnis, Ewan Cameron, and Melissa A. Penny. (2020) **"A quantitative approach to guide development of novel disease interventions"** 

## Availability
The workflow is freely available under a GNU General Public License v3.0 at [https://github.com/SwissTPH/TPP_workflow](https://github.com/SwissTPH/TPP_workflow)

## Contact
- melissa.penny@unibas.ch
- monica.golumbeanu@unibas.ch

## Running the workflow
The workflow is modular and designed for running on a high performance cluster (HPC) with the slurm job scheduler. The worflow consists of the following main parts which are run sequentially on a HPC:
1. **W1_OM_simulation_workflow.sh**: includes the commands and functions needed for running simulations with [OpenMalaria](https://github.com/SwissTPH/openmalaria/wiki) and creating a database of simulation scenarios. All the files needed for running this part of the workflow are included in folders 1_OM_basic_workflow and 2_postprocessing. All the OpenMalaria xml files needed to define all scenarios and interventions are included in the folder Intervention_scenarios. Resource files defining the seasonal patterns and mosquito biting behavior are included in the folder Resource_files.

2. **W2_GP_analysis.sh**: comprises the machine learning part of the workflow consisting of:
	1. Training a Gaussian Process model on the set of previously simulated scenarios with OpenMalaria (folder 3_GP_train). 
	2. Sensitivity analysis and identification of the dirvers of intervention impact (folder 4_sensitivity_analysis)
	3. Optimisation of intervention characteristics for achieving a defined health goal (folder 5_optimization)
	
	
## Dependencies
- R version 3.6.0 (2019-04-26) or later with packages: dplyr, gridExtra, hetGP, lhs, metaheuristicOpt, multisensi, nloptr, rapportools, reshape2, Rsolnp, sensitivity, stringr, tools, lhs, tgp
