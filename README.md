I used the same scripts as in the toy examples on the wormsim for model testing. Used synthetic data (using some pre defined transmission parameters), check if these parameter values can be recovered

Description:

In this folder there are 3 sub-folders, namely the 04_wormsim-v2.58Ap27 (the source code wormsim v2.58Ap27), the output (the results from running the SMC algorithm saved as myrun14_10_18.Rdata and in pdf myrun14_10_18.pdf) and the wormsim_source (all necessary files to parse transmission values to the xml provided – template2MDA, the following r scripts:

smc_run_Peri_v2_run_me.r (this is the front-end to run the new ABC-SMC script)

smc_app_dependent_functions_v2_Peri.r (smc dependent files linked to distance metric used the difference in the proportion of prevalence between simulations and observations, summed squared)

smc_core_functions_v2_Peri.r (this contains the core functions of the smc code)

smc_plot_functions.r (this contains the necessary code to plot the results from the SMC code)

smc_prior_functions_v2_Peri.r (this file log-transforms the priors provided)

and 2 csv files, the ydata.csv (number of peopled surveyed by age) and the syn.obs.csv (intensity categorization by level, year and relavant prevalanve). More-over the contents of this files include:

"village","year","intens","cases","N","prev"

"TestVillage",2011,"None",34,43,0.81
