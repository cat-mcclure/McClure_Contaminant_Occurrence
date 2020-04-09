# McClure_Contaminant_Occurrence
 Code for the Bayesian hierarchical joint-contaminant model used to analyze the occurrence and co-occurrence of contaminants in the Chesapeake Bay Watershed. Additional information on the R package used (Hierarchical Modelling of Species Communities; HMSC) can be found at https://www.helsinki.fi/en/researchgroups/statistical-ecology/hmsc

https://doi.org/10.5281/zenodo.3746590


All necessary data sets can be found in the Data folder, along with the necessary shape files for mapping

See Williams et al., 2019 (https://doi.org/10.5066/P96L2GB0) for full data set 



Description of R Scripts:


Install_HMSC.R is the code for installing the software
 
 
HMSC_ag.R is the model for the AG data set (includes an additional river site)


HMSC_flow_ag.R is the model for the FLOW data set (excludes a river site in order to evaluate the effect of river discharge)


HMSC_int_only_ag.R is the model for the AG data set that excludes covariates


HMSC_int_only_flow_ag.R is the model for the FLOW data set that excludes covariates


Maps_Ch_1_MS.R is the mapping code (Figure 1)


Variance_Partitioning.R is the plotting code for the variance partitioning (Figure 2)


Effect_plots_Flow_Ag.R is the plotting code for estimated effects plots and posterior probability of a positive effect for the FLOW data set (Figures 3 and 4)


Association_networks_code.R is the plotting code for association networks (co-occurrence)(Figure 5)
 
 
Effects_plots_Ag.R is the plotting code for estimated effects plots and posterior probability of a positive effect for the AG data set (figures in appendix)


Occurence_curves_flow_ag.R is the plotting code (not used in manuscript) to plot the occurrence of contaminants over an agricultural land use gradient by season
