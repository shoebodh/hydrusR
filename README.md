
#Some utility functions to run the HYDRUS-1D soil-water flow model through R (variably saturated flow through soils). ###NOTE: This package requires the open-source HYDRUS-1D software installed in your system, which can be downloaded from https://www.pc-progress.com/en/Default.aspx?H1d-downloads.

hydrusR came out of my own need to run the soil-water flow model HYDRUS-1D (Simunek et al., 1988) for relatively longer periods than typically allowed (in one simulation) by the HYDRUS-1D software. For example, running simulations for a year and printing outputs at 15 minute intervals. Hydrus GUI currently allows only 250 print times while you can set up upto 1000 print times in the 'selector.in' if run from the command line.

Model set-up utilities include the functions for writing out soil parameters, initial conditions, root distribution, observation nodes, and boundary conditions (top and bottom) to the main input file 'selector.in'.

One can run the HYDRUS simulation right from within R.

The package also allows for reading the raw simulation output files into R in a more analysis-friendly formats.

This is now slightly improved and enables performing long simulations from within R. i.e., with number of time steps more than the HYDRUS1D GUI permits. 
For example it is possible to perform simulation for a year at hourly time steps where as HYDRUS1D only permits printing 960( or may be 1000, not sure) time steps.

This package automatically devides the time-steps, sequentially performs simulations and finally compile the primary outputs. 

Also check out the package RHydrus that attempts to convert the original FORTRAN code to C and create a native hydrus R package. https://github.com/mespe/RHydrus
