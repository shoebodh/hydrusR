#Some utility functions to run the HYDRUS-1D soil-water flow model through R (variably saturated flow through soils). ###NOTE: This package requires the open-source HYDRUS-1D software installed in the system, which can be downloaded from https://www.pc-progress.com/en/Default.aspx?H1d-downloads.

hydrusR came out of my own need to simulate soil water flow experiments using HYDRUS-1D, (Simunek et al., 1988) for relatively longer time periods than typically allowed (in one simulation) by the HYDRUS-1D software. For example, running simulations for a year and printing outputs at 15 minute intervals. Hydrus GUI currently allows only 250 print times while you can set up upto 1000 print times in the 'selector.in' if run from the command line.

This package automatically devides the time-steps, sequentially performs simulations and finally compile the primary outputs. 

Model set-up utilities include the functions for writing out soil parameters, initial conditions, root distribution, observation nodes, and boundary conditions (top and bottom) to the main input file 'selector.in'.

One can run the HYDRUS simulation right from within R.

Functions for reading the raw simulation output files into R in a more analysis-friendly formats are also there

This is now slightly improved and is more seamless in producing hydrus outputs and compiling the simulations. 

Also check out the package RHydrus that attempts to convert the original FORTRAN code to C and create a more native HYDRUS1D package in R. https://github.com/mespe/RHydrus

Installation

devtools::install_github("shoebodh/hydrusR")

## run a test simulation
source(system.file("examples/h1d_flow_example.R", package = "hydrusR"))

To do:
Add top constant boundary conditions (currently only time variable boundary conditions is checked)
Add other processes(e.g. solute transport)
