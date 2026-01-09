library(Rfa)

# Get the filenames of the clim-files you want to compare
output_dir <- "/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc46613_amber/Project_CitiesandPrecipitation/climate_simulations/runs/code_to_run_model/PGD-clim-Tier1/climake/outputFiles/"
output_folder_1 <- "BE13b_zoom_c46_URB"
output_folder_2 <- "BE13b_zoom_c46_NOURB_1_TEBoff"
filename_clim_1 <- paste(output_dir, output_folder_1, "Const.Clim.01", sep="/")
filename_clim_1 <- paste(output_dir, output_folder_2, "Const.Clim.01", sep="/")

pdffilename <- "compare_clim.pdf"

# Open the clim-files
x <- FAopen(filename_clim_1)
y <- FAopen(filename_clim_1)
varname_list <- x$list

# For each field: calculate difference and plot
pdf(pdffilename, width = 10, height = 7)

 for (idx in seq_along(varname_list["name"][[1]])) {
        varname <- varname_list["name"][[1]][[idx]]
        xx <- FAdec(x, varname)
        yy <- FAdec(y, varname)
        frac <- xx/yy
        iview(frac, legend = TRUE)
    }

dev.off()
