library(Rfa)

output_dir <- "/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc46613_amber/Project_CitiesandPrecipitation/climate_simulations/runs/code_to_run_model/PGD-clim-Tier1/climake/outputFiles/BE13b_zoom_c46_URB"
output_file_name <- "PGD.fa"
pdffilename <- "BE13b_zoom_c46_URB_PGD.pdf"

pgdfilename <- paste(output_dir, output_file_name, sep="/")
x <- FAopen(pgdfilename)
varname_list <- x$list

print("CHECK1")

pdf(pdffilename)

for (idx in seq_along(varname_list["name"][[1]])) {
    varname <- varname_list["name"][[1]][[idx]]
    y <- try(FAdec(x, varname), silent=TRUE)
    if (!inherits(y, "try-error")) {
        iview(y, legend=TRUE)
    }
}

dev.off()
