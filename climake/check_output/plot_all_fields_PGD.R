library(Rfa)

# Define the name of the output folder for which you want to make pdf of the PGD
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  args[1] = "BE13b_zoom_c46_URB"
}



output_dir <- "/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc46613_amber/Project_CitiesandPrecipitation/climate_simulations/runs/code_to_run_model/PGD-clim-Tier1/climake/outputFiles"
output_file_name <- "PGD.fa"
pgdfilename <- paste(output_dir, args[1], output_file_name, sep="/")

print(pgdfilename)

pdffilename <- paste(args[1], "_PGD.pdf", sep="")

print(pdffilename)


x <- FAopen(pgdfilename)
varname_list <- x$list

print("CHECK1")

pdf(pdffilename, width = 8, height = 7)

for (idx in seq_along(varname_list["name"][[1]])) {
    varname <- varname_list["name"][[1]][[idx]]
    y <- try(FAdec(x, varname), silent=TRUE)
    if (!inherits(y, "try-error")) {
        iview(y, legend=TRUE)
    }
}

dev.off()
