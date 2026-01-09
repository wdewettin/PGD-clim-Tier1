library(Rfa)

# Define the name of the output folder for which you want to make pdf of the clims
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  args[1] = "BE13b_zoom_c46_URB"
}

mstart <- 1
mend <- 12

climfilename_dir <- "/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc46613_amber/Project_CitiesandPrecipitation/climate_simulations/runs/code_to_run_model/PGD-clim-Tier1/climake/outputFiles"
climfilename_base<- paste(climfilename_dir, args[1], "Const.Clim.", sep="/")
pdffilename_base <- paste(args[1], "_Const.Clim.", sep="")


for (m in mstart:mend) {
    if (m < 10) {
        mm <- paste("0", as.character(m), sep = "")
    } else {
        mm <- as.character(m)
    }

    print(mm)

    climfilename <- paste(climfilename_base, mm, sep="")
    x <- FAopen(climfilename)
    varname_list <- x$list

    pdffilename <- paste(pdffilename_base, mm, ".pdf", sep="")
    pdf(pdffilename)

    for (idx in seq_along(varname_list["name"][[1]])) {
        varname <- varname_list["name"][[1]][[idx]]
        y <- FAdec(x, varname)
        iview(y, legend = TRUE)
    }

    dev.off()
}