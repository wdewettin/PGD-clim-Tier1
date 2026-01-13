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

# Make list of fields with possible NaN
fields_with_nan <- list( "SFX.SSO_STDEV   ", "SFX.SSO_ANIS    ","SFX.SSO_DIR     ","SFX.SSO_SLOPE   ","SFX.HO2IP       ","SFX.HO2JP       ","SFX.HO2IM       ","SFX.HO2JM       ","SFX.AOSIP       ","SFX.AOSJP       ","SFX.AOSIM       ","SFX.AOSJM       ","SFX.BATHY       ","SFX.CLAY        ","SFX.SAND        ","SFX.RUNOFFB     ","SFX.WDRAIN      ","SFX.GD_CLAY     ","SFX.GD_SAND     ","SFX.GD_RUNOFFB  ","SFX.GD_WDRAIN   ")

for (idx in seq_along(varname_list["name"][[1]])) {
    varname <- varname_list["name"][[1]][[idx]]
    y <- try(FAdec(x, varname), silent=TRUE)
    if (!inherits(y, "try-error")) {
      if (varname %in% fields_with_nan){
        print("Found field with NaN")
        print(varname)
        y_nonan <- y
        y_nonan[y_nonan>=1e20] <- NA
        iview(y_nonan, legend=TRUE)} 
        else {
        iview(y, legend=TRUE)}
    }
}

dev.off()
