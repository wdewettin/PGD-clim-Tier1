library(Rfa)

output_dir <- "/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc46613_amber/Project_CitiesandPrecipitation/climate_simulations/runs/code_to_run_model/PGD-clim-Tier1/climake/outputFiles/BE13b_zoom_c46_URB"
output_file_name_PGD <- "PGD.fa"
output_file_name_clim <- "Const.Clim.01"
filename_PGD <- paste(output_dir, output_file_name_PGD, sep="/")
filename_clim <- paste(output_dir, output_file_name_clim, sep="/")

pdffilename <- "compare_orography.pdf"


x <- FAopen(filename_PGD)
xo <- FAdec(x, 'SFX.ZS')

y <- FAopen(filename_clim)
yo <- FAdec(y, 'SURFGEOPOTENTIEL')

diff <- xo-(yo/9.80665)
frac <- xo/yo*9.80665

range(diff, na.rm = TRUE)
max(abs(diff), na.rm = TRUE)

range(frac, na.rm = TRUE)
max(abs(frac - 1), na.rm = TRUE)

pdf(pdffilename, width = 10, height = 7)

iview(xo, legend=TRUE)
iview(yo, legend=TRUE)
iview(diff, legend=TRUE)
iview(frac, legend=TRUE, zlim = c(0.95, 1.05))

dev.off()
