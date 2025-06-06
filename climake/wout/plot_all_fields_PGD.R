library(Rfa)

output_dir <- "/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake/outputFiles/pgddomain-be70c_g1.nam__climdomain-be70c_g1.nam__cycle-cy43t2_clim-op8.01__v-1.37"
output_file_name <- "PGD.fa"
pdffilename <- "PGD_be70c_g1.pdf"

pgdfilename <- paste(output_dir, output_file_name, sep="/")
x <- FAopen(pgdfilename)
varname_list <- x$list

pdf(pdffilename)

for (idx in seq_along(varname_list["name"][[1]])) {
    varname <- varname_list["name"][[1]][[idx]]
    y <- try(FAdec(x, varname), silent=TRUE)
    if (!inherits(y, "try-error")) {
        iview(y, legend=TRUE)
    }
}

dev.off()
