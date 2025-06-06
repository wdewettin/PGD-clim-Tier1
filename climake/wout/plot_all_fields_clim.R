library(Rfa)

mstart <- 1
mend <- 12
climfilename_base <- "/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake/outputFiles/pgddomain-be70c_g1.nam__climdomain-be70c_g1.nam__cycle-cy43t2_clim-op8.01__v-1.37/Const.Clim."

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

    pdffilename <- paste(climfilename_base, mm, ".pdf", sep="")
    pdf(pdffilename)

    for (idx in seq_along(varname_list["name"][[1]])) {
        varname <- varname_list["name"][[1]][[idx]]
        y <- FAdec(x, varname)
        iview(y, legend = TRUE)
    }

    dev.off()
}