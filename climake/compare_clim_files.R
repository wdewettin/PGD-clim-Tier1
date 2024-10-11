library(Rfa)

# File locations

output_dir_1 <- "/dodrio/scratch/users/vsc45263/DYDOCASE/runs/long/level03/clim"
output_file_name_1 <- "Const.Clim."

output_dir_2 <- "/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake/outputFiles/pgddomain-BE40a_l.nam__climdomain-BE40a_l.nam__cycle-cy43t2_clim-op8.01__pgdnam-GCO__climnam-GCO__orotrunc-QUADRATIC__othertrunc-LINEAR__v-1.37"
output_file_name_2 <- "Const.Clim."

name_1 <- "l03_original"
name_2 <- "l03_climake"

pgd <- TRUE
pgd_file_name_1 <- "PGD.fa"
pgd_file_name_2 <- "PGD.fa"

if (pgd) {
    # Open pdf
    pdf_file_name <- paste("compare_pgd_", name_1, "_", name_2, ".pdf", sep="")
    pdf(pdf_file_name)

    pgd_file_1 <- file.path(output_dir_1, pgd_file_name_1)
    pgd_file_2 <- file.path(output_dir_2, pgd_file_name_2)

    xs_1 <- FAopen(pgd_file_1)
    xs_2 <- FAopen(pgd_file_2)

    sfx_fields_list <- xs_1$list["name"][[1]]

    for (field_name in sfx_fields_list) {
        y1 <- try(FAdec(xs_1, field_name), silent=TRUE)
        y2 <- try(FAdec(xs_2, field_name), silent=TRUE)
        if (!inherits(y1, "try-error") & !inherits(y2, "try-error")) {
            y <- y2 - y1
            if (max(abs(y)) > 0.0){
                print("---------------------------------------------")
                print(field_name)
                print(abs(y))
                iview(y, legend=TRUE)
            }
        }
    }

    dev.off()
}

# Open pdf
pdf_file_name <- paste("compare_clim_", name_1, "_", name_2, ".pdf", sep="")
pdf(pdf_file_name)

for (month in seq(1, 12)) {
    print(month)
    mm <- sprintf("%02d", month)
    output_file_1 <- file.path(output_dir_1, paste(output_file_name_1, mm, sep=""))
    output_file_2 <- file.path(output_dir_2, paste(output_file_name_2, mm, sep=""))

    # Plot fields
    xa_1 <- FAopen(output_file_1)
    xa_2 <- FAopen(output_file_2)

    alr_fields_list <- xa_1$list["name"][[1]]

    for (field_name in alr_fields_list) {
        y1 <- try(FAdec(xa_1, field_name), silent=TRUE)
        y2 <- try(FAdec(xa_2, field_name), silent=TRUE)
        if (!inherits(y1, "try-error") & !inherits(y2, "try-error")) {
            y <- y2 - y1
            if (max(abs(y)) > 0.0){
                print("---------------------------------------------")
                print(field_name)
                print(abs(y))
                iview(y, legend=TRUE)
            }
        }
    }

}

dev.off()