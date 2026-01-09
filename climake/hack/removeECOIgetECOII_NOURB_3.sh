#>>> overwrite ECOCLIMAP I files by ECOCLIMAP II WITH cities (in case of no cities: select the ECOCLIMAP II in your own folder without cities!)

# First change permission, to be able to change the files
chmod 644 ecoclimap.dir ecoclimap.hdr # files worden verwijderbaar en overschrijfbaar

# Remove the existing ecoclimap
rm ecoclimap.dir
rm ecoclimap.hdr

# Copy the correct ecoclimap to the folder, but they need to get the same name as the original files
cp /dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc46613_amber/Project_CitiesandPrecipitation/climate_simulations/runs/ECOCLIMAP/ECOCLIMAP_nocities_domain_dominant_diffsubd/ECOCLIMAP_adjusted_3degrees.dir ecoclimap.dir
cp /dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/PGD-clim-data/data/ECOCLIMAP_II_EUROP_V2.5.hdr ecoclimap.hdr
