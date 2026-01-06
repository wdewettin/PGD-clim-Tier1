FC=mpiifort
OPTS=-convert big_endian -assume byterecl -align array64byte,all -fpic -qopenmp -qopenmp-threadprivate compat -fp-model strict -ftz
PACKDIR=/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc45302_kwinten/root/46t1_bf.07.INTEL.x/
INCS=$(shell cat $(PACKDIR)/src/.incpath.local $(PACKDIR)/src/.modpath.local)
LIBS=-L$(PACKDIR)/lib -lifsaux.local -L/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc45302_kwinten/software/iimpi2023a/lib64/ -leccodes_f90 -leccodes \
	-L/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc45302_kwinten/software/iimpi2023a/auxlibs/lib/ -lgribex

all: replace_orography

replace_orography: replace_orography.F90
	$(FC) $(INCS) $(OPTS) -o replace_orography replace_orography.F90 $(LIBS)

clean:
	rm replace_orography
