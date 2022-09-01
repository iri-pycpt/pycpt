# CPT Makefile

# version info
MAJOR_VERSION=17
MINOR_VERSION=7
PATCH_VERSION=4
VERSION=$(MAJOR_VERSION).$(MINOR_VERSION).$(PATCH_VERSION)

# User should change the "FC" to reflect the fortran compiler available on your machine.  
# CPT was tested with the following fortran compilers: gfortran, ifort, nagfor, pgf90, pgf95 
FC=x86_64-conda_cos6-linux-gnu-gfortran

# install target installs the binaries to INSTALL_DIR
INSTALL_DIR ?= .
INSTALL_DIR := $(abspath $(INSTALL_DIR))

# debug version
DEBUG ?= 0

# double precision version
DP ?= 1

# gui version
GUI ?= 0

#-----------------------------

CPPDEFS := -DDP=$(DP)
ifeq ($(GUI),1)
   CPPDEFS += -DGUI=1
endif
ifeq ($(DEBUG),1)
   CPPDEFS += -DDEBUG=1
endif

LAPACK_DIR=lapack/lapack
LAPACK_LIB=lapack
BLAS_LIB=refblas


ifeq ($(FC),x86_64-conda_cos6-linux-gnu-gfortran)
   CPPDEFS += -DGFORTRAN 
   ifeq ($(DEBUG),1)
      FFLAGS= -g -Wall -fbounds-check $(CPPDEFS) -std=f2008 -fall-intrinsics
   else
      FFLAGS= -O $(CPPDEFS) -std=f2008 -fall-intrinsics 
   endif
endif

ifeq ($(FC),ifort)
   CPPDEFS += -DIFORT
   ifeq ($(DEBUG),1)
      FFLAGS = $(CPPDEFS) -fpp -fpe0 -fPIC -free -check -heap-arrays -debug all -warn all -g -std08 -assume minus0 -Tf
   else
      FFLAGS = $(CPPDEFS) -fpe0 -fPIC -free -fpp -std08 -assume minus0 -Tf
   endif
endif

ifeq ($(FC),nagfor)
   CPPDEFS += -DNAGFOR
   ifeq ($(DEBUG),1)
      FFLAGS=-C -PIC $(CPPDEFS) -g -gline -f2008 -fall-intrinsics
   else
      FFLAGS=-O -PIC $(CPPDEFS) -f2008 -fall-intrinsics
   endif
endif

ifeq ($(FC),pgf90)
   CPPDEFS += -DPGI
   FFLAGS=-O -Kieee $(CPPDEFS)
endif

ifeq ($(FC),pgf95)
   CPPDEFS += -DPGI
   FFLAGS = -O -Kieee $(CPPDEFS)
endif

SRCS := $(wildcard *.F95) 
SRCS := $(filter-out axes.F95, $(SRCS))
OBJS := $(SRCS:.F95=.o)

LIBS= -L$(LAPACK_DIR) -l$(LAPACK_LIB) -l$(BLAS_LIB) -lgfortran

build: CPT.x

$(LAPACK_DIR)/lib$(LAPACK_LIB).a $(LAPACK_DIR)/lib$(BLAS_LIB).a: 
	make -C $(LAPACK_DIR)

CPT.x: $(OBJS) $(LAPACK_DIR)/lib$(LAPACK_LIB).a $(LAPACK_DIR)/lib$(BLAS_LIB).a
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIBS)

testDGESDD: $(OBJS) testDGESDD.o
	$(FC) $(FFLAGS) -o $@ testDGESDD.o $(OBJS) $(LIBS)


.SUFFIXES: .mod .o .F95
%.o: %.mod # turn off default Modula rule
%.mod %.o: %.F95
	$(FC) -c $(FFLAGS) $<

srcdeps: $(SRCS)
	touch $@
	echo $(CPPDEFS)
	./sfmakedepend -x '$(FC) $(CPPDEFS) -E -o /dev/stdout' -n 32000 -f $@ $(SRCS)
	rm $@.old

-include srcdeps


tests:
	make -C test all

RELEASE_DIR := CPT/$(VERSION)
TARBALL := CPT.$(VERSION).tar.gz
release: distclean
	make srcdeps
	mkdir -p $(RELEASE_DIR)/lapack/lapack
	mkdir -p $(RELEASE_DIR)/data
	mkdir -p $(RELEASE_DIR)/help
	mkdir -p $(RELEASE_DIR)/output_module
	tar cf - -C $(LAPACK_DIR) --exclude=.git . | tar xf - -C $(RELEASE_DIR)/lapack/lapack
	tar cf - -C .. --exclude=.git output_module LICENSE | tar xf - -C $(RELEASE_DIR)
	tar cf - --exclude=.git data help cpt.ini sfmakedepend srcdeps *.F95 Makefile | tar xf - -C $(RELEASE_DIR)
	tar czf $(TARBALL) $(RELEASE_DIR)

install: build
	mkdir -p $(INSTALL_DIR)/bin
	tar cf - -C . --exclude=.git cpt.ini data/labels.txt data/download_IRIDL.txt CPT.x | tar xf - -C $(INSTALL_DIR)/bin
	#tar cf - -C .. --exclude=.git | tar xf - -C $(INSTALL_DIR)/bin

distclean cleanall deepclean: clean
	make -C lapack/lapack cleancpt
	rm -rf $(RELEASE_DIR) $(TARBALL) srcdeps

clean:
	rm -f *.o *.mod CPT.x
# DO NOT DELETE THIS LINE - used by make depend
