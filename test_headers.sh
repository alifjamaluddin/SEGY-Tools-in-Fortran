#!/bin/sh

gfortran -O3 src/word_definitions_mod.f90 \
	 src/read_segy_tools_mod.f90 \
	 src/ebcdic_reel_header_mod.f90 \
	 src/binary_reel_header_mod.f90 \
	 src/trace_reel_header_mod.f90 \
	 src/traces_mod.f90 \
	 test_headers.f90 -o test_headers

rm *.mod
