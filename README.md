# SEGY-Tools-in-Fortran
This repository contains several functions and soubroutines in Fortran 90 designed to work with SEG-Y files.

## Build
bash build.sh

## Command-line argument
```
./segytool input_path <option> -o output_path
-pE print EBCDIC header
-pB print Binary header
-pT print Trace header
-nt to set trace number, default=1
-h print help
-o output path, optional
```