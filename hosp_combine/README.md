# AHCA Hospital Data Aggregation Pipeline

Uses AHCA hospital data inputs (.xlsx) and outputs one single file that aggregates all data (.csv) with consistent column names (if possible) and adds both date/time columns and consistent FIPS codes. 

## Pre-requesites

Input files should be named following the following format: ***_***_***_mm.dd.yyyy_time 

## Approach

1. Convert input files from .xlsx to .csv and extract all available sheets.
	* <Tom's ssconvert script>
1. Remove any PHI disclaimers or other unnecessary headers.
	* rmv_PHI.sh <Tom might have updated version of this script that accounts for files with and without the PHI disclaimer>
	* Removes first line of all files with the PHI disclaimer
	* Store all "cleaned" input data in separate directory
1. Aggregate data.
	* Requires "col_syn.csv" and fipsCodes.csv" files
	* Create output directory
	* dataAggregationRename.R (inputs files from stored directory and saves aggreagated CSV file to output directory)
