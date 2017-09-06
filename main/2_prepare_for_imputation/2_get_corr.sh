#!/bin/bash
for decade in 1 2 3 4 5
do
    JOB=`msub - << EOJ
    

#MSUB -A p30171
#MSUB -q short
#MSUB -l walltime=04:00:00
#MSUB -M dtb@u.northwestern.edu
#MSUB -j oe
#MSUB -N 2_get_corr
#MSUB -l mem=20gb
#MSUB -l nodes=1:ppn=1

# Set working directory 
cd ~/selfreliance

# Load R
module load R/3.3.1

# Run script
Rscript main/2_prepare_for_imputation/2_get_corr_r2_cramers_v_by_decade_array.R ${decade}


EOJ
`

echo "JobID = ${JOB} for parameter ${decade} submitted on `date`"

done

exit