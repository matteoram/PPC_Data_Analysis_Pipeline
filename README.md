#PPC Contents and background

Sow trees, reap data


What is in this file?
Within the PPC folder there are 4 additional folders and 2 additional files:
1) Final_Data- Are the Key Indicators that are produced after running all of the R scripts in this document.These data can be used later for further analysis

2) Misc_files- These are often old miscellaneous files that I haven’t deleted yet, but should probably delete. One file that shouldn't be delete is the R markdown used to 

3) R_scripts- This is the folder that contains all of the R scripts that are referenced in this script and are used to generate the Final_Data.

4) Raw_Data- This is the folder that contains all of the R data that is downloaded from Kobotool, and is the repository for data in intermediate steps before producing the final data.

5) PPC_Data.pdf - sort of like Master R script. This file is used to the run all of the R codes, which generate all of the intermediate data, and ultimately leads to the the Final_Data of Key Indicators.

6) a README file- A file describing the contents of the folder. . .

7) Tree Restoration Monitoring Framework - Field Test Edition 3.pdf - this file provides an overview of the methods used to monitor and measure the trees in the PPC project.

NOTE 1: All of the files should be present once downloaded from CI Catalyst Teams' github, and are necessary for the scripts to run correctly. Please read the PPC_Data.pdf to run the R codes and generate the the final data. It is recommended that 

NOTE 2: If any script is run multiple times, several different copies of the same files will be stored in the "Raw_Data" folder, but with different dates. Before running each script, all files except the most recent "Taxonomy_updates_XXXX- XX-XX.csv" file should be removed the "Old_data" folder which is within the "Misc_files" folder.

What is the purpose of this file?
This file acts as a repository for the PPC plot inventory data. Assuming no glitches, this script should produce all of the key indicators for the PPC data. This 'key indicator' data can then be used for other purposes or analyses by end-users 

Where can this file be found?
https://github.com/Catalytic-Science-for-NCS/PPC