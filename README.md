# Priceless Planet Coalition Data Analysis Pipeline

## What is this repository?
This repository houses the data processing/analysis scripts for dealing with PPC
monitoring data. The most important folder here is the 'Scripts' folder, which 
contains executable R scripts, each of which perform a different essential function.
Some of these scripts need to be executed in a certain order, because outputs of
one will become inputs to the next. Others can be run as standalone scripts. 

The other folders in the repository contain key information that enable the scripts
to run seamlessly. It is is important not to change the directory structures! Do
not rename folders or move them around. The scripts will all assume that the parent
folder in this repository is set as the working directory, and as such when they
search for necessary files, if they are renamed or moved, they will not be able to
run.


## How to download
If you are familiar with git, you can clone the repository in a directory of your 
choice. If not, the simplest way is to click on the green code button in the top
right corner of the repository page, and then 'Download Zip'. Here is a screenshot:

![Example Image](download_instructions_1.png)


