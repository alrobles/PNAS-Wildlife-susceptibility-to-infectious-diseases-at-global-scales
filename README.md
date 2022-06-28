# Wildlife susceptibility to infectious diseases at global scale

Instructions to run the code used to generate results in the manuscript “Wildlife susceptibility to infectious diseases at global scales” (PNAS)

All analyses were run in a computer with the following specifications:

CPU:  Intel(R) Core(TM) i7-6700 CPU @ 3.40GH with 64GB RAM

File description and steps:

Install this repository 
- Download scripts from the GitHub repository (https://github.com/alrobles/PNAS-Wildlife-susceptibility-to-infectious-diseases-at-global-scale
/scripts)
- Download all files with raw data (i.e., distribution files for birds and mammals and PCs from bioclimatic layers) 
from the following Google Drive directory (https://drive.google.com/drive/folders/1XhKyYw2u5RbnKF0d4BXw0C3DARiICf6r?usp=sharing). 

- You should copy these to /data-raw folder from the files previously downloaded from the GitHub repository in the first bullet.

- Script 1 from GitHub repository estimates the power law distribution of incidence of Plasmodium in birds, West Nile Virus in birds, and coronaviruses in bats. The raw incidences of these three host-pathogen systems are in three separate files (avian malaria = MalAvi_7133898_coordinatesOK_ALN_Angel.xlsx; West Nile Virus = Tolsa2018Birds-WNV.xls; coronavirus-bats = bat_coronavirus_DB.csv; please, see details on sources of these files and databases in the main text of the manuscript). This script also conducts a significant test that the distribution of incidences follow a power-law distribution and generates a plot for each case.
- Script 2-3 from GitHub repository calculate the geographic distance between centroids of geographic ranges of birds and mammals, respectively.
- Script 4-5 from GitHub repository calculate the environmental distance between for birds and mammals, respectively. It first masks each of the three bioclimatic PCs with the geographic range of each species, and then for the multivariate distribution it calculates the maximum of each bioclimatic PC and then calculates the distance between the maxima for each pair of species.
- Script 6 generates a consensus phylogenetic tree from 1000 trees taken from birdtree.org (i.e., output_bird.nex file in the raw data folder), and calculates the phylogenetic distance between pairs of species.
- Script 7 from the GitHub repository uses the consensus phylogenetic tree for mammals taken from https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.2443, and calculates the phylogenetic distances between pairs of species.
- Script 8 from the GitHub repository is used to generate centered and scaled environmental, geographic, and phylogenetic distances dataset for birds, mammals, and bats. Note however that if you would like to skip this process of generating the distances, the ‘ecointeraction’ package comes with those distances already calculated and ready for use (i.e., equivalent to processing in scripts 2-7). 
- Scripts 9-11 from GitHub repository would be used to generate 1000 random forest models for each assemblage separately. Script 9 would generate simulations for avian malaria (bird_plasmodium_1000_sims.rds), script 10 would generate simulations for West Nile Virus (bird_wnv_1000_sims.rds), and script 11 would generate simulations for coronavirus-bats (bat_coronavirus_1000_sims.rds). Note that if you would like to review or use the simulations already produced for the results presented in the manuscript, you can use the .rds files.
- Script 12 from the GitHub repository generates the top-ten incidence and susceptibility species per assemblage. These are the results that come as tables in the main text of the manuscript.
- Script 13 from the GitHub repository generates the environmental envelopes from the first top-six species both for incidence and susceptibility and plots them.
- Scripts 14-16 from the GitHub repository generate for each host-pathogen case separately: (a) richness maps of susceptible species; (b) extract, filter, and clean the occurrences of pathogen incidences from raw/field observations (avian malaria = MalAvi_7133898_coordinatesOK_ALN_Angel.xlsx; West Nile Virus = Tolsa2018Birds-WNV.xls; coronavirus-bats = bat_coronavirus_DB.csv; please, see details on sources of these files and databases in the main text of the manuscript); (c) conducts a likelihood ratio test according to a homogeneous Poisson process, where point density is due a completely random process (H0), and compares it against an inhomogeneous Poisson process, where point density is explained by susceptible species richness (HA); (d) density estimation of occurrences of pathogens given richness maps of susceptible species.
- Scripts 17-19 from the GitHub repository conduct the reconstruction of the pathogen susceptibility as an ancestral character on top of the hosts phylogeny for each host-pathogen case separately.
