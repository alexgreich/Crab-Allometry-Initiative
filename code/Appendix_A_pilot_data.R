##Appendix A: Pilot data
#Alex Reich (alex.reich@alaska.gov)
# 11/27/26
# This code reads in and analyses the pilot data for the RKC allometry project. The pilot data was collected in 2017 and includes measurements of carapace length, carapace width, coxa length, and mera length.
# The objective is to determine if coxa width or merus length is a better indicator of crab legality
# Spencer had analyzed this data previously and determined that coxa width is better than merus length. Here I am formatting that analysis into a publishable format (and I dont know where her code is)

dat <- read.csv("data/RKC_allometry_pilot_data_filtered.csv")
