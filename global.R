require(epiChoose)
require(shiny)
require(tidyverse)
require(ggrepel)
require(reshape2)
require(biomaRt)
require(rtracklayer)
require(Sushi)
require(stringr)
require(readxl)
require(shinyBS)
require(DT)
# require(plyr)
# require(rTensor)
# require(reshape)


# global objects

# load("data/dat_old.RData") # main data object
load("data/dat_all.RData")
load("data/gene_list_all.RData") # granges object (column annotation)
load("data/t_list.RData") # for sushi plot
load("data/msig_go_bp.RData") # gene sets for enrichment
data_gsk = read_excel(system.file("extdata", "data_gsk.xlsx", package="epiChoose"))

