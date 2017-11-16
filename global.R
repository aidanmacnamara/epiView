require(epiChoose)
require(shiny)
require(tidyverse)
require(ggrepel)
require(reshape2)
require(biomaRt)
require(rtracklayer)
require(Sushi)
require(stringr)
require(ggplot2)


# global objects

load("data/dat.RData") # main data object
load("data/gene_list_all.RData") # granges object (column annotation)
load("data/t_list.RData") # for sushi plot
load("data/msig_go_bp.RData") # gene sets for enrichment

