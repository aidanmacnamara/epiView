require(epiChoose)
require(shiny)
require(tidyverse)
require(ggrepel)
require(reshape2)
require(biomaRt)
require(rtracklayer)
require(Sushi)
require(shinyBS)
require(stringr)

# set working directory to source location

load("dat.RData") # main data object
load("gene_list_all.RData") # granges object (column annotation)
load("t_list.RData") # for sushi plot
load("msig_go_bp.RData") # gene sets for enrichment

cells = rownames(dat[[1]]$res)
genes = colnames(dat[[1]]$res)

runApp()

