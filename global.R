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
require(GoViewer)
# require(plyr)
# require(rTensor)
# require(reshape)

# run this to get transcript data for browser

# mart_1 = useMart("ensembl", dataset="hsapiens_gene_ensembl")
# t_list = getBM(attributes=c("chromosome_name","exon_chrom_start","exon_chrom_end","ensembl_transcript_id","ensembl_gene_id","strand","external_gene_name"), filters=list(chromosome_name=c(as.character(1:22), "X", "Y"), with_protein_id=TRUE), mart=mart_1)
# t_list = arrange(t_list, chromosome_name, exon_chrom_start, exon_chrom_end)
# save(t_list, file="data/t_list.RData")

# global objects

load("data/dat_all.RData")
load("data/gene_list_all.RData") # granges object (column annotation)
load("data/t_list.RData") # for sushi plot
load("data/msig_go_bp.RData") # gene sets for enrichment
load("data/roi_reg_df.RData") # regulatory regions for browser
data_gsk = read_excel(system.file("extdata", "data_gsk.xlsx", package="epiChoose"))
data_gsk$Bigwig = str_replace(data_gsk$Bigwig, "/GWD/bioinfo/projects/RD-Epigenetics-NetworkData/", "http://ftp.ebi.ac.uk/pub/databases/opentargets/")

cx = reactiveValues(full.data=list(), selections=list())
data(P2.source)
cx$full.data = P2.source

