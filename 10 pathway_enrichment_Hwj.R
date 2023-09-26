## Function: enrichment analysis of genes 
# -----------------------------------------
# Author: Cao Luolong, caoluolong@outlook.com
# Date created: 09-04-2021
# Date updated: 2022.07.05
# Warning: 
# @FUDAN.

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.15")
# BiocManager::install("clusterProfiler")  #for enrichment
# BiocManager::install("topGO")  #for plot
# BiocManager::install("Rgraphviz")
# BiocManager::install("pathview") #for KEGG pathway
# BiocManager::install("rrvgo")
# BiocManager::install("org.Hs.eg.db") #for gene annotation.
# BiocManager::install("ReactomePA")
# devtools::install_github("jokergoo/simplifyEnrichment")
# library(BiocManager)
# library(clusterProfiler)
# # library(R.utils)
# # R.utils::setOption( "clusterProfiler.download.method",'auto')
# library(topGO)
# library(Rgraphviz)
# library(org.Hs.eg.db)
# library(ggplot2)
# library(dplyr)
# library(stringr)
# library(Cairo)
# library(msigdbr)
# library(rrvgo)
# library(simplifyEnrichment)
# library(GOSemSim)
# library(ReactomePA)
# library(DOSE)
# library(disgenet2r)
############## load packages ###########
rm(list=ls())
pkgs <- c("BiocManager","clusterProfiler","topGO","Rgraphviz","org.Hs.eg.db","ggplot2","stringr","Cairo",
          "msigdbr","rrvgo","simplifyEnrichment","GOSemSim","ReactomePA","DOSE","disgenet2r","readxl")
lapply(pkgs, library, character.only = T)

############## load gene list ###########
# dir.root <- "F:/work_dir/AHBAenrich/enrichment_out/"
dir.root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dir.root);kegmt<-read.gmt(paste0(dir.root,"/rawdata/msigdb.v7.5.1.symbols.gmt"))
# kegmt<-read.gmt(paste0(dir.root,"/ReactomePathways.gmt"))
# cell_markers <- vroom::vroom('http://bio-bigdata.hrbmu.edu.cn/CellMarker/download/Human_cell_markers.txt') %>%
#     tidyr::unite("cellMarker", tissueType, cancerType, cellName, sep=", ") %>% 
#     dplyr::select(cellMarker, geneID) %>%
#     dplyr::mutate(geneID = strsplit(geneID, ', '))
# curl::curl_download("http://117.50.127.228/CellMarker/CellMarker_download_files/file/Cell_marker_Human.xlsx", "Cell_marker_Human.xlsx")
cell_markers <- read_excel(paste0(dir.root,"/rawdata/Cell_marker_Human.xlsx")) %>% 
  tidyr::unite("marker", tissue_type, cancer_type, cell_name, sep=", ") %>%
  dplyr::select(marker, GeneID) %>%
  dplyr::mutate(geneID = strsplit(GeneID, ', '))
dir.work <- list.files(path = paste0(dir.root,"/Output/"),full.names = TRUE);dir.work <- dir.work[length(dir.work)]

file_name_pre <- '*_ROIs_slim_total.txt$'
sig_files <- list.files(path = dir.work, pattern = file_name_pre,full.names = TRUE);out_probe_join11 <- read.table(sig_files,sep = '\t',header = T);
###########  ORA enrich  ##############
ora.enrich.all <- NULL
# for(file_num in c(2,3,5,10,12,17,18,19,20)){
for(file_num in 2:ncol(out_probe_join11)){
  out_probe_join_ord <- out_probe_join11[order(out_probe_join11[,file_num],decreasing = TRUE),c(1,file_num)]
  gene_num = round(dim(out_probe_join11)[1]/10)
  # egenes.symble <- as.character(out_probe_join11$Symbol[which(out_probe_join11$PLS3Z<=-3)])#set a threshold of gene-Z score
  egenes.symble <- as.character(out_probe_join_ord$Symbol[(dim(out_probe_join11)[1]-gene_num):dim(out_probe_join11)[1]])#set a threshhood of proporation
  file_name <- paste0('/',colnames(out_probe_join11)[file_num],'_neg')
  if (FALSE){
    # egenes.symble <- as.character(out_probe_join11$Symbol[which(out_probe_join11$PLS3Z>=1.96)])#set a threshold of gene-Z score
    egenes.symble <- as.character(out_probe_join_ord$Symbol[1:gene_num])
    file_name <- paste0('/',colnames(out_probe_join11)[file_num],'_pos')
  }# these code won't run!!!!
  egenes.entrez <- na.omit(mapIds(x = org.Hs.eg.db, keys = egenes.symble, keytype = "SYMBOL", column="ENTREZID"))
  universe.entriez <- na.omit(mapIds(x = org.Hs.eg.db, keys = out_probe_join11$Symbol, keytype = "SYMBOL", column="ENTREZID"))
  ora.cell <- enricher(
    gene = egenes.entrez,
    pvalueCutoff = 0.05,
    pAdjustMethod = "BH",
    # universe = universe.entriez,universe = NULL,
    minGSSize = 1,
    maxGSSize = 500,
    qvalueCutoff = 0.2,
    gson = NULL,
    TERM2GENE=cell_markers,
    TERM2NAME = NA)
  ora.cell <- setReadable(ora.cell, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
  ora.go <- enrichGO(gene = egenes.entrez,
                     OrgDb = org.Hs.eg.db,
                     keyType = "ENTREZID",
                     ont = "ALL",
                     pvalueCutoff = 0.01,
                     pAdjustMethod = 'BH',
                     # universe = universe.entriez,universe = NULL,
                     qvalueCutoff = 0.2,
                     minGSSize = 10,
                     maxGSSize = 500,
                     readable = TRUE)
  ora.kegg <- enrichKEGG(
    gene = egenes.entrez,
    organism = "hsa",
    keyType = "kegg", # one of "kegg", 'ncbi-geneid', 'ncib-proteinid' and 'uniprot'
    pvalueCutoff = 0.05,
    qvalueCutoff = 0.2,
    pAdjustMethod = "BH",
    # universe = universe.entriez,universe = NULL,
    minGSSize = 10,
    maxGSSize = 500,
    use_internal_data = FALSE)
  ora.kegg <- setReadable(ora.kegg, OrgDb = org.Hs.eg.db, keyType="ENTREZID")


###########  enrich plot ##############
scoreMethod=2 # 1.R 2.Partial_least_square  3.combine_Z_fisher  4.add_Z	 5.add_Z_PPI   6.max_Z  7.max_Z_PPI
richSet=1 # 1.go,2.do,3.wp,4.dgn,5.ncg,6.kegg,7.reactome,8.gsea,9.cell-type
write.table(ora.enrich.all[[scoreMethod]][[richSet]]@result,file = paste0(dir.work,'/PLS_GO.txt'),col.names = T,row.names = F,sep = '\t',quote = F)

richSet=6 # 1.go,2.do,3.wp,4.dgn,5.ncg,6.kegg,7.reactome,8.gsea,9.cell-type
write.table(ora.enrich.all[[scoreMethod]][[richSet]]@result,file = paste0(dir.work,'/PLS_Kegg.txt'),col.names = T,row.names = F,sep = '\t',quote = F)

richSet=9 # 1.go,2.do,3.wp,4.dgn,5.ncg,6.kegg,7.reactome,8.gsea,9.cell-type
write.table(ora.enrich.all[[scoreMethod]][[richSet]]@result,file = paste0(dir.work,'/PLS_CellType.txt'),col.names = T,row.names = F,sep = '\t',quote = F)






