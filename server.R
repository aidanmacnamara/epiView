
# single_labels = rownames(dat[[1]]$res)
# group_labels = "GSK"
# pca_data = prep_for_plot(dat, annot_1=group_labels, annot_2=single_labels, marks=names(dat), plot_type="mds")

shinyServer(function(input, output, session) {
  
  
  # tensorTab("TV", input, output, ui=F)
  
  
  dat = reactive({
    
    dat_out = vector("list", 6)
    names(dat_out) = names(dat_all[[1]])
    
    if(input$h3k27ac_summ == "max") {dat_out$H3K27ac = dat_all$max$H3K27ac}
    else if(input$h3k27ac_summ == "tss") {dat_out$H3K27ac = dat_all$tss$H3K27ac}
    else if(input$h3k27ac_summ == "sum") {dat_out$H3K27ac = dat_all$sum$H3K27ac}
    else if(input$h3k27ac_summ == "closest") {dat_out$H3K27ac = dat_all$closest$H3K27ac}
    
    if(input$h3k4me3_summ == "max") {dat_out$H3K4me3 = dat_all$max$H3K4me3}
    else if(input$h3k4me3_summ == "tss") {dat_out$H3K4me3 = dat_all$tss$H3K4me3}
    else if(input$h3k4me3_summ == "sum") {dat_out$H3K4me3 = dat_all$sum$H3K4me3}
    else if(input$h3k4me3_summ == "closest") {dat_out$H3K4me3 = dat_all$closest$H3K4me3}
    
    if(input$h3k27me3_summ == "max") {dat_out$H3K27me3 = dat_all$max$H3K27me3}
    else if(input$h3k27me3_summ == "tss") {dat_out$H3K27me3 = dat_all$tss$H3K27me3}
    else if(input$h3k27me3_summ == "sum") {dat_out$H3K27me3 = dat_all$sum$H3K27me3}
    else if(input$h3k27me3_summ == "closest") {dat_out$H3K27me3 = dat_all$closest$H3K27me3}
    
    if(input$ctcf_summ == "max") {dat_out$CTCF = dat_all$max$CTCF}
    else if(input$ctcf_summ == "tss") {dat_out$CTCF = dat_all$tss$CTCF}
    else if(input$ctcf_summ == "sum") {dat_out$CTCF = dat_all$sum$CTCF}
    else if(input$ctcf_summ == "closest") {dat_out$CTCF = dat_all$closest$CTCF}
    
    if(input$atac_summ == "max") {dat_out$ATAC   = dat_all$max$ATAC}
    else if(input$atac_summ == "tss") {dat_out$ATAC = dat_all$tss$ATAC}
    else if(input$atac_summ == "sum") {dat_out$ATAC = dat_all$sum$ATAC}
    else if(input$atac_summ == "closest") {dat_out$ATAC = dat_all$closest$ATAC}
    
    # dat_out$H3K4me3 = dat_all$tss$H3K4me3
    # dat_out$H3K27me3 = dat_all$max$H3K27me3
    # dat_out$ATAC = dat_all$closest$ATAC
    # dat_out$CTCF = dat_all$closest$CTCF
    dat_out$RNA = dat_all$tss$RNA
    
    return(dat_out)
    
  })
  
  
  global_choice = eventReactive(input$do_global, {
    validate(
      need(input$project_choice != "", 'Please select at least one project to view.')
    )
    
    tmp_list = list()
    
    tmp = dat()
    proj_ix = which(tmp[[1]]$annot$Project %in% input$project_choice)
    
    if(!"All" %in% input$project_choice) {
      for(i in 1:length(tmp)) {
        tmp[[i]]$res = tmp[[i]]$res[proj_ix,] 
        tmp[[i]]$annot = tmp[[i]]$annot[proj_ix,]
      }
    }
    
    group_labels = tmp[[1]]$annot$Project
    single_labels = rownames(tmp[[1]]$res)
    
    if(input$cell_type_select!="") { # if only certain blueprint cell types are wanted
      
      cell_types = unlist(str_split(input$cell_type_select, ",")) # split the user input by ,
      cell_types = trimws(cell_types) # remove leading/trailing whitespace
      c_l = !grepl(paste(cell_types, collapse="|"), rownames(tmp[[1]]$res), ignore.case=TRUE) # get the logic vector for the cell types
      c_ix = grep(paste(cell_types, collapse="|"), rownames(tmp[[1]]$res), ignore.case=TRUE) # get the index vector for the cell types
      
      for(i in 1:length(tmp)) { # slice all data types to remove non-relevant blueprint data
        tmp[[i]]$res = tmp[[i]]$res[!(group_labels=="BLUEPRINT" & c_l),]
      }
      
      # single_labels[c_ix] = str_extract(tolower(single_labels[c_ix]), paste(tolower(cell_types), collapse="|")) # change label name
      single_labels = single_labels[!(group_labels=="BLUEPRINT" & c_l)] # slice single labels
      group_labels = group_labels[!(group_labels=="BLUEPRINT" & c_l)] # slice group labels
      
    }
    
    tmp_list$tmp = tmp
    tmp_list$mds_type = input$mds_type
    tmp_list$single_labels = single_labels
    tmp_list$group_labels = group_labels
    
    return(tmp_list)
    
  })
  
  
  global_plot <- reactive({
    
    # a_gc <<- global_choice() # make global for david's code
    # save(a_gc, file="gc.rda")
    
    pca_data = prep_for_plot(global_choice()$tmp, annot_1=global_choice()$group_labels, annot_2=global_choice()$single_labels, marks=names(global_choice()$tmp), plot_type=global_choice()$mds_type)
    pca_data$annot_1 = paste("Project", pca_data$annot_1)
    return(pca_data)
  })
  
  
  output$global_view <- renderPlot({
    
    if(input$data_type=="All") {
      p_1 = ggplot(global_plot(), aes(x=x, y=y, color=factor(annot_1))) + geom_point(size=3, shape=17) + theme_thesis(20) + facet_wrap(~mark, nrow=2, scales="free")
    } else {
      p_1 = ggplot(filter(global_plot(), mark==input$data_type), aes(x=x, y=y, color=factor(annot_1))) + geom_point(size=3, shape=17) + theme_thesis(20) 
    }
    
    if(input$show_sample_labels) {
      return(p_1 + geom_text_repel(aes(label=annot_2), fontface="bold", size=input$label.size.global, force=0.5, show.legend=FALSE))
    } else {
      return(p_1)
    }
    
  })
  
  
  local_choice <- eventReactive(input$do_local, {
    validate(
      need(
        length(input$cell_target_choice) &
          length(input$cell_candidate_choice), 'Please select cell targets and candidates.')
    )
    
    tmp = dat()
    
    gene_upload <- NULL
    if(!is.null(input$user_choice$datapath)) {
      if(str_detect(input$user_choice$datapath, "\\.xlsx$") | str_detect(input$user_choice$datapath, "\\.xls$")) {
        if(input$header) {
          gene_upload = as.character(unlist(read_excel(input$user_choice$datapath)))
        } else {
          gene_upload = as.character(unlist(read_excel(input$user_choice$datapath, col_names=FALSE)))
        }  
      } else {
        if(input$header) {
          gene_upload = as.character(unlist(read_csv(input$user_choice$datapath)))
        } else {
          gene_upload = as.character(unlist(read_csv(input$user_choice$datapath, col_names=FALSE)))
        }
      }
    }
    
    if(length(input$go_choice) | length(input$gene_choice) | length(gene_upload)) {
      genes = unique(c(unlist(msig_go_bp[names(msig_go_bp) %in% input$go_choice]), input$gene_choice, gene_upload))
      col_ix = which(colnames(tmp[[1]]$res) %in% genes)
    } 
    else {
      col_ix = 1:dim(tmp[[1]]$res)[2]
    }
    
    row_ix = which(rownames(tmp[[1]]$res) %in% c(input$cell_candidate_choice, input$cell_target_choice))
    
    # slice matrices if necessary
    for(j in 1:length(tmp)) { # each data type
      tmp[[j]]$res = tmp[[j]]$res[row_ix,col_ix,drop=FALSE]
    }
    
    return(tmp)
    
  })
  
  
  local_plot <- eventReactive(input$do_local, {
    
    c_ix = match(input$cell_candidate_choice, rownames(local_choice()[[1]]$res))
    t_ix = match(input$cell_target_choice, rownames(local_choice()[[1]]$res))
    
    res = dist_mat(local_choice(), comp_ix=list(c_ix, t_ix), labels=rownames(local_choice()[[1]]$res), label_points=FALSE)
    return(res)
  })
  
  
  scatter_select <- eventReactive(input$do_local, {
    
    x_axis = input$scatter_x_axis
    y_axis = input$scatter_y_axis
    
    # if x_axis or y_axis is null, plot cell types vs. cell types for single data type
    
    all_ix = c(
      match(input$cell_target_choice, rownames(local_choice()[[1]]$res)),
      match(input$cell_candidate_choice, rownames(local_choice()[[1]]$res))
    )
    
    if(y_axis=="Compare across cells with 'X-axis' datatype") {
      
      to_plot = melt(as.matrix(local_choice()[[which(names(local_choice())==x_axis)]]$res[all_ix,]))
      
      u_ct = unique(to_plot[,1])
      combs = combn(1:length(u_ct), 2)
      to_plot = do.call("rbind", lapply(as.list(1:dim(combs)[2]), get_comb, to_plot, u_ct, combs))
      names(to_plot) = c("Comparison","Gene","X","Y")
      
    } else {
      
      to_plot = cbind(
        melt(as.matrix(local_choice()[[which(names(local_choice())==x_axis)]]$res[all_ix,])),
        as.numeric(as.matrix(local_choice()[[which(names(local_choice())==y_axis)]]$res[all_ix,]))
      )
      names(to_plot) = c("Cell Type", "Gene", x_axis, y_axis)
      
    }
    
    return(to_plot)
    
  })
  
  
  scatter_ranges <- reactiveValues(x=NULL, y=NULL, facet=NULL)
  
  
  observeEvent(input$scatter_dblclick, {
    
    brush <- input$scatter_brush
    
    if (!is.null(brush)) {
      scatter_ranges$x <- c(brush$xmin, brush$xmax)
      scatter_ranges$y <- c(brush$ymin, brush$ymax)
      scatter_ranges$facet <- brush$panelvar1
      
    } else {
      scatter_ranges$x <- NULL
      scatter_ranges$y <- NULL
      scatter_ranges$facet <- NULL
    }
    
  })
  
  
  output$local_view <- renderPlot({
    
    if(input$what_view=="correlation") {
      return(local_plot()$plots[[1]] + geom_text_repel(aes(label=Cell), fontface="bold", size=input$label.size.local, force=0.5, show.legend=FALSE) + theme_thesis(input$axis.label.size))
    }
    
    if(input$what_view=="barchart") {
      
      if(!is.null(scatter_ranges$facet)) {
        
        x = local_choice()[[which(names(local_choice())==scatter_ranges$facet)]]$res
        y = melt(as.matrix(x))
        names(y) = c("Cell Line", "Gene", "Score")
        return(ggplot(y, aes(x=Gene, y=Score)) + geom_bar(aes(fill=`Cell Line`), position="dodge", stat="identity") + theme_thesis(input$axis.label.size))
        
      } else {
        return(local_plot()$plots[[2]] + theme_thesis(input$axis.label.size))
      }
      
    }
    
    if(input$what_view=="boxplot") {
      return(local_plot()$plots[[3]] + theme_thesis(input$axis.label.size))
    }
    
    if(input$what_view=="scatter") {
      
      to_plot = scatter_select()
      
      if(!is.null(scatter_ranges$x)) {
        my_genes = to_plot$Gene[(to_plot[,3] >= scatter_ranges$x[1] & to_plot[,3] <= scatter_ranges$x[2]) & (to_plot[,4] >= scatter_ranges$y[1] & to_plot[,4] <= scatter_ranges$y[2]) & (to_plot[,1]==scatter_ranges$facet)]
        to_plot = dplyr::filter(to_plot, Gene %in% my_genes)
      }
      
      if(names(to_plot)[1]=="Cell Type") {
        
        s_1 = ggplot(to_plot, aes_string(x=names(to_plot)[3], y=names(to_plot)[4])) + geom_point(size=5, shape=17, color="red", alpha=0.3) + theme_thesis(angle_45=FALSE) + facet_wrap(~`Cell Type`, ncol=2)
        
        if(input$show_gene_labels) {
          return(s_1 + geom_text_repel(aes(label=Gene), fontface="bold", size=input$label.size.local, force=0.5))
        } else {
          return(s_1)
        }
        
      } else {
        
        s_1 = ggplot(to_plot, aes(x=X, y=Y)) + geom_point(size=5, shape=17, color="red", alpha=0.3) + theme_thesis(base_size=input$axis.label.size, angle_45=FALSE) + facet_wrap(~Comparison, ncol=2) + xlab("") + ylab("") + geom_abline(slope=1, color="red", alpha=0.5)
        
        if(input$show_gene_labels) {
          return(s_1 + geom_text_repel(aes(label=Gene), fontface="bold", size=input$label.size.local, force=0.5))
        } else {
          return(s_1)
        }
      }
    }
    
  })
  
  
  selected_scatter <- reactive({ # gets selected genes from scatter plot
    brushedPoints(scatter_select(), input$scatter_brush)
  })
  
  
  enrichment_results_explore <- eventReactive(input$run_local_enrichment, {
    
    if(dim(selected_scatter())[1]) {
      
      my_genes = selected_scatter()$Gene
      res = enrichment_test(genes=my_genes, gene_sets=msig_go_bp, genes_ref=unique(unlist(msig_go_bp)), verbose=FALSE)
      
      # gsea
      # see https://www.biostars.org/p/113680/
      # selection_ranked = dplyr::arrange(data.frame(gene=selected_data()$Gene, score=unlist(selected_data()[,2])*unlist(selected_data()[,3])), desc(score))
      # global_ranked = dplyr::arrange(data.frame(gene=mode_choice()$Gene, score=unlist(mode_choice()[,2])*unlist(mode_choice()[,3])), desc(score))
      
      return(res)
      
    } else {
      
      return(NULL)
      
    }
  })
  
  
  output$enrichment_explore <- renderPrint({
    
    if(!is.null(enrichment_results_explore())) {
      return(enrichment_results_explore())
    } else {
      return("Enrichment not run ...")
    }
    
  })
  
  
  output$brush_info_scatter <- renderPrint({ # displays selected genes from scatter plot
    if(dim(selected_scatter())[1]) {
      return(tbl_df(selected_scatter()$Gene[!is.na(selected_scatter()$Gene)]))
    } else {
      return("No genes selected ...")
    }
  })
  
  
  output$plot_brushinfo <- renderPrint({ # for debugging, shows output from brush
    str(input$scatter_brush)
  })
  
  
  model_choice <- eventReactive(input$do_model, {
    
    c_ix = which(rownames(dat()[[1]]$res) %in% input$candidate_choice)
    a_ix = which(rownames(dat()[[1]]$res) %in% input$alt_choice)
    t_ix = which(rownames(dat()[[1]]$res) %in% input$target_choice)
    
    if(
      length(c_ix) &
      length(a_ix) &
      length(t_ix) &
      length(input$x_axis) &
      length(input$y_axis)
    )  {
      
      my_df = spotfire_view(dat(), x_axis=input$x_axis, y_axis=input$y_axis, comp_ix=list(c_ix, a_ix, t_ix))
      rownames(my_df) = NULL
      my_df = tbl_df(my_df)
    }
    
    return(my_df)
    
  })
  
  
  output$plot_model_choice <- renderPlot({
    
    dat_out = model_choice()
    dat_out$color = 1
    dat_out$color = factor(dat_out$color)
    dat_out = tbl_df(dat_out)
    
    ggplot(dat_out, aes_string(names(dat_out)[2], names(dat_out)[3])) + geom_point(size=2, aes(color=color, alpha=color)) + theme_thesis(angle_45=FALSE) + scale_color_discrete(guide=FALSE) + scale_alpha_manual(guide=FALSE, values=c(0.3,0.8)) + annotate("rect", xmin=Inf, xmax=0, ymin=Inf, ymax=0, fill="green", alpha=0.1) 
    
  })
  
  
  selected_data <- reactive({
    brushedPoints(model_choice(), input$plot_model_choice_brush)
  })
  
  
  output$brush_info <- renderPrint({
    if(dim(selected_data())[1]) {
      return(selected_data())
    } else {
      return("No genes selected ...")
    }
  })
  
  
  output$download_table <- downloadHandler(
    filename = function() { 
      paste('selected_rows', '.csv', sep='') 
    },
    content = function(file) {
      write_csv(selected_data(), file)
    }
  )
  
  
  enrichment_results <- reactive({
    
    if(dim(selected_data())[1]) {
      
      my_genes = selected_data()$Gene
      res = enrichment_test(genes=my_genes, gene_sets=msig_go_bp, genes_ref=unique(unlist(msig_go_bp)), verbose=FALSE)
      
      # gsea
      # see https://www.biostars.org/p/113680/
      # selection_ranked = dplyr::arrange(data.frame(gene=selected_data()$Gene, score=unlist(selected_data()[,2])*unlist(selected_data()[,3])), desc(score))
      # global_ranked = dplyr::arrange(data.frame(gene=mode_choice()$Gene, score=unlist(mode_choice()[,2])*unlist(mode_choice()[,3])), desc(score))
      
      return(res)
      
    } else {
      
      return(NULL)
      
    }
  })
  
  
  output$enrichment <- renderPrint({
    
    if(!is.null(enrichment_results())) {
      return(enrichment_results())
    } else {
      return("Enrichment not run ...")
    }
    
  })
  
  
  output$download_enrichment <- downloadHandler(
    filename = function() { 
      paste('enrichment_results', '.csv', sep='') 
    },
    content = function(file) {
      write_csv(enrichment_results(), file)
    }
  )
  
  
  sushi_p <- eventReactive(input$do_sushi, { # get sushi plot ready on button click
    
    sample_ix = sapply(input$cell_browser_choice, function(x) which(rownames(dat()[[1]]$res)==x))
    data_type = input$data_type_choice
    win = input$browser_window
    col_ix = which(colnames(dat()[[1]]$res) %in% input$gene_browser_choice)
    
    roi = gene_list_all[col_ix]
    start(roi) = start(roi) - win
    end(roi) = end(roi) + win
    
    # TO EDIT BY COLLAPSE-TO-GENE TYPE
    tss_window = 2e3
    tss_regions = gene_list_all[col_ix]
    start(tss_regions) = tss_regions$transcription_start_site - tss_window
    end(tss_regions) = tss_regions$transcription_start_site + tss_window
    tss_regions_df = as.data.frame(tss_regions)[,1:3]
    
    my_tracks_df = vector("list", length(data_type))
    
    for(i in 1:length(data_type)) {
      
      if(data_type[i] == "RNA") {
        
        rna_in = unlist(tile(roi, width=5))
        rna_in$score = 0
        for(k in 1:length(tss_regions)) {
          rna_in$score[subjectHits(findOverlaps(tss_regions[k], rna_in))] = k
          # rna_in = c(rna_in[rna_in$score!=k], reduce(rna_in[rna_in$score==k]))
          # rna_in$score[is.na(rna_in$score)] = k
          # rna_in = sort(rna_in)
        }
        
        rna_in_ls = lapply(1:length(sample_ix), function(x) as.data.frame(rna_in)[,c(1:3,6)])
        
        for(j in 1:length(sample_ix)) {
          for(k in 1:length(col_ix)) {
            if(is.na(dat()$RNA$res[sample_ix[j],col_ix[k]])) {
              rna_in_ls[[j]]$score[rna_in_ls[[j]]$score==k] = 0
            } else {
              rna_in_ls[[j]]$score[rna_in_ls[[j]]$score==k] = dat()$RNA$res[sample_ix[j],col_ix[k]]
            }
          } 
        }
        
        my_tracks_df[[i]] = rna_in_ls
        
      } else {
        
        for(j in sample_ix) {
          
          ### FOR SERVER ###
          x = str_replace(dat()[[data_type[i]]]$annot$Bigwig[j], "/GWD/bioinfo/projects/RD-Epigenetics-NetworkData/", "http://ftp.ebi.ac.uk/pub/databases/opentargets/")
          cat(file=stderr(), "File to import is:", x, "\n")
          my_tracks_df[[i]] = c(my_tracks_df[[i]], list(as.data.frame(import.bw(x, which=roi))[,c(1:3,6)]))
          ### FOR SERVER ###
          
          ### FOR LOCAL ###
          # x = str_replace(dat()[[data_type[i]]]$annot$Bigwig[j], "/GWD/bioinfo/projects/", "z:/links/")
          # if(file.exists(x)) {
          #   my_tracks_df[[i]] = c(my_tracks_df[[i]], list(as.data.frame(import.bw(x, which=roi))[,c(1:3,6)]))
          # } else {
          #   my_tracks_df[[i]] = c(my_tracks_df[[i]], list(data.frame()))
          # }
          ### FOR LOCAL ###
          
        }
      }
      
    }
    
    t_list_filtered = filter(t_list, external_gene_name %in% input$gene_browser_choice)
    
    layout(matrix(c(1:((length(data_type)+2)*length(roi))), length(data_type)+2, length(roi), byrow=FALSE), heights=c(rep(6/length(data_type),length(data_type)),1,4))
    par(mar=c(4,3,3,3))
    
    for(g_ix in 1:length(roi)) {
      
      chrom = as.character(seqnames(roi)[g_ix])
      chromstart = start(roi)[g_ix]
      chromend = end(roi)[g_ix]
      
      for(d_ix in 1:length(data_type)) {
        
        # get order
        my_tracks_df_g = lapply(my_tracks_df[[d_ix]], function(x) dplyr::filter(x, (seqnames==chrom & start>=chromstart & end<=chromend)))
        sample_o = order(unlist(lapply(my_tracks_df_g, function(x) range(x$score)[2])), decreasing=TRUE)
        
        if(g_ix==1) {
          plotBedgraph(my_tracks_df_g[[sample_o[1]]], chrom, chromstart, chromend, transparency=.001, color=opaque(SushiColors(max(length(sample_ix),2))(length(sample_ix))[sample_o[1]]), main=data_type[d_ix], cex.main=3)
        } else {
          plotBedgraph(my_tracks_df_g[[sample_o[1]]], chrom, chromstart, chromend, transparency=.001, color=opaque(SushiColors(max(length(sample_ix),2))(length(sample_ix))[sample_o[1]]))
        }
        
        if(d_ix==length(d_ix)) {
          labelgenome(chrom, chromstart, chromend, n=10, scale="Mb", cex.axis=2, scalecex=2, scaleline=1.2, chromcex=2, chromline=1.2)
        } else {
          labelgenome(chrom, chromstart, chromend, n=10, scale="Mb", cex.axis=2, scalecex=2, scaleline=1.2, chromcex=2, chromline=1.2) # turn off labels here, need to use axis()
        }
        axis(side=2, las=2, tcl=.2, cex.axis=2)
        
        if(length(sample_o)>1) {
          for(j in 2:length(sample_o)) {
            plotBedgraph(my_tracks_df_g[[sample_o[j]]], chrom, chromstart, chromend, transparency=.001, color=opaque(SushiColors(max(length(sample_ix),2))(length(sample_ix))[sample_o[j]]), overlay=TRUE, rescaleoverlay=FALSE)
          }
        }
        
        if(g_ix==1 & d_ix==1) {
          legend("topright", inset=0.025, legend=rownames(dat()[[1]]$res)[sample_ix], fill=opaque(SushiColors(max(length(sample_ix),2))(length(sample_ix))), border=SushiColors(max(length(sample_ix),2))(length(sample_ix)), text.font=2, cex=2.0)
        }
      }
      
      plotBed(tss_regions_df, chrom, chromstart, chromend)
      
      plotGenes(t_list_filtered, chrom, chromstart, chromend, types=t_list_filtered$type, labeltext=TRUE, maxrows=50, height=0.4, plotgenetype="box", fontsize=2)
      labelplot(title=roi$hgnc_symbol[g_ix], titlecex=2)
      
    }
    
  })
  
  
  output$sushi <- renderPlot({ # plot sushi plot
    sushi_p()
  })
  
  
  output$gsk_data = DT::renderDataTable(escape=FALSE, {
    
    # https://stackoverflow.com/questions/26026740/rstudio-shiny-list-from-checking-rows-in-datatables  
    # add_check <- paste0('<input type="checkbox" name="row', 1, '" value="', 1, '">',"")
    # cbind(Download=add_check, data_gsk[,input$show_vars,drop=FALSE])
    data_gsk[,input$show_vars,drop=FALSE]
    
  }, filter="top", rownames=FALSE
  )
  
  
  output$download_gsk_data <- downloadHandler(
    filename = function() { 
      paste('data_gsk', '.csv', sep='') 
    },
    content = function(file) {
      write_csv(data_gsk, file)
    }
  )
  
  
  output$download_bigwig <- downloadHandler(
    filename = function() { 
      paste('selected_signals', '.zip', sep='') 
    },
    content = function(file) {
      # working on code for this - will be slow ...
    }
  )
  
  
  output$info_button_1 <- renderImage({
    return(list(
      src = "www/information.png",
      filetype = "image/png",
      alt = "Information on plot",
      width = "20px",
      height = "20px"
    ))
  }, deleteFile=FALSE)
  
  
  output$info_button_2 <- renderImage({
    return(list(
      src = "www/information.png",
      filetype = "image/png",
      alt = "Information on plot",
      width = "20px",
      height = "20px"
    ))
  }, deleteFile=FALSE)
  
  
  output$info_button_3 <- renderImage({
    return(list(
      src = "www/information.png",
      filetype = "image/png",
      alt = "Information on plot",
      width = "20px",
      height = "20px"
    ))
  }, deleteFile=FALSE)
  
  
  output$info_button_4 <- renderImage({
    return(list(
      src = "www/information.png",
      filetype = "image/png",
      alt = "Information on plot",
      width = "20px",
      height = "20px"
    ))
  }, deleteFile=FALSE)
  
  
  output$info_button_5 <- renderImage({
    return(list(
      src = "www/information.png",
      filetype = "image/png",
      alt = "Information on plot",
      width = "20px",
      height = "20px"
    ))
  }, deleteFile=FALSE)
  
  
  output$info_button_6 <- renderImage({
    return(list(
      src = "www/information.png",
      filetype = "image/png",
      alt = "Information on plot",
      width = "20px",
      height = "20px"
    ))
  }, deleteFile=FALSE)
  
  
  gvTab2("gv", input, output, session, ui=F, cx=cx)
  
  
})


