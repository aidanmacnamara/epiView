
# single_labels = rownames(dat[[1]]$res)
# group_labels = "GSK"
# pca_data = prep_for_plot(dat, annot_1=group_labels, annot_2=single_labels, marks=names(dat), plot_type="mds")

shinyServer(function(input, output) {
  
    
  global_choice = eventReactive(input$do_global, {
    validate(
      need(input$project_choice != "", 'Please select at least one project to view.')
    )
    
    tmp_list = list()
    
    tmp = dat
    proj_ix = which(tmp[[1]]$annot$Project %in% input$project_choice)
    
    if(!"All" %in% input$project_choice) {
      for(i in 1:length(tmp)) {
        tmp[[i]]$res = tmp[[i]]$res[proj_ix,] 
        tmp[[i]]$annot = tmp[[i]]$annot[proj_ix,]
      }
    }
    
    tmp_list$tmp = tmp
    tmp_list$mds_type = input$mds_type
    tmp_list$labels = tmp[[1]]$annot$Label
    
    return(tmp_list)
  })
  
  
  global_plot <- reactive({
    
    single_labels = rownames(global_choice()$tmp[[1]]$res)
    group_labels = global_choice()$tmp[[1]]$annot$Project
    pca_data = prep_for_plot(global_choice()$tmp, annot_1=group_labels, annot_2=single_labels, marks=names(global_choice()$tmp), plot_type=global_choice()$mds_type)
    pca_data$annot_1 = paste("Project", pca_data$annot_1)
    return(pca_data)
  })
  
  
  output$global_view <- renderPlot({
    print(ggplot(global_plot(), aes(x=x, y=y, color=factor(annot_1))) + geom_point(size=3, shape=17) + theme_thesis(20) + geom_text_repel(aes(label=annot_2), fontface="bold", size=input$label.size.global, force=0.5, show.legend=FALSE) + facet_wrap(~mark, nrow=2, scales="free"))
    
  })
  
  
  output$fountain_plot <- renderPlot({
    
    if(
      (length(input$gene_choice) | length(input$go_choice)) &
      length(input$cell_target_choice) &
      length(input$cell_candidate_choice)
    ) {
      
      if(length(input$go_choice)) {
        genes = unique(unlist(msig_go_bp[names(msig_go_bp) %in% input$go_choice]))
      } else {
        genes = input$gene_choice
      }
      
      c_cells = input$cell_candidate_choice
      t_cells = input$cell_target_choice
      
      col_ix = which(colnames(dat[[1]]$res) %in% genes)
      
      # slice matrices if necessary
      dat_plot = dat
      for(j in 1:length(dat_plot)) { # each data type
        dat_plot[[j]]$res = dat[[j]]$res[,col_ix,drop=FALSE]
      }
      
      single_labels = rownames(dat_plot[[1]]$res)
      
      c_ix = match(c_cells, rownames(dat[[1]]$res))
      t_ix = match(t_cells, rownames(dat[[1]]$res))
      
      res = dist_mat(dat_plot, comp_ix=list(c_ix, t_ix), labels=single_labels, plot_labels=c("BEAS2B","A549","NHLF"), plot_res=TRUE, use_corr=TRUE, font_size=30, label_size=input$label.size.1)
      
    } else {
      res = dist_mat(x=dat, comp_ix=list(), labels=single_labels, plot_res=TRUE, use_corr=TRUE, font_size=30, label_size=input$label.size.1, plot_blank=TRUE)
    }
  })
  
  
  model_choice <- eventReactive(input$do_model, {
    
    c_ix = which(rownames(dat[[1]]$res) %in% input$candidate_choice)
    a_ix = which(rownames(dat[[1]]$res) %in% input$alt_choice)
    t_ix = which(rownames(dat[[1]]$res) %in% input$target_choice)
    
    if(
      length(c_ix) &
      length(a_ix) &
      length(t_ix) &
      length(input$x_axis) &
      length(input$y_axis)
    )  {
      
      my_df = spotfire_view(dat, x_axis=input$x_axis, y_axis=input$y_axis, comp_ix=list(c_ix, a_ix, t_ix))
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
    
    ggplot(dat_out, aes_string(names(dat_out)[2], names(dat_out)[3])) + geom_point(size=2, aes(color=color, alpha=color)) + theme_thesis() + scale_color_discrete(guide=FALSE) + scale_alpha_manual(guide=FALSE, values=c(0.3,0.8))
    
  })
  
  
  selected_data <- reactive({
    brushedPoints(model_choice(), input$plot_model_choice_brush)
  })
  
  
  output$brush_info <- renderPrint({
    selected_data()
  })
  
  
  output$download_table <- downloadHandler(
    filename = function() { 
      paste('selected_rows', '.csv', sep='') 
    },
    content = function(file) {
      write_csv(selected_data(), file)
    }
  )
  
  
  output$GSEA <- renderPrint({
    
    # gsea_input = dplyr::arrange(data.frame(gene=x$Gene, score=unlist(x[,2])*unlist(x[,3])), desc(score))
    # rownames(gsea_input) = gsea_input$gene
    # gsea_input = as.matrix(gsea_input[,2,drop=FALSE])
    
    return(data.frame(foo=1:10, bar=LETTERS[1:10]))
    
  })
  
  
  output$sushi<- renderPlot({
    
    sample_ix = c(1,3,15)
    data_type = 1
    win = 20000
    
    if(
      (my_choices()$gene_choice[1]!="NULL" | my_choices()$go_choice[1]!="NULL") &
      my_choices()$cell_target_choice[1]!="NULL" &
      my_choices()$cell_candidate_choice[1]!="NULL"
    ) {
      
      if(my_choices()$go_choice[1]!="NULL") {
        print(my_choices()$go_choice)
        genes = msig_go_bp[[which(names(msig_go_bp)==my_choices()$go_choice)]]
      } else {
        genes = my_choices()$gene_choice
      }
      
      col_ix = which(colnames(dat[[1]]$res) %in% genes)
      
      roi = gene_list_all[col_ix]
      start(roi) = start(roi) - win
      end(roi) = end(roi) + win
      
      my_tracks = sapply(str_replace(dat[[data_type]]$annot$Bigwig[sample_ix], "/GWD/bioinfo/projects/", "z:/links/"), function(x) import.bw(x, which=roi))
      
      mart_1 = useMart("ensembl", dataset="hsapiens_gene_ensembl")
      t_list = getBM(attributes=c("chromosome_name","exon_chrom_start","exon_chrom_end","ensembl_transcript_id","strand","ensembl_gene_id"), filters='hgnc_symbol', values=roi$hgnc_symbol, mart=mart_1)
      t_list$type = "exon"
      t_list$chromosome_name = paste0("chr", t_list$chromosome_name)
      
      my_tracks_df = lapply(my_tracks, function(x) as.data.frame(x)[,c(1:3,6)])
      
      par(mfcol=c(length(sample_ix)+1,length(roi)), mar=c(4,4,2,2))
      
      for(g_ix in 1:length(roi)) {
        
        chrom = as.character(seqnames(roi)[g_ix])
        chromstart = start(roi)[g_ix]
        chromend = end(roi)[g_ix]
        
        for(i in 1:length(sample_ix)) {
          
          if(g_ix==1) {
            plotBedgraph(my_tracks_df[[i]], chrom, chromstart, chromend, transparency=.2, color=SushiColors(2)(length(sample_ix))[i], main=rownames(dat[[1]]$res)[sample_ix[i]])
          } else{
            plotBedgraph(my_tracks_df[[i]], chrom, chromstart, chromend, transparency=.2, color=SushiColors(2)(length(sample_ix))[i])
          }
          labelgenome(chrom, chromstart, chromend, n=10, scale="Mb")
          axis(side=2, las=2, tcl=.2)
          
        }
        
        plotGenes(t_list, chrom, chromstart, chromend, types=t_list$type, labeltext=TRUE, maxrows=50, height=0.4, plotgenetype="box")
        labelplot(title=roi$hgnc_symbol[g_ix])
        
      }
    }
  })

    
})

