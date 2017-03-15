#' Select data from protein groups
#' @export
#' @param proteinfileinput obtained from file input

Selectdata_From_ProteinGroupsFile<-function(proteinfileinput,expression_data_tobe_selected = "LFQ_intensity"){
  proteinGroups<-proteinfileinput
  #do filtering, to remove rows with contaminant/revers/identified by id
  proteinGroups_Reversed<-proteinGroups[proteinGroups$Reverse=="+",]
  proteinGroups_Contaminant<-proteinGroups[proteinGroups$Contaminant=="+",]
  proteinGroups_Only.identified.by.site<-proteinGroups[proteinGroups$Only.identified.by.site=="+",]
  
  # remove all the rows marked as reverse/contaminat/identified.by.site
  proteinGroups_filtered<-proteinGroups[proteinGroups$Reverse!="+" & proteinGroups$Contaminant!="+" & proteinGroups$Only.identified.by.site!="+",]
 
  proteinGroups_filter_summary<-list( # this is for output
      proteinGroups_count_orignal=nrow(proteinGroups),
      proteinGroups_count_Reversed=nrow(proteinGroups_Reversed),
      proteinGroups_count_Contaminant=nrow(proteinGroups_Contaminant),
      proteinGroups_count_Only.identified.by.site=nrow(proteinGroups_Only.identified.by.site),
      proteinGroups_count_after=nrow(proteinGroups_filtered)
    )
  
  # to select the columns of expression
  proteinGroups_headers<-colnames(proteinGroups_filtered)
  # keep the LFQ columns as example for downstream analysis
  if(expression_data_tobe_selected == "LFQ_intensity"){
    proteinGroups_filtered_LFQ_intensity<-proteinGroups_filtered[,grep("LFQ.Intensity.",proteinGroups_headers)]
    # simplify LFQ column names
    colnames(proteinGroups_filtered_LFQ_intensity)<-gsub("LFQ.Intensity.", "", colnames(proteinGroups_filtered_LFQ_intensity))
    # re-arrange the columns, according to the names
    proteinGroups_filtered_LFQ_intensity<-proteinGroups_filtered_LFQ_intensity[,order(colnames(proteinGroups_filtered_LFQ_intensity))]
    proteinGroups_filtered_selected_matrix<-proteinGroups_filtered_LFQ_intensity
  }
  
  
  #output, as table for session, and return a list to be catched by js
  write.table(proteinGroups_filtered_selected_matrix,paste("Out_ProteinGroups_SelectedColumns_",expression_data_tobe_selected,".txt",sep=""),sep="\t",row.names = TRUE,col.names = NA)  
  
  return(list(proteinGroups_filtered_selected_matrix=proteinGroups_filtered_selected_matrix,
              proteinGroups_filter_summary = proteinGroups_filter_summary
  ))
}