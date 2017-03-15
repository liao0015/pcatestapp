
#' Process Marix Intensity
#' @export
#' @param IntensityMarix data_matrix as input
#' @param threshold the default value is set to 1
#' @param Imputation the default value is set to TRUE
#' @param Normalize_columns the default value is set to TRUE
#' @param Normalize_rows the default value is set to TRUE

IntensityMarix_process<-function(IntensityMarix, threshold = 1, Imputation = TRUE, Normalize_columns = TRUE, Normalize_rows = TRUE){
  
  IntensityMarix[IntensityMarix==0]<-NaN # replace the 0 with NaN
  IntensityMarix_log10<-log10(IntensityMarix) # take log10

  # missing value filtering
  tempt_filter_result <- missingvalue_filtering(IntensityMarix_log10, threshold)
  IntensityMarix_log10_NAfiltered <- tempt_filter_result$data_qualified # home made function [missingvalue_filtering]
  IntensityMarix_log10_NAfiltered_filtering_summary <-tempt_filter_result$filtering_summary

  p1<-matrix_ggboxplot(IntensityMarix_log10_NAfiltered, maintitle = "Distribution of NA-filtered")$violinplot
  p2<-matrix_ggboxplot(IntensityMarix_log10_NAfiltered, maintitle = "Distribution of NA-filtered")$boxplot
    
  if(Imputation == "TRUE"){  # missing value imputation
    IntensityMarix<-rrcovNA::impSeqRob(IntensityMarix_log10_NAfiltered)$x
  }
  
  if (Normalize_columns == "TRUE"){ # do column scaling, keep in mind that the scale function in R is scaling by column
    IntensityMarix<-scale(IntensityMarix)
  }
  
  if (Normalize_rows == "TRUE"){ # scaling of each protein
    IntensityMarix<-t(scale(t(IntensityMarix)))
  }
  
  p3<-matrix_ggboxplot(IntensityMarix, maintitle = "Distribution of NA-filtered & Processed")$violinplot
  p4<-matrix_ggboxplot(IntensityMarix, maintitle = "Distribution of NA-filtered & Processed")$boxplot
  
    
  write.table(IntensityMarix,paste("Out_ProteinGroups_NAfiltered_Scaled",".txt",sep=""),sep="\t",row.names = TRUE,col.names = NA)  
  return(list(IntensityMarix_processed = IntensityMarix,
              IntensityMarix_filtering_summary = IntensityMarix_log10_NAfiltered_filtering_summary,
              violinplot.before = p1,
              boxplot.before = p2,
              violinplot.after = p3,
              boxplot.after = p4
              
  ))
}