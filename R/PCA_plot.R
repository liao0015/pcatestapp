#' PCA screen plot original
#' @export
#' @param data_matrix take data_matrix as input
#' @param grouping optional for plotting

PCA_plot<-function(data_matrix, grouping){
  
  data_matrix_t<-t(data_matrix)
  data_matrix_t_merge <- merge(grouping, data_matrix_t, by.y=0, by.x = "Sample.Name")  
  row.names(data_matrix_t_merge)<-data_matrix_t_merge$Sample.Name
  p1<-autoplot(prcomp(data_matrix_t), data = data_matrix_t_merge, colour = 'Groups',label = TRUE )
  p1<-p1+labs(title = "PCA Clustering")
  p1<-p1+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
  return(list(pca.plot = p1))
}