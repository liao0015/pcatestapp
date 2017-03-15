#________________________________________________________________________________________
#     matrix_ggboxplot
#________________________________________________________________________________________

# ___Description___: 
# ggplot2 is powerful at boxplot and violinplot, but needs some pre-process of the datamatrix, a bit tricky sometimes
# here, the steps are wrapped up to give out an easy way

# ___Arguments___:
# data_matrix: data matrix
#  xlabel, ylabel, maintitle 

#____Usage____;
# boxplot_ressult <- matrix_ggboxplot(data_matrix, xlabel="Sample", ylabel = "Value", maintitle = "Distribution")
# plot by: boxplot_ressult$boxplot, boxplot_ressult$violinplot

# ___Values___:
# a list of plot, the first object boxplot, second one is violinplot
#' ggbox plot
#' @export
#' @param data_matrix data_matrix as input
#' @param xlabel label for x
#' @param ylabel label for y
#' @param maintitle title for main

matrix_ggboxplot<-function(data_matrix, xlabel="Samples", ylabel = "Value", maintitle = "Distribution"){
  data_matrix_melt<-reshape2::melt(as.matrix(data_matrix))
  # in data_matrix_melt, Var1 is the orignal row.names, Var2 is the orignial column names, value is the orignial values

  p1<-ggplot(data_matrix_melt, aes(x = Var2, y = value, fill=Var2))+geom_boxplot() 
  p1<-p1+labs(x = xlabel,y=ylabel,title = maintitle, fill = "Samples")
  p1<-p1+theme_bw()+theme(plot.title = element_text(hjust = 0.5)) 
  
  p2<-ggplot(data_matrix_melt, aes(x = Var2, y = value, fill=Var2)) +geom_jitter(shape=21,alpha=0.3) +geom_violin()
  p2<-p2+labs(x = xlabel,y=ylabel,title = maintitle)
  p2<-p2+theme_bw()+theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Samples"))
  
  return(list(boxplot = p1, violinplot = p2))
}
