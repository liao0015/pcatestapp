#' PCA screen plot version one
#' @export
#' @param data_matrix take data_matrix as input

PCA_Screeplot<-function(data_matrix){
  pca.output <- prcomp(t(data_matrix), scale.=TRUE, center = TRUE) 
  sd <- pca.output$sdev
  scores <- pca.output$x
  var <- sd^2
  var.percent <- var/sum(var) * 100
  
  #barplot(var.percent, xlab="Principal Component", ylab="Percent of Variance", names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), col="gray", main="Percent of Variance")
  #abline(h=1/nrow(pca.output$rotation)*100, col="red")
  #p1 <- recordPlot()
  
  p1<-ggplot()+geom_bar(aes(x=c(1:length(var.percent)),y=var.percent), stat="identity")
  p1<-p1+geom_hline(yintercept = 1/nrow(pca.output$rotation)*100, colour = "red")
  p1<-p1+labs(x = "Princaple Component Number",y="Percent of Variance",title = "Screeplot of Variance")
  p1<-p1+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
  
  return(list(Scree.plot = p1))
}