#' plot PCA
#'
#' This function loads files and plot PCA as output 
#' @export
#' @param x experimental design input
#' @param y protein groups input
get_obj<-function(x, y){

ExperimentalDesign<-x
proteinGroups<-y
# read in experimental desgin
#ExperimentalDesign<-read.delim("inst/extdata/Experimental_Design.txt",header=TRUE)
#sort according to the file names
ExperimentalDesign<-ExperimentalDesign[order(ExperimentalDesign$Old.Sample.Name),]

# read in full proteinGroups.txt file
#proteinGroups<-read.delim("inst/extdata/proteinGroups.txt",row.names=1,header=TRUE)

# read in configuration from html in the future
  proteinGroups_Reversed<-proteinGroups[proteinGroups$Reverse=="+",]
  proteinGroups_Contaminant<-proteinGroups[proteinGroups$Contaminant=="+",]
  proteinGroups_Only.identified.by.site<-proteinGroups[proteinGroups$Only.identified.by.site=="+",]
  
  # count how many for the three reverse/contaminat/identified.by.site  
  proteinGroups_count<-nrow(proteinGroups)
  proteinGroups_count_Reversed<-nrow(proteinGroups_Reversed)
  proteinGroups_count_Contaminant<-nrow(proteinGroups_Contaminant)  
  proteinGroups_count_Only.identified.by.site<-nrow(proteinGroups_Only.identified.by.site)
  
  # remove all the rows marked as reverse/contaminat/identified.by.site
  proteinGroups_filtered<-proteinGroups[proteinGroups$Reverse!="+" & proteinGroups$Contaminant!="+" & proteinGroups$Only.identified.by.site!="+",]
  proteinGroups_filtered_count<-nrow(proteinGroups_filtered)
  
  proteinGroups_filtering<-(list(filtered=proteinGroups_filtered,
              proteinGroups_count=proteinGroups_count,
              proteinGroups_count_Reversed=proteinGroups_count_Reversed,
              proteinGroups_count_Contaminant=proteinGroups_count_Contaminant,
              proteinGroups_count_Only.identified.by.site=proteinGroups_count_Only.identified.by.site,
              proteinGroups_filtered_count=proteinGroups_filtered_count    
  ))
  
# Step 1: do filtering, to remove rows with contaminant/revers/identified by id
proteinGroups_filtered<-proteinGroups_filtering$filtered

#The filtering summary can also be reported out to the user end
#proteingroups before filtering:
#proteinGroups_filtering$proteinGroups_count
#proteingroups after filtering:
#proteinGroups_filtering$proteinGroups_filtered_count
#proteingroups marked as reversed
#proteinGroups_filtering$proteinGroups_count_Reversed
#proteingroups  marked as contaminant
#proteinGroups_filtering$proteinGroups_count_Contaminant
#proteingroups marked as Only identified by site
#proteinGroups_filtering$proteinGroups_count_Only.identified.by.site


# keep the LFQ columns as example for downstream analysis
proteinGroups_headers<-colnames(proteinGroups_filtered)
proteinGroups_filtered_LFQ_intensity<-proteinGroups_filtered[,grep("LFQ.Intensity.",proteinGroups_headers)]

# simplify LFQ column names
colnames(proteinGroups_filtered_LFQ_intensity)<-gsub("LFQ.Intensity.", "", colnames(proteinGroups_filtered_LFQ_intensity))
# re-arrange the columns, according to the names
proteinGroups_filtered_LFQ_intensity<-proteinGroups_filtered_LFQ_intensity[,order(colnames(proteinGroups_filtered_LFQ_intensity))]



# Step 2: very basic data process: normalization

# replace the 0 with NaN
proteinGroups_filtered_LFQ_intensity[proteinGroups_filtered_LFQ_intensity==0]<-NaN
# take log10
proteinGroups_filtered_LFQ_intensity_log10<-log10(proteinGroups_filtered_LFQ_intensity)

# filter out the entrieséproteins which have larger number of NA counting
# the threshold cold be easily setup by QXX starndard, by using ncol(matrix)*Q
# here we start from with 0 tolerannce of NA (Q100) as a test
proteinGroups_filtered_LFQ_intensity_log10_Q100<-proteinGroups_filtered_LFQ_intensity_log10[which(apply(proteinGroups_filtered_LFQ_intensity_log10,1,function(x)(sum(is.na(x)))<1)),]
proteinGroups_filtered_LFQ_intensity_log10_Q100
}