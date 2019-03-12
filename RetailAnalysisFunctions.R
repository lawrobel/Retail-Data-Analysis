# load libraries
suppressPackageStartupMessages(library(tidyverse)) # suppress startup messages since they get printed out in rmarkdown and this is distracting
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(arules))
suppressPackageStartupMessages(library(arulesViz))
suppressPackageStartupMessages(library(useful))
suppressPackageStartupMessages(library(RColorBrewer))

convert_to_transactions<-function(df,description_start_end=c(0,0)){
    #This function converts a dataframe, which contains of invoice IDs and product descriptions 
    #given here as InvoiceNumber and Description (amongest other variables), to a transactions
    #object that can be used with the arules package
    #
    #Args: df is the dataframe to be turned into a transactions object
    #description_start_end is a vector of two numbers (start_position, end_position)* 
    #
    #Returns: trans which is an object of class transactions
    #
    #*Note: start_position is the index of the first word of the description to use and end_position is
    #the index of the last word of the description to use. For instance, if the description is "Large Polka Dot Dress"
    #and description_start_end=(2,4), then the description subset used in the transactions will be 
    #"Polka Dot Dress". description_start_end=c(0,0) means use full description  
    
    start_position<-description_start_end[1]
    end_position<-description_start_end[2]
  
    if (start_position != 0){
      # the word function is from stringr and is used to extract subsets of individual words from the whole description
      df_2 <- df %>% filter(is.na(Description)==FALSE) %>% mutate(Description = paste(word(Description,start_position,end_position)))
    }
    else{
      df_2 <- df
    }
    transcation_id_Description <-df_2 %>% select(InvoiceNumber, Description) 
  
    # the following three lines of code are used to spread the descriptions to columns which is necessary to get the data into a
    # transactions matrix
    setDT(transcation_id_Description)[, N:= paste0('Description',1:.N), InvoiceNumber]
    transcation_id_Description_2<-dcast(transcation_id_Description, InvoiceNumber~ N, value.var='Description', fill='')
  
    transcation_id_Description_3<-dcast(melt(transcation_id_Description_2,id.var="InvoiceNumber"), InvoiceNumber ~ value, length)
  
    # remove all variables besides the different descriptions, V1 and NA are temporary variables which need to be removed
    transcation_id_Description_4  <- transcation_id_Description_3 %>% select(-V1,-InvoiceNumber,-`NA`)
    transcation_id_Description_matrix  <- as.matrix(transcation_id_Description_4)
  
    # convert matrix to transcations object, the as function is from the arules package
    trans <- suppressWarnings(as(transcation_id_Description_matrix, "transactions")) # warning is given about matrix not having all 1's, ignore this, this is no issue
                                                                                
  
  return(trans)
}
