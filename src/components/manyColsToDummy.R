#A base-R script that searches over many input columns for input values 
#and returns a dataframe of dummy variables that indicate if the values 
#were present in these columns.

#Note that grep functions use regular expressions as their search terms, so if you are trying to match a string or number, adding ^ and $ is necessary!
#For example, if you want to match 612, use "^612$"



manyColsToDummy<-function(search_terms, search_columns,
                          output_table){
    #initialize output table
    temp_table<-data.frame(matrix(ncol=length(search_terms),
                                  nrow= nrow(search_columns)))
    colnames(temp_table)<-search_terms
    
    #make table
    for (i in 1:length(search_terms)){
        vec<-rowSums(sapply(search_columns,
                            function(x) grepl(search_terms[i], x, ignore.case = TRUE)
        ))>0
        temp_table[,i]<-vec
    }
    temp_table<-sapply(temp_table, as.integer, as.logical)
    temp_table<-as.data.frame(temp_table)
    assign(x = output_table, value = temp_table, envir = globalenv())
}


#Example
#manyColsToDummy("find_this", big_table[,columns_to_look_in], "dummy_output_table")

#Visual example found here: https://ibb.co/VwHvBLT
