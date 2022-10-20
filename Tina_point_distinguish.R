# install.packages('dplyr')
# install.packages('plyr')
# function
identity_detection <- function(file_path, same=T, dis=5, write=F, write_path=getwd(), prefix='')  {
  library(dplyr)
  data <- read.delim(file_path, header = T, sep = '\t')[, c(1,4,5)]
  
  if(isTRUE(same))  {
    data$recom <- paste(data$X, data$Y, sep = '_')
    data <- data[, c(1,4)]
    A <- as.data.frame(table(data$recom), stringsAsFactors = F)
    A <- A[!(A$Freq %in% 1), ]
    for(i in seq_len(nrow(A)))  {
      redun <- data[,1][data$recom %in% A[i,1]]
      A[i,seq(3, (3+length(redun)-1))] <- redun
    }
    colnames(A)[1:2] <- c('xy', 'freq')
    out2 <- A %>% replace(is.na(.), '')
    if(write== FALSE) {
      return(out2)
    }
    if(write==TRUE) {
      if(isFALSE(grepl(write_path, pattern = '&/')))  {
        write_path <- paste(write_path,'/', sep = '')
      }
      if(prefix=='')  {
        prefix <- 'same_xy_number'
      }
      write.csv(out2, file = paste(write_path, prefix, ".csv"),row.names = FALSE, quote = F)
    }
  }
  
  if(isFALSE(same))  {
    result <- dist(data[ c(2,3)], diag = F) %>%as.matrix() %>%  as.data.frame()
    result$add <- seq(1, nrow(result), 1)
    out <- list()
    for(i in  seq_len((ncol(result)-1)))  {
      A <- as.data.frame(result[,c(i, ncol(result))])
      A <- A[A[,1]<=dis,]
      A <- A[A$add>= i, ]
      # A <- A[order(A[,2], decreasing = F), ]
      out[[i]] <- A$add
    }
    
    out2 <- list()
    for(i in seq_along(out))  {
      out2[[i]] <- out[[i]] %>% as.matrix() %>% t() %>% as.data.frame()
    }
    
    out2 <- plyr::rbind.fill(out2[1:length(out2)])
    out2 <- out2 %>% replace(is.na(.), '')
    adj <- ifelse(out2>=0, yes = 1, no = 0) %>% as.data.frame()
    adj$sum <- rowSums(adj)
    adj <- adj[order(adj[,ncol(adj)], decreasing = T),]
    out2 <- out2[c(as.numeric(rownames(adj))),]
    if(write== FALSE) {
      return(out2)
    }
    if(write==TRUE) {
      if(isFALSE(grepl(write_path, pattern = '&/')))  {
        write_path <- paste(write_path,'/', sep = '')
      }
      if(prefix=='')  {
        prefix <- paste('dist_', dis, 'number', sep = '')
      }
      write.csv(out2, file = paste(write_path, prefix, ".csv", sep = ''),row.names = FALSE
                , quote = F)
    }
  }
  
}


# function usage
A <- identity_detection(file_path = 'C://Users/user/Documents/VP00293.txt', 
                        same = F, dis = 100, write = F, prefix = 'test')




