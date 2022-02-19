## Calculate the split half consistency for memorability
## function for split half with X iterations
split = function(iteration){
  
  mem.scores.1 = data.frame(matrix(NA, nrow = 138)) 
  mem.scores.2 = data.frame(matrix(NA, nrow = 138)) 
  mem.scores.1[,1] <- img.names
  colnames(mem.scores.1) = "image"
  mem.scores.2[,1] <- img.names 
  colnames(mem.scores.2) = "image"
  
  corr = data.frame(matrix(NA, ncol = 2)) %>% dplyr::rename(rank.corr = X1, p = X2)
  
  for (i in 1:iteration) {
    tryCatch({
    ind <- sample.int(n = nrow(dat.mem), size = nrow(dat.mem)/2, replace=F)
    dat.1 <- dat.mem[ind, ]
    dat.2 <- dat.mem[-ind,]
    
    img.values.1 <- calculate_mem(dat.1) %>% select(Memorability) %>% tibble::rownames_to_column("image")
    mem.scores.1 <- merge(img.values.1, mem.scores.1, by = "image")
    names(mem.scores.1)[names(mem.scores.1) == 'Memorability'] <- as.character(ncol(mem.scores.1)+1)

    img.values.2 <- calculate_mem(dat.2) %>% select(Memorability) %>% tibble::rownames_to_column("image")
    mem.scores.2 <- merge(img.values.2, mem.scores.2, by = "image")
    names(mem.scores.2)[names(mem.scores.2) == 'Memorability'] <- as.character(ncol(mem.scores.2)+1)
    
    cor.data <- merge(img.values.1, img.values.2, by = "image")
    corr.result = cor.test(x = cor.data$Memorability.x, y = cor.data$Memorability.y, method = 'spearman')
    corr = rbind(corr, unname(c(corr.result$estimate, corr.result$p.value)))
    print(i)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  list.results <- list(corr = corr,
                  scores.1 = mem.scores.1,
                  scores.2 = mem.scores.2)
  return(list.results)
}


#apply function for calculating split-half analysis
list.results = split(1000)


#apply the function split() loaded from the R file to calculate with 1000 iterations
list.results = split(1000)

mem.scores.1 = list.results$scores.1
mem.scores.2 = list.results$scores.1
corr = list.results$corr

#calculate mean memorability and ranking for Group 1
mem.scores.1$mean <- rowSums(mem.scores.1[,3:(ncol(mem.scores.1))])/(ncol(mem.scores.1)-2)
mem.scores.1$Rank<- 139-rank(mem.scores.1$mean)
mem.scores.1 <- mem.scores.1[,colSums(is.na(mem.scores.1))<nrow(mem.scores.1)]
mean.1 = mem.scores.1 %>% select(image, mean, Rank)

#calculate mean memorability for Group 2
mem.scores.2$mean <- rowSums(mem.scores.2[,3:(ncol(mem.scores.2))])/(ncol(mem.scores.2)-2)
mem.scores.2 <- mem.scores.2[,colSums(is.na(mem.scores.2))<nrow(mem.scores.2)]
mean.2 = mem.scores.2 %>% select(image, mean)

#merge mean from Group 1 and Group 2
plot.data = merge(mean.1, mean.2, by = "image")

## Random level mem scores
mem.scores.value.1 = mem.scores.1 %>% select(-image, -X, -Rank, -mean)
ran = data.frame(matrix(NA, nrow = 138))

for (i in 1:ncol(mem.scores.value.1)){
  rows <- sample(nrow(mem.scores.value.1))
  ran <- data.frame(ran, mem.scores.value.1[rows, i])
}

ran = ran %>% select(-starts_with("matrix"))
mean.ran <- rowSums(ran[,2:(ncol(ran))])/(ncol(ran)-1)

#merge randome level data with the two groups
plot.data.ran = data.frame(plot.data, mean.ran) 