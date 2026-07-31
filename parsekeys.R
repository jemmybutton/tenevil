install.packages("data.tree")

library(data.tree)

rawkeys <- read.csv("characterkeys.csv")
keysList <- unique(toupper(unlist(strsplit(rawkeys$characterkeys,"[0-9]"))))
keysList <- keysList[2:length(keysList)]
keysTable <- data.frame(characternumber = rep(NA,nrow(rawkeys)))
for (i in keysList){
  keysTable[,i] <- NA
}
for (i in c("minN","maxN")){
  keysTable[[i]] <- as.data.frame(t(sapply(1:nrow(rawkeys), function(x){
    currentLine <- rawkeys[x,]
    currentKeys <- strsplit(currentLine$characterkeys, "(?<=..)", perl = TRUE)[[1]]
    currentKeysTable <- as.data.frame(t(sapply(currentKeys, function(y){strsplit(y, "")[[1]][c(2,1)]})))
    sapply(keysList, function(y){
      if (ncol(currentKeysTable) > 0){
        if (i == "minN"){
          n <- sum(as.numeric(currentKeysTable[currentKeysTable[,1] == y,2]))
        } else {
          n <- sum(as.numeric(currentKeysTable[toupper(currentKeysTable[,1]) == y,2]))
        }
        if (length(n) > 0){
          n
        } else {
          0
        }
      } else {
        0
      }
    })
  })))
  rownames(keysTable[[i]]) <- rawkeys$charactercode
  keysTable[[i]]$characternumber <- rawkeys$characternumber
}

produceTree <- function(keysTableSubset, keysLevel, depth){
  splitTable <- as.data.frame(t(sapply(keysList, function(x){
    toHalf <- as.data.frame(t(sapply((1 + min(keysTableSubset[["maxN"]][,x])):(max(keysTableSubset[["maxN"]][,x])), function(y){
      candidatePartA <- keysTableSubset[keysTableSubset[["maxN"]][,x] >= y,]
      candidatePartB <- keysTableSubset[keysTableSubset[["minN"]][,x] < y,]
      partSizeDifference <- abs(nrow(candidatePartA) - nrow(candidatePartB))
      uniqueRowsA <- unique(rbind(candidatePartA[["minN"]][,keysList],candidatePartA[["maxN"]][,keysList]))
      uniqueA <- nrow(uniqueRowsA)
      uniqueRowsB <- unique(rbind(candidatePartB[["minN"]][,keysList],candidatePartB[["maxN"]][,keysList]))
      uniqueB <- nrow(uniqueRowsB)
      uniqueUnion <- nrow(unique(rbind(uniqueRowsA,uniqueRowsB)))
      c(y, partSizeDifference, (uniqueA + uniqueB - uniqueUnion)/uniqueUnion)
    })))
    names(toHalf) <- c("index","psd","ui")
    minIndex <- toHalf$index[which(toHalf$psd == min(toHalf$psd))[1]]
    c(minIndex,toHalf$psd[which(toHalf$index == minIndex)])
  })))
  names(splitTable) <- c("index","value")
  splitPosition <- splitTable[which(splitTable$value == min(splitTable$value, na.rm = T))[1],]
  keyName <- rownames(splitPosition)
  keyValue <- splitPosition$index
  partA <- keysTableSubset[keysTableSubset[["maxN"]][,keyName] >= keyValue,]
  partA[["minN"]][partA[["minN"]][,keyName] < keyValue, keyName] <- keyValue
  partB <- keysTableSubset[keysTableSubset[["minN"]][,keyName] < keyValue,]
  partB[["maxN"]][partB[["maxN"]][,keyName] >= keyValue, keyName] <- keyValue - 1

  if (
    nrow(partA) > 3
    & nrow(partB) > 3
    & nrow(keysTableSubset[["minN"]]) >= 15
    & depth > 0
  ) {
    keysLevel$charactersList <- NA
    keysLevelPartA <- keysLevel$AddChild(paste(keyName, "ge", keyValue, sep = ""))
    keysLevelPartA$keyName <- keyName
    keysLevelPartA$keyValue <- keyValue
    keysLevelPartA$keyKind <- "ge"
    keysLevelPartA$nCharacters <- nrow(partA)
    produceTree(partA, keysLevelPartA, depth - 1)
    keysLevelPartB <- keysLevel$AddChild(paste(keyName, "ls", keyValue, sep = ""))
    keysLevelPartB$keyName <- keyName
    keysLevelPartB$keyValue <- keyValue
    keysLevelPartB$keyKind <- "ls"
    keysLevelPartB$nCharacters <- nrow(partB)
    produceTree(partB, keysLevelPartB, depth - 1)
  } else {
    charactersTable <- as.data.frame(cbind(keysTableSubset[["minN"]]$characternumber,rownames(keysTableSubset[["minN"]])))
    names(charactersTable) <- c("number","name")
    keysLevel$charactersTable <- charactersTable
  }
}

classificationKeys <- Node$new("key")

produceTree(keysTable, classificationKeys, 10)

print(classificationKeys,"nCharacters")

fileName <- "dichotomouskey.tex"
write("", fileName)

classificationKeys$Do(function(x){
  if(length(x$children)>0){
    keyLabel <- x$pathString
    childALabel <- x$children[[1]]$pathString
    childBLabel <- x$children[[2]]$pathString
    childrenTrait <- x$children[[1]]$keyName
    childrenValue <- x$children[[1]]$keyValue
    questionLine <- paste(
      "\\identificationKey{",childrenTrait,"}","{",childrenValue,"}"
      ,"{",childALabel,"}","{",childBLabel,"}"
      ,"\\label{",keyLabel,"}", sep = "")
    write(questionLine, fileName, append = TRUE)
  } else {
    keyLabel <- x$pathString
    charactersTable <- x$charactersTable
    charNumbers <- charactersTable$number
    charNames  <- charactersTable$name
    resultLine <- paste(
      "\\identificationResult{", paste(charNumbers, collapse = ","),"}","{", paste(charNames, collapse = ","),"}"
      ,"\\label{",keyLabel,"}", sep = "")
    write(resultLine, fileName, append = TRUE)
  }
})
