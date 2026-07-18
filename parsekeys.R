install.packages("data.tree")

library(data.tree)

rawkeys <- read.csv("characterkeys.csv")
keysList <- unique(unlist(strsplit(rawkeys$characterkeys,"[0-9]")))
keysList <- keysList[2:length(keysList)]
keysTable <- as.data.frame(t(sapply(1:nrow(rawkeys), function(x){
  currentLine <- rawkeys[x,]
  currentKeys <- strsplit(currentLine$characterkeys, "(?<=..)", perl = TRUE)[[1]]
  currentKeysTable <- as.data.frame(t(sapply(currentKeys, function(y){strsplit(y, "")[[1]][c(2,1)]})))
  sapply(keysList, function(y){
    if (ncol(currentKeysTable) > 0){
      n <- sum(as.numeric(currentKeysTable[currentKeysTable[,1] == y,2]))
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
rownames(keysTable) <- rawkeys$charactercode
keysTable$characternumber <- rawkeys$characternumber


produceTree <- function(keysTableSubset, keysLevel, depth){
  splitTable <-as.data.frame(t(sapply(keysList, function(x){
    toHalf <- sapply(1:(max(keysTableSubset[,x])), function(y){
      abs(0.5 - sum(keysTableSubset[,x] >= y)/nrow(keysTableSubset))
    })
    minIndex <- which(toHalf == min(toHalf))[1]
    c(minIndex,toHalf[minIndex])
  })))
  names(splitTable) <- c("index","value")
  splitPosition <- splitTable[which(splitTable$value == min(splitTable$value))[1],]
  keyName <- rownames(splitPosition)
  keyValue <- splitPosition$index
  firstPart <- keysTableSubset[keysTableSubset[,keyName] >= keyValue,]
  secondPart <- keysTableSubset[keysTableSubset[,keyName] < keyValue,]

  if (
    nrow(firstPart) > 5
    & nrow(secondPart) > 5
  ) {
    keysLevel$charactersList <- NA
    keysLevelFirstPart <- keysLevel$AddChild(paste(keyName, "ge", keyValue, sep = ""))
    keysLevelFirstPart$keyName <- keyName
    keysLevelFirstPart$keyValue <- keyValue
    keysLevelFirstPart$keyKind <- "ge"
    keysLevelFirstPart$nCharacters <- nrow(firstPart)
    produceTree(firstPart, keysLevelFirstPart, depth+1)
    keysLevelSecondPart <- keysLevel$AddChild(paste(keyName, "ls", keyValue, sep = ""))
    keysLevelSecondPart$keyName <- keyName
    keysLevelSecondPart$keyValue <- keyValue
    keysLevelSecondPart$keyKind <- "ls"
    keysLevelSecondPart$nCharacters <- nrow(secondPart)
    produceTree(secondPart, keysLevelSecondPart, depth+1)
  } else {
    charactersTable <- as.data.frame(cbind(keysTableSubset$characternumber,rownames(keysTableSubset)))
    names(charactersTable) <- c("number","name")
    keysLevel$charactersTable <- charactersTable
  }
}

classificationKeys <- Node$new("key")

produceTree(keysTable, classificationKeys, 0)

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
