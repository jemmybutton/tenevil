library(rpart)

rawkeys <- read.csv("characterkeys.csv")
keysList <- unique(toupper(unlist(strsplit(rawkeys$characterkeys,"[0-9]"))))
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
keysTable$charactercode <- rawkeys$charactercode
keysTable$characternumber <- rawkeys$characternumber

binaryKeysTable <- keysTable
binaryKeysTable[,keysList] <- binaryKeysTable[,keysList] > 0

fit <- rpart(charactercode~.-characternumber,
    data = binaryKeysTable, method = "class",
    parms = list(split = "gini"),
      control = rpart.control(
        maxdepth = 20,
        cp = 0.00001,
        minsplit = 10,
        minbucket = 3,
        maxcompete = 20, maxsurrogate = 20, xval = 20))

splitsVector <- cumsum(fit$frame$ncompete+fit$frame$nsurrogate+1-(fit$frame$var == "<leaf>"))

getSplitDirection <- function(x){
  if(fit$frame$var[x] != "<leaf>"){
    if (x == 1){
      splitStart <- 1
    } else {
      splitStart <- splitsVector[x-1]+1
    }
    splitEnd <- splitsVector[x]
    trait <- fit$frame$var[x]
    rv <- fit$splits[splitStart:splitEnd,"ncat"]
    if (length(rv) > 1){
      rv <- rv[trait]
    }
  } else {
    rv <- NA
  }
  rv
}

fileName <- "dichotomouskey.tex"
write("", fileName)

parseTree <- function(nodeNumberInFrame, parentNodeNumber, includedTraits, excludedTraits, depth){
  currentRow <- fit$frame[nodeNumberInFrame,]
  nodeNumber <- as.numeric(rownames(currentRow))
  currentKey <- currentRow$var
  if (currentKey != "<leaf>"){
    if (getSplitDirection(nodeNumberInFrame) == -1){
      refA <- 2*nodeNumber+1
      refB <- 2*nodeNumber
    } else {
      refA <- 2*nodeNumber
      refB <- 2*nodeNumber+1
    }
    questionLine <- paste(
      "\\identificationKey{",depth,"}{",currentKey,"}","{",1,"}"
      ,"{",paste("node",refA,sep=""),"}"
      ,"{",paste("node",refB,sep=""),"}"
      ,"{",paste("node",parentNodeNumber,sep=""),"}"
      ,"{",paste(includedTraits,collapse=""),"}"
      ,"{",paste(excludedTraits,collapse=""),"}"
      ,"\\label{",paste("node",nodeNumber,sep=""),"}", sep = "")
    nodeNumbers <- rownames(fit$frame)
    #print(questionLine)
    write(questionLine, fileName, append = TRUE)
    parseTree(
      nodeNumberInFrame = which(nodeNumbers == refA),
      parentNodeNumber = nodeNumber,
      includedTraits = c(includedTraits, currentKey),
      excludedTraits = excludedTraits,
      depth = depth + 1)
    parseTree(
      nodeNumberInFrame = which(nodeNumbers == refB),
      parentNodeNumber = nodeNumber,
      includedTraits = includedTraits,
      excludedTraits = c(excludedTraits, currentKey),
      depth = depth + 1)
  } else {
    charactersTable <- unique(keysTable[fit$where == nodeNumberInFrame, c("charactercode", "characternumber")])
    resultLine <- paste(
      "\\identificationResult{",depth,"}{", paste(charactersTable$characternumber, collapse = ","),"}","{", paste(charactersTable$charactercode, collapse = ","),"}"
      ,"{",paste("node",parentNodeNumber,sep=""),"}"
      ,"{",paste(includedTraits,sep="",collapse=""),"}"
      ,"{",paste(excludedTraits,sep="",collapse=""),"}"
      ,"\\label{",paste("node",nodeNumber,sep=""),"}", sep = "")
    #print(resultLine)
    write(resultLine, fileName, append = TRUE)
  }
}

parseTree(1, 0, c(), c(), 0)
