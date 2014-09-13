
########################################################################################
#### Companion code for YouTube video: https://www.youtube.com/watch?v=ee6T9ytzjyU
########################################################################################

# Load Titanic data set
# using dataset from the UCI Machine Learning Repository (http://archive.ics.uci.edu/ml/)
titanicDF <- read.csv('http://math.ucdenver.edu/RTutorial/titanic.txt',sep='\t')

# creating new title feature
titanicDF$Title <- ifelse(grepl('Mr ',titanicDF$Name),'Mr',ifelse(grepl('Mrs ',titanicDF$Name),'Mrs',ifelse(grepl('Miss',titanicDF$Name),'Miss','Nothing')))
titanicDF$Title <- as.factor(titanicDF$Title)

# impute age to remove NAs
titanicDF$Age[is.na(titanicDF$Age)] <- median(titanicDF$Age, na.rm=T)

# reorder data set so target is last column
titanicDF <- titanicDF[c('PClass', 'Age',    'Sex',   'Title', 'Survived')]

# binarize all factors
library(caret)
titanicDummy <- dummyVars("~.",data=titanicDF, fullRank=F)
titanicDF <- as.data.frame(predict(titanicDummy,titanicDF))

########################################################################################

# generalize your data set specific variables 
isclassification <- T
outcomeName <- 'Survived'
weightName <- ''
objDF <- titanicDF
predictors <- names(objDF)[!names(objDF) %in% c(outcomeName, weightName)]
finalVWfileName <- 'vw.txt'

########################################################################################

# https://github.com/JohnLangford/vowpal_wabbit/wiki/Input-format
# [Label] [Importance [Tag]]|Namespace Features |Namespace Features ... |Namespace Features

# LABELS & IMPORTANCE
if (is.null(labelName)) {
        outcomeName <- 'ignoreme'
        objDF[,outcomeName] <- "0 |"
} else {
        if (isclassification) {
                # everything should be -1,1 for classification
                objDF[,outcomeName] <- ifelse(objDF[,outcomeName]>0,1,-1)
        }
        
        if (weightName != '')
                objDF[,outcomeName] <- paste(objDF[,outcomeName], objDF[,weightName], "|")
        else
                objDF[,outcomeName] <- paste(objDF[,outcomeName], "|")
}


# Pairing column names with data... adding 1 blank character before each variable
for (i in predictors) {
        objDF[,i] <- ifelse(objDF[,i]==1,paste0(' ',i),
                            ifelse(objDF[,i]==0,'',paste0(' ', i,':', objDF[,i])))
}

# reorder columns so that label (outcome) is first followed by predictors     
objDF <- objDF[c(outcomeName, predictors)]

write.table(objDF, finalVWfileName, sep="", quote = F, row.names = F,  col.names = F)

########################################################################################

# Vowpal Wabbit Data Format Validation
http://hunch.net/~vw/validate.html
