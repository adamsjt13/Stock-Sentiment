rm(list = ls())
setwd("~/Documents/BZAN_583_Text_Mining/FinalProject/stocks/Final_Project_Files")

intel_news <- read.csv("articles_for_intel.csv", stringsAsFactors = FALSE)
intel_stock_data <- read.csv("INTL_stock_data.csv")
apple_news <- read.csv("articles_for_apple.csv", stringsAsFactors = FALSE)
apple_stock_data <- read.csv("AAPL_stock_data.csv")
facebook_news <- read.csv("articles_for_facebook.csv", stringsAsFactors = FALSE)
facebook_stock_data <- read.csv("FB_stock_data.csv")

############ build classifier ############
require(stringr)
require(rvest)
require(ngram)
require(tm)
require(SnowballC)
require(AUC)
require(e1071)
require(randomForest)

### combine all company articles
combined_articles <- rbind(intel_news,apple_news,facebook_news)
### remove date
combined_articles$Date <- NULL
### removed quotes
combined_articles$Article <- gsub('"', '', combined_articles$Article)
### create tags from sentiment score for classifier
combined_articles$Sentiment <- ifelse(combined_articles$Sentiment >= 0, 'pos','neg')
### convert to ASCII to remove weird symbols
combined_articles$Article <- iconv(combined_articles$Article, to = "ASCII//TRANSLIT")
### make corpus of symptom text for clustering
articles_corp <- Corpus(VectorSource(combined_articles$Article))
### clean corpus and make DTM
dtm <- DocumentTermMatrix(articles_corp, control=list(removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      tolower = TRUE,
                                                      stemming = TRUE,
                                                      stopwords = stopwords("SMART"),
                                                      minDocFreq=1,
                                                      minWordLength = 1))
### Reduce sparse terms
dtm_dense <- removeSparseTerms(dtm, 0.99)

### Weighting Terms by TF-IDF
dtm_tfxidf <- suppressWarnings(weightTfIdf(dtm_dense))

# trainind <- allind[1:round(length(allind)/3)]
# valind <- allind[(round(length(allind)/3)+1):round(length(allind)*(2/3))] 
# testind <- allind[round(length(allind)*(2/3)+1):length(allind)]

### basetable
basetable <- as.matrix(dtm_tfxidf)
### class
y <- factor(combined_articles$Sentiment)

results_df <- data.frame("SVM AUC" = rep(0,5), 
                         "SVM ACC" = rep(0,5), 
                         "SVM CORRECT" = rep(0,5),
                         "NB AUC" = rep(0,5), 
                         "NB ACC" = rep(0,5),
                         "NB CORRECT" = rep(0,5),
                         "RF AUC" = rep(0,5),
                         "RF ACC" = rep(0,5),
                         "RF CORRECT" = rep(0,5))
rownames(results_df) <- c("80/20","70/30","5Fold","10Fold","15Fold")


################### 80/20 split ###################
bins <- cut(seq(1,nrow(basetable)),breaks=10,labels=FALSE)

allind <- sample(x=1:nrow(basetable),size=nrow(basetable))
trainind <- allind[which(bins %in% 1:4)]
valind <- allind[which(bins %in% 5:8)]
testind <- allind[which(bins %in% 9:10)]

basetabletrain <- basetable[trainind,]
basetableval <- basetable[valind,]
basetabletest <- basetable[testind,]
basetabletrainbig <- rbind(basetabletrain,basetableval)

ytrain <- y[trainind]
yval <- y[valind]
ytest <- y[testind]
ytrainbig <- factor(c(as.character(ytrain),as.character(yval)))

### SVM 80/20 split
SV.cost <- 2^(-5:-4) 
SV.gamma <- 2^(-15:-14) 
SV.degree <- c(1,2)
SV.kernel <- c('polynomial')

parameters <- expand.grid("Cost" = SV.cost,
                          "Gamma" = SV.gamma,
                          "Degree" = SV.degree,
                          "Kernel" = SV.kernel)
aucstore <- numeric(nrow(parameters))
for(i in 1:nrow(parameters)){
  start <- Sys.time()
  model <- svm(basetabletrain, 
               ytrain,
               type = "C-classification",
               probability = TRUE, 
               kernel = parameters$Kernel[i],
               degree = parameters$Degree[i],
               cost = parameters$Cost[i],
               gamma = parameters$Gamma[i])
  pred_prob <- predict(model, basetableval, decision.values = TRUE, probability = TRUE)
  print(i)
  aucstore[i] <- AUC::auc(roc(pred_prob,yval))
}

optimal <- parameters[which.max(aucstore),]
model <- svm(basetabletrainbig, 
             ytrainbig,
             type = "C-classification",
             probability = TRUE, 
             kernel = optimal$Kernel,
             degree = optimal$Degree,
             cost = optimal$Cost,
             gamma = optimal$Gamma)
pred_prob <- predict(model, basetabletest, decision.values = TRUE, probability = TRUE)
x <- table(pred_prob,ytest)
(svm_auc <- AUC::auc(roc(pred_prob,ytest)))

results_df["80/20","SVM.AUC"] <- AUC::auc(roc(pred_prob,ytest))
results_df["80/20","SVM.ACC"] <- sum(diag(x))/sum(x)
results_df["80/20","SVM.CORRECT"] <- sum(diag(x))

### NB
allind <- sample(x=1:nrow(basetable),size=nrow(basetable))
trainind <- allind[which(bins %in% 1:8)]
testind <- allind[-trainind]

basetabletrain <- basetable[trainind,]
basetabletest <- basetable[testind,]

ytrain <- y[trainind]
ytest <- y[testind]

NB <- naiveBayes(x=basetabletrain, y=ytrain)
predNB <- predict(NB,basetabletest, type = "class", threshold = 0.001)
NB_table <- table(predNB,ytest)
nb_accuracy <- sum(diag(NB_table)) / sum(NB_table)
(nb_auc <- AUC::auc(roc(predNB,ytest)))

results_df["80/20","NB.AUC"] <- AUC::auc(roc(predNB,ytest))
results_df["80/20","NB.ACC"] <- sum(diag(NB_table))/sum(NB_table)
results_df["80/20","NB.CORRECT"] <- sum(diag(NB_table))

### RF
rf <- randomForest(x = basetabletrain,
                   y = ytrain, 
                   ntree=500)
predrf <- predict(rf, basetabletest, type="class")
rf_table <- table(predrf, ytest)
rf_accuracy <- sum(diag(rf_table)) / sum(rf_table)
(rf_auc <- AUC::auc(roc(predrf, ytest)))

results_df["80/20","RF.AUC"] <- AUC::auc(roc(predrf,ytest))
results_df["80/20","RF.ACC"] <- sum(diag(rf_table))/sum(rf_table)
results_df["80/20","RF.CORRECT"] <- sum(diag(rf_table))

write.csv(results_df, "results.csv")

################### 70/20 split ###################
bins <- cut(seq(1,nrow(basetable)),breaks=10,labels=FALSE)

allind <- sample(x=1:nrow(basetable),size=nrow(basetable))
trainind <- allind[which(bins %in% 1:4)]
valind <- allind[which(bins %in% 5:7)]
testind <- allind[which(bins %in% 8:10)]

basetabletrain <- basetable[trainind,]
basetableval <- basetable[valind,]
basetabletest <- basetable[testind,]
basetabletrainbig <- rbind(basetabletrain,basetableval)

ytrain <- y[trainind]
yval <- y[valind]
ytest <- y[testind]
ytrainbig <- factor(c(as.character(ytrain),as.character(yval)))

### SVM 70/30 split
SV.cost <- 2^(-5:-4) 
SV.gamma <- 2^(-15:-14) 
SV.degree <- c(1,2)
SV.kernel <- c('polynomial')

parameters <- expand.grid("Cost" = SV.cost,
                          "Gamma" = SV.gamma,
                          "Degree" = SV.degree,
                          "Kernel" = SV.kernel)
aucstore <- numeric(nrow(parameters))
for(i in 1:nrow(parameters)){
  start <- Sys.time()
  model <- svm(basetabletrain, 
               ytrain,
               type = "C-classification",
               probability = TRUE, 
               kernel = parameters$Kernel[i],
               degree = parameters$Degree[i],
               cost = parameters$Cost[i],
               gamma = parameters$Gamma[i])
  pred_prob <- predict(model, basetableval, decision.values = TRUE, probability = TRUE)
  print(i)
  aucstore[i] <- AUC::auc(roc(pred_prob,yval))
}

optimal <- parameters[which.max(aucstore),]
model <- svm(basetabletrainbig, 
             ytrainbig,
             type = "C-classification",
             probability = TRUE, 
             kernel = optimal$Kernel,
             degree = optimal$Degree,
             cost = optimal$Cost,
             gamma = optimal$Gamma)
pred_prob <- predict(model, basetabletest, decision.values = TRUE, probability = TRUE)
x <- table(pred_prob,ytest)
(svm_auc <- AUC::auc(roc(pred_prob,ytest)))

results_df["70/30","SVM.AUC"] <- AUC::auc(roc(pred_prob,ytest))
results_df["70/30","SVM.ACC"] <- sum(diag(x))/sum(x)
results_df["70/30","SVM.CORRECT"] <- sum(diag(x))

### NB
allind <- sample(x=1:nrow(basetable),size=nrow(basetable))
trainind <- allind[which(bins %in% 1:7)]
testind <- allind[-trainind]

basetabletrain <- basetable[trainind,]
basetabletest <- basetable[testind,]

ytrain <- y[trainind]
ytest <- y[testind]

NB <- naiveBayes(x=basetabletrain, y=ytrain)
predNB <- predict(NB,basetabletest, type = "class", threshold = 0.001)
NB_table <- table(predNB,ytest)
nb_accuracy <- sum(diag(NB_table)) / sum(NB_table)
(nb_auc <- AUC::auc(roc(predNB,ytest)))

results_df["70/30","NB.AUC"] <- AUC::auc(roc(predNB,ytest))
results_df["70/30","NB.ACC"] <- sum(diag(NB_table))/sum(NB_table)
results_df["70/30","NB.CORRECT"] <- sum(diag(NB_table))

### RF
rf <- randomForest(x = basetabletrain,
                   y = ytrain, 
                   ntree=500)
predrf <- predict(rf, basetabletest, type="class")
rf_table <- table(predrf, ytest)
rf_accuracy <- sum(diag(rf_table)) / sum(rf_table)
(rf_auc <- AUC::auc(roc(predrf, ytest)))

results_df["70/30","RF.AUC"] <- AUC::auc(roc(predrf,ytest))
results_df["70/30","RF.ACC"] <- sum(diag(rf_table))/sum(rf_table)
results_df["70/30","RF.CORRECT"] <- sum(diag(rf_table))

write.csv(results_df, "results.csv")

### cross fold validation
numbreaks <- c(5,10,15)

for(j in numbreaks){
  bins <- cut(seq(1,nrow(basetable)),breaks=j,labels=FALSE)
  svm_temp_auc <- svm_temp_acc <- svm_temp_correct <-
    nb_temp_auc <- nb_temp_acc <- nb_temp_correct <-
    rf_temp_auc <- rf_temp_acc <- rf_temp_correct <- numeric(j)
  
  print(paste0(j,"-fold validation"))
  
  for(i in 1:j){
    print(paste0("Fold: ",i))
    allind <- sample(x=1:nrow(basetable),size=nrow(basetable))
    alltrainind <- allind[which(bins != i)]
    trainind <- alltrainind[1:round(length(alltrainind)/2)]
    valind <- alltrainind[(round(length(alltrainind)/2)+1):length(alltrainind)]
    testind <- allind[which(bins == i)]

    basetabletrain <- basetable[trainind,]
    basetableval <- basetable[valind,]
    basetabletest <- basetable[testind,]
    basetabletrainbig <- rbind(basetabletrain,basetableval)

    ytrain <- y[trainind]
    yval <- y[valind]
    ytest <- y[testind]
    ytrainbig <- factor(c(as.character(ytrain),as.character(yval)))

    ### SVM
    SV.cost <- 2^(-5:-4)
    SV.gamma <- 2^(-15:-14)
    SV.degree <- c(1,2)
    SV.kernel <- c('polynomial')

    parameters <- expand.grid("Cost" = SV.cost,
                              "Gamma" = SV.gamma,
                              "Degree" = SV.degree,
                              "Kernel" = SV.kernel)
    aucstore <- numeric(nrow(parameters))
    for(k in 1:nrow(parameters)){
      model <- svm(basetabletrain,
                   ytrain,
                   type = "C-classification",
                   probability = TRUE,
                   kernel = parameters$Kernel[k],
                   degree = parameters$Degree[k],
                   cost = parameters$Cost[k],
                   gamma = parameters$Gamma[k])
      pred_prob <- predict(model, basetableval, decision.values = TRUE, probability = TRUE)
      x <- table(pred_prob,yval)
      aucstore[k] <- AUC::auc(roc(pred_prob,yval))
      print(paste0("SVM Parameter: ",k))
    }

    optimal <- parameters[which.max(aucstore),]
    model <- svm(basetabletrainbig,
                 ytrainbig,
                 type = "C-classification",
                 probability = TRUE,
                 kernel = optimal$Kernel,
                 degree = optimal$Degree,
                 cost = optimal$Cost,
                 gamma = optimal$Gamma)
    pred_prob <- predict(model, basetabletest, decision.values = TRUE, probability = TRUE)
    x <- table(pred_prob,ytest)
    (svm_auc <- AUC::auc(roc(pred_prob,ytest)))

    svm_temp_auc[i] <- AUC::auc(roc(pred_prob,ytest))
    svm_temp_acc[i] <- sum(diag(x))/sum(x)
    svm_temp_correct[i] <- sum(diag(x))
    
    ### new sample for just test/train
    allind <- sample(x=1:nrow(basetable),size=nrow(basetable))
    trainind <- allind[which(bins != i)]
    testind <- allind[which(bins == i)]
    
    basetabletrain <- basetable[trainind,]
    basetabletest <- basetable[testind,]
    
    ytrain <- y[trainind]
    ytest <- y[testind]
    
    ### NB
    print("Naive Bayes")
    NB <- naiveBayes(x=basetabletrain, y=ytrain)
    predNB <- predict(NB,basetabletest, type = "class", threshold = 0.001)
    NB_table <- table(predNB,ytest)
    nb_accuracy <- sum(diag(NB_table)) / sum(NB_table)
    (nb_auc <- AUC::auc(roc(predNB,ytest)))
    
    nb_temp_auc[i] <- AUC::auc(roc(predNB,ytest))
    nb_temp_acc[i] <- sum(diag(NB_table))/sum(NB_table)
    nb_temp_correct[i] <- sum(diag(NB_table))
    
    ### RF
    print("Random Forest")
    rf <- randomForest(x = basetabletrain,
                       y = ytrain, 
                       ntree=500)
    predrf <- predict(rf, basetabletest, type="class")
    rf_table <- table(predrf, ytest)
    rf_accuracy <- sum(diag(rf_table)) / sum(rf_table)
    (rf_auc <- AUC::auc(roc(predrf, ytest)))
    
    rf_temp_auc[i] <- AUC::auc(roc(predrf,ytest))
    rf_temp_acc[i] <- sum(diag(rf_table))/sum(rf_table)
    rf_temp_correct[i] <- sum(diag(rf_table))
    
  }
  
  if(j == 5){
    # results_df["5Fold","SVM.AUC"] <- median(svm_temp_auc)
    # results_df["5Fold","SVM.ACC"] <- median(svm_temp_acc)
    # results_df["5Fold","SVM.CORRECT"] <- median(svm_temp_correct)
    
    results_df["5Fold","NB.AUC"] <- median(nb_temp_auc)
    results_df["5Fold","NB.ACC"] <- median(nb_temp_acc)
    results_df["5Fold","NB.CORRECT"] <- median(nb_temp_correct)
    
    results_df["5Fold","RF.AUC"] <- median(rf_temp_auc)
    results_df["5Fold","RF.ACC"] <- median(rf_temp_acc)
    results_df["5Fold","RF.CORRECT"] <- median(rf_temp_correct)
  } else if(j == 10){
    # results_df["10Fold","SVM.AUC"] <- median(svm_temp_auc)
    # results_df["10Fold","SVM.ACC"] <- median(svm_temp_acc)
    # results_df["10Fold","SVM.CORRECT"] <- median(svm_temp_correct)
    
    results_df["10Fold","NB.AUC"] <- median(nb_temp_auc)
    results_df["10Fold","NB.ACC"] <- median(nb_temp_acc)
    results_df["10Fold","NB.CORRECT"] <- median(nb_temp_correct)
    
    results_df["10Fold","RF.AUC"] <- median(rf_temp_auc)
    results_df["10Fold","RF.ACC"] <- median(rf_temp_acc)
    results_df["10Fold","RF.CORRECT"] <- median(rf_temp_correct)
  } else {
    # results_df["15Fold","SVM.AUC"] <- median(svm_temp_auc)
    # results_df["15Fold","SVM.ACC"] <- median(svm_temp_acc)
    # results_df["15Fold","SVM.CORRECT"] <- median(svm_temp_correct)
    
    results_df["15Fold","NB.AUC"] <- median(nb_temp_auc)
    results_df["15Fold","NB.ACC"] <- median(nb_temp_acc)
    results_df["15Fold","NB.CORRECT"] <- median(nb_temp_correct)
    
    results_df["15Fold","RF.AUC"] <- median(rf_temp_auc)
    results_df["15Fold","RF.ACC"] <- median(rf_temp_acc)
    results_df["15Fold","RF.CORRECT"] <- median(rf_temp_correct)
  }
  
  write.csv(results_df, "results.csv")
}

################### SAVE MODEL WITH HIGHEST PERFORMANCE (RF WITH 80/20 SPLIT) ###################

### basetable
basetable <- as.matrix(dtm_tfxidf)
### class
y <- factor(combined_articles$Sentiment)

################### 80/20 split ###################
bins <- cut(seq(1,nrow(basetable)),breaks=10,labels=FALSE)

allind <- sample(x=1:nrow(basetable),size=nrow(basetable))
trainind <- allind[which(bins %in% 1:8)]
testind <- allind[-trainind]

basetabletrain <- basetable[trainind,]
basetabletest <- basetable[testind,]

ytrain <- y[trainind]
ytest <- y[testind]

### RF
rf <- randomForest(x = basetabletrain,
                   y = ytrain, 
                   ntree=500)
predrf <- predict(rf, basetabletest, type="class")
rf_table <- table(predrf, ytest)
(rf_accuracy <- sum(diag(rf_table)) / sum(rf_table))
(rf_auc <- AUC::auc(roc(predrf, ytest)))

save(rf,file = "rf.RData")

