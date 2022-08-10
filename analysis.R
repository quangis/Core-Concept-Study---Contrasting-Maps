## [SC] before running this code make sure "source" variable points to the folder containing the datafiles

## [SC] CHANGE THIS VARIABLE
source <- ""

#####################################################################################

# [SC] dplyr is for descriptive summary for group stats
libraries <- c("stringr", "dplyr", "ggplot2", "rstatix", "FSA", "gplots")

for(mylibrary in libraries){
  ## [SC] installing gplots package
  if (!(mylibrary %in% rownames(installed.packages()))) {
    install.packages(mylibrary)
  }
  library(mylibrary, character.only = TRUE)
}

se <- function(x) { 
  sd(x) / sqrt(length(x))
}

## [SC] remove extra column names
cleanHeader <- function(){
  rawDataDF <- read.csv(paste0(source, "Core_Concept_Study3Output2_December 8, 2021_08.18.csv"), stringsAsFactors=FALSE, header=TRUE)
  ## [SC] remove extra column names
  rawDataDF <- rawDataDF[-c(1,2),]
  
  write.csv(rawDataDF, paste0(source, "Core_Concept_Study3Output2_December 8_clean.csv"), row.names=FALSE)
}

analyzeQOnePooled <- function(){
  # [SC] load reference data for Q1
  qOneRefDF <- read.csv(paste0(source, "qOneReferenceData.csv"), stringsAsFactors=FALSE, header=TRUE)
  
  qcolnames <- paste0("Q1_", 1:36)
    
  # [SC] all responses for Q1 are stored here
  responsesDF <- data.frame(partId=NA, expertise=NA, cohort=NA, qId=NA, opOne=NA, opTwo=NA, opThree=NA, correct=NA)
  
  #################################################################################################
  ## [SC] parse responses from the qualtrics survey
  
  mainRawDataDF <- read.csv(paste0(source, "Core_Concept_Study3Output2_December 8_clean.csv"), stringsAsFactors=FALSE, header=TRUE)
  
  ## [SC] include only consented results
  mainRawDataDF <- subset(mainRawDataDF, Consent == "Yes, I consent in participating in the research study")
  
  ## [SC] exclude any laymen?
  ## [SC] 2 students from GIMinor and 23 students from INFOMSDASM identified as Laymen
  #mainRawDataDF <- subset(mainRawDataDF, expertise != "GIS layman: No previous contact with GIS")
  
  ## [SC] remove late INFOMSDASM responses
  ## [SC] parse datetime in format "%Y-%m-%d %H:%M:%S"
  mainRawDataDF$EndDate <- as.POSIXct(mainRawDataDF$EndDate)
  mainRawDataDF <- subset(mainRawDataDF, EndDate < "2021-11-20")
  
  
  ############################################################
  ## [TODO] need to make sure there are no duplicate responses
  ############################################################
  
  
  ## [SC] include only finished results
  #rawDataDF <- subset(rawDataDF, Finished == "True")
  
  # [SC] identify and select participants who finished Q1
  mainRawDataDF <- cbind(mainRawDataDF, qOneCount=0)
  for(partIndex in 1:nrow(mainRawDataDF)){
    # [SC] iterate through the Q1 responses
    for(colName in qcolnames){
      if (mainRawDataDF[partIndex,colName] != "" && !is.na(mainRawDataDF[partIndex,colName])){
        mainRawDataDF[partIndex,"qOneCount"] <- mainRawDataDF[partIndex,"qOneCount"] + 1
      }
    }
  }
  mainRawDataDF <- subset(mainRawDataDF, qOneCount == 18)
  

  expertIds <- c(927806,570347,989157,151764,590510,857400,808338
                 ,855723,81759,881096,755905,200685,128948,431246
                 ,222234,943406,610375,270770,155668,249782,711072
                 ,188312,352569,563043,897147,476721,872120,232653
                 ,965719,470663,744965,315110,554654,608747,687942
                 ,989055,375581,653130,680385,307515,309911,782841
                 ,744479,817592)
  
  # [SC] identity participant cohort: expert or students; exclude all other participant data
  mainRawDataDF <- cbind(mainRawDataDF, cohort=NA)
  for(partIndex in 1:nrow(mainRawDataDF)){
     if (mainRawDataDF$Source[partIndex] %in% as.character(expertIds)){
       mainRawDataDF$cohort[partIndex] <- "skilled"
     }
     else if (mainRawDataDF$Source[partIndex] == "GIMinor"){
       mainRawDataDF$cohort[partIndex] <- "student"
     }
     else if (mainRawDataDF$Source[partIndex] == "INFOMSDASMpre"){
       mainRawDataDF$cohort[partIndex] <- "student"
     }
  }
  mainRawDataDF <- subset(mainRawDataDF, !is.na(cohort))
  
  
  # [SC] parse the Q1 responses
  for(colName in qcolnames){
    ## [SC] extract responses for this question
    qDF <- subset(mainRawDataDF, mainRawDataDF[,colName] != "")
    
    if(nrow(qDF) > 0){
      ## [SC] get reference data for this question
      refQDF <- subset(qOneRefDF, qId == colName)
      
      for(responseIndex in 1:nrow(qDF)){
        response <- qDF[responseIndex, colName]
        
        opOneVal <- 0
        opTwoVal <- 0
        opThreeVal <- 0
        correctVal <- 0
        
        if (grepl(refQDF$opOne, response, fixed = TRUE)){
          opOneVal <- 1
        }
        
        if (grepl(refQDF$opTwo, response, fixed = TRUE)){
          opTwoVal <- 1
        }
        
        if (grepl(refQDF$opThree, response, fixed = TRUE)){
          opThreeVal <- 1
        }
        
        if (grepl(refQDF$correct, response, fixed = TRUE)){
          correctVal <- 1
        }
        
        responsesDF <- rbind(responsesDF, data.frame(partId=qDF[responseIndex, "ResponseId"]
                                                     , expertise=qDF[responseIndex, "expertise"]
                                                     , cohort=qDF[responseIndex, "cohort"]
                                                     , qId=colName
                                                     , opOne=opOneVal, opTwo=opTwoVal
                                                     , opThree=opThreeVal, correct=correctVal))
      }
    }
  }
  responsesDF <- responsesDF[-1,]
  
  #################################################################################################
  ## [SC] parse responses from the google surveys
  
  glRawDataDF <- read.csv(paste0(source, "google-survey-dccsl6yehegbsjdbgvip3qb7se-final.csv"), stringsAsFactors=FALSE, header=TRUE)
  
  qRefDF <- data.frame(qName=c("Question..2.Answer", "Question..3.Answer"
                               , "Question..4.Answer", "Question..5.Answer"
                               , "Question..6.Answer", "Question..7.Answer")
                       , qShort=c("Q1_15", "Q1_14", "Q1_16", "Q1_17", "Q1_18", "Q1_13")
                       , stringsAsFactors = FALSE)
  
  expertColName <- "Question..1.Answer"
  
  skillDF <- data.frame(full=c("*Beginner*: can use basic GIS functions"
                               , "*Trained*: formally trained by a GIS course"
                               , "*Expert*: used GIS for 5 years or more")
                        , short=c("Beginner", "Trained", "Expert"))
  
  
  # [SC] filter out participants familiar with the core concepts
  glRawDataDF <- subset(glRawDataDF, Question..8.Answer == "No")
  
  # [SC] aggregate all responses to responsesDF
  for(rowIndex in 1:nrow(qRefDF)){
    qName <- qRefDF$qName[rowIndex]
    qShort <- qRefDF$qShort[rowIndex]
    
    ## [SC] get reference data for this question
    refQDF <- subset(qOneRefDF, qId == qShort)
    
    # [SC] iterate through the participants' responses for the current question
    for(dRowIndex in 1:nrow(glRawDataDF)){
      response <- glRawDataDF[dRowIndex, qName]
      
      opOneVal <- 0
      opTwoVal <- 0
      opThreeVal <- 0
      correctVal <- 0
      
      response <- glRawDataDF[dRowIndex, qName]
      if (grepl(refQDF$opOne, response, fixed = TRUE)){
        opOneVal <- 1
      }
      
      response <- glRawDataDF[dRowIndex, qName]
      if (grepl(refQDF$opTwo, response, fixed = TRUE)){
        opTwoVal <- 1
      }
      
      response <- glRawDataDF[dRowIndex, qName]
      if (grepl(refQDF$opThree, response, fixed = TRUE)){
        opThreeVal <- 1
      }
      
      response <- glRawDataDF[dRowIndex, qName]
      if (grepl(refQDF$correct, response, fixed = TRUE)){
        correctVal <- 1
      }
      
      responsesDF <- rbind(responsesDF, data.frame(partId=glRawDataDF[dRowIndex, "User.ID"]
                                                   , expertise=glRawDataDF[dRowIndex, expertColName]
                                                   , cohort="general"
                                                   , qId=qShort
                                                   , opOne=opOneVal, opTwo=opTwoVal
                                                   , opThree=opThreeVal, correct=correctVal))
    }
  }
  
  #################################################################################################
  ## [SC] finalize the result table
  
  responsesDF[grepl("layman", responsesDF$expertise, fixed = TRUE), "expertise"] <- "Layman"
  responsesDF[grepl("Beginner", responsesDF$expertise, fixed = TRUE), "expertise"] <- "Beginner"
  responsesDF[grepl("Trained", responsesDF$expertise, fixed = TRUE), "expertise"] <- "Trained"
  responsesDF[grepl("Expert", responsesDF$expertise, fixed = TRUE), "expertise"] <- "Expert"
  
  responsesDF <- merge(responsesDF, qOneRefDF[,c("qId","type","group","cc")])
  responsesDF <- cbind(responsesDF, freq=1)
  
  qIdShortDF <- data.frame(qId=paste0("Q1_", c(1:36)), qIdShort=c(paste0("Q0", c(1:9)), paste0("Q", c(10:36))))
  responsesDF <- merge(responsesDF, qIdShortDF)
  
  
  #################################################################################################
  ## [SC] analyze only RPRO responses to compare to the responses from the general cohort
  
  if(TRUE){
    regionDF <- subset(responsesDF, group == "RPRO")
    
    ## [SC] this code is to test whether Beginner participants from the general cohort 
    ## are better than the Beginner participants from the student cohort.
    ## Accuracies are 26% for general and 25% for student
    #regionDF <- subset(regionDF, !(cohort == "general" & (expertise == "Trained" | expertise == "Expert")))
    
    print("Region only - Number of participants in each cohort:")
    respCountDF <- aggregate(freq ~ cohort, regionDF, sum)
    respCountDF$freq <- respCountDF$freq/6
    print(respCountDF)
    print("")
    
    print("Region only - Overall accuracy % for each cohort:")
    overallDF <- aggregate(correct ~ cohort, regionDF, mean)
    overallDF$correct <- round(overallDF$correct * 100, 0)
    overallDF <- cbind(overallDF, qIdShort="overall")
    overallDF <- cbind(overallDF, cc="both")
    print(overallDF)
    print("")
    
    cexVal=1.5
    # [SC] analyze expertise distribution within each cohort
    print("Region only - Distribition of expertise by cohorts:")
    experiseDF <- aggregate(freq ~ cohort + expertise, regionDF, sum)
    experiseDF$freq <- experiseDF$freq/6
    experiseDF$cohort <- factor(experiseDF$cohort, levels=c("general", "student", "skilled"))
    experiseDF$expertise <- factor(experiseDF$expertise, levels=c("Layman", "Beginner", "Trained", "Expert"))
    par(mfrow=c(1, 4), oma=c(0,0,0,0), mar=c(4,4,2,1)) # b l t r
    #barplot(freq ~ expertise + cohort, data=experiseDF, beside=TRUE
    #        , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
    #        , legend.text=TRUE, ylab="Sample size", xlab=""
    #        , main="Sample size per condition")
    print(experiseDF[order(experiseDF$cohort),])
    print("")
    
    # [SC] calculate accuracy per participant
    partAccDF <- aggregate(correct ~ partId + cohort + expertise, regionDF, sum)
    
    # [SC] plot mean and sd of average accuracies by cohort
    partAccMeanDF <- aggregate(correct ~ cohort, partAccDF, mean)
    colnames(partAccMeanDF)[colnames(partAccMeanDF)=="correct"] <- "correctMean"
    partAccMeanDF <- merge(partAccMeanDF, aggregate(correct ~ cohort, partAccDF, se))
    partAccMeanDF <- partAccMeanDF[order(factor(partAccMeanDF$cohort, levels = c("general", "student", "skilled"))),]
    barplot2(partAccMeanDF$correctMean, names.arg=partAccMeanDF$cohort, beside = TRUE
             , plot.ci = TRUE, ci.l = partAccMeanDF$correctMean-partAccMeanDF$correct
             , ci.u = partAccMeanDF$correctMean+partAccMeanDF$correct
             , main="Mean of correct answers", xlab="cohort", ylab="# of correct answers"
             , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
             , ylim=c(0, 4.5))
    # [SC] plot accuracy distribution for each cohort
    print("Region only - Distribution of correct answers per participant by cohorts")
    histBreaks <- c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5)
    hist(subset(partAccDF, cohort=="general")$correct, breaks=histBreaks
         , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
         , main="General cohort", ylab="Frequency", xlab="# of correct answers")
    hist(subset(partAccDF, cohort=="student")$correct, breaks=histBreaks
         , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
         , main="Student cohort", ylab="Frequency", xlab="# of correct answers")
    hist(subset(partAccDF, cohort=="skilled")$correct, breaks=histBreaks
         , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
         , main="Skilled cohort", ylab="Frequency", xlab="# of correct answers")
    
    
    ## [SC] summarize per participant accuracies by cohorts
    partAccDF$cohort <- ordered(partAccDF$cohort, levels = c("general", "student", "skilled"))
    sumResult <- group_by(partAccDF, cohort) %>%
      summarise(
        count = n(),
        mean = mean(correct, na.rm = TRUE),
        sd = sd(correct, na.rm = TRUE),
        median = median(correct, na.rm = TRUE),
        IQR = IQR(correct, na.rm = TRUE)
      )
    print(sumResult)
    
    ## [SC] non-parametric alternative for One-way ANOVA for indepenedent groups 
    ## [SC] on per participant accuracies by cohorts
    print(kruskal.test(correct ~ cohort, data = partAccDF))
    ## [SC] follow-up pairwise tests with Holm-Bonferroni P-value adjustment on per participant accuracies between cohorts
    ## [SC the Dunn's test is for independent groups
    print(dunn_test(correct ~ cohort, data = partAccDF, p.adjust.method = "bonferroni")) # [SC] uses rstatix package
    #print(dunnTest(correct ~ cohort, data=partAccDF, method="bonferroni")) # [SC] uses FSA package
    ## [SC] follow-up pairwise tests with Holm-Bonferroni P-value adjustment on per participant accuracies between cohorts
    ## [SC] not sure if this is an implementation for indepependent groups
    #print(pairwise.wilcox.test(partAccDF$correct, partAccDF$cohort, p.adjust.method = "BH"))
    print("")
    
    
    # [SC] calculate accuracy for each cohort-question pair
    qAggrDF <- aggregate(correct ~ cohort + qIdShort + cc, regionDF, mean)
    qAggrDF$correct <- round(qAggrDF$correct * 100, 0)
    
    qAggrDF <- rbind(qAggrDF, overallDF)
    qAggrDF$cohort <- factor(qAggrDF$cohort, levels=c("general", "student", "skilled"))
    qAggrDF$qIdShort <- factor(qAggrDF$qIdShort)
    #par(mfrow=c(1,1))
    #barplot(correct ~ cohort + qIdShort, data=qAggrDF, beside=TRUE, ylim=c(0, 100)
    #        , legend.text=TRUE, ylab="Accuracy (%)", xlab=""
    #        #, angle=c(rep(45, 9), rep(135, 9), rep(NA, 3)), density=c(rep(30, 18), rep(NA, 3))
    #        , args.legend=list(x = "topleft", inset = c(0.15, 0))
    #        , main="Accuracy in the RO-RP group by cohorts")
    #abline(h=33.33, col="red", lty=2)
    par(mfrow=c(1,1))
    p <- ggplot(qAggrDF, aes(x=qIdShort, y=correct, fill=cohort)) + 
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      labs(x="", y="Accuracy (%)", title="Accuracy in the RO-RP group by cohorts")+
      theme(text=element_text(size = 15), plot.margin=margin(t=2, r=0, b=-15, l=0, unit="pt"))+
      ylim(0, 80)+
      geom_hline(yintercept=38.33, linetype="dashed", color = "red")+
      geom_hline(yintercept=33.33, linetype="solid", color = "red")+
      geom_hline(yintercept=28.33, linetype="dashed", color = "red")+
      #p + scale_fill_brewer(palette="Paired") + theme_minimal()
      scale_fill_grey()
    print(p)
    
    ## [SC] test the proportion of correct answers against the chance probability
    print("Region only - Exact binomial test of significant difference from the chance probability:")
    for(currCohort in unique(regionDF$cohort)){
      for(currQ in unique(regionDF$qIdShort)){
        tempDF <- subset(regionDF, cohort == currCohort & qIdShort == currQ)
        
        ## [SC] non-parametric exact binomial test suitable for smaller sample sizes
        res <- binom.test(x = sum(tempDF$correct), n = nrow(tempDF), p = 0.33)
        print(paste(currCohort, currQ, "success:", res$statistic, "total:", res$parameter, "p-value:"
                    , round(res$p.value, 3), "estimate:", round(res$estimate, 3)))
        
        # [SC] prop.test uses ChiSquare test and is for large sample sizes
        #res <- prop.test(x = sum(tempDF$correct), n = nrow(tempDF), p = 0.33)
        #print(paste(currCohort, currQ, "success:", sum(tempDF$correct), "total:", nrow(tempDF), "p-value:"
        #            , round(res$p.value, 3), "Chi2:", round(res$statistic, 2), "estimate:", round(res$estimate, 3)))
      }
      
      ## [SC] non-parametric exact binomial test suitable for smaller sample sizes
      tempDF <- subset(regionDF, cohort == currCohort)
      res <- binom.test(x = sum(tempDF$correct), n = nrow(tempDF), p = 0.33)
      print(paste(currCohort, "success:", res$statistic, "total:", res$parameter, "p-value:"
                  , round(res$p.value, 3), "estimate:", round(res$estimate, 3)))
    }
    
    # [SC] calculate proportions of choices among three triplets
    for(currCohort in unique(regionDF$cohort)){
      cohortDF <- subset(regionDF, cohort == currCohort)
      
      print(paste0("############# ", currCohort))
      for(currId in unique(regionDF$qIdShort)){
        qDF <- subset(cohortDF, qIdShort == currId)
        print(
          paste(currId, round(mean(qDF$opOne), 2)*100, round(mean(qDF$opTwo), 2)*100, round(mean(qDF$opThree), 2)*100)
        )
      }
    }
  }
  
  
  #################################################################################################
  ## [SC] analyze 6 contrast groups in the student and skilled cohorts
  
  if(TRUE){
    fullDF <- subset(responsesDF, cohort == "student" | cohort == "skilled")
    
    print("6 contrast groups - Number of participants in each cohort:")
    respCountDF <- aggregate(freq ~ cohort, fullDF, sum)
    respCountDF$freq <- respCountDF$freq/18
    print(respCountDF)
    print("")
    
    print("6 contrast groups - Overall accuracy % for each cohort:")
    overallDF <- aggregate(correct ~ cohort, fullDF, mean)
    overallDF$correct <- round(overallDF$correct * 100, 0)
    overallDF <- cbind(overallDF, qIdShort="overall")
    overallDF <- cbind(overallDF, cc="both")
    print(overallDF)
    print("")
    
    
    # [SC] analyze expertise distribution within each cohort
    cexVal=1.5
    print("6 contrast groups - Distribition of expertise by cohorts:")
    experiseDF <- aggregate(freq ~ cohort + expertise, fullDF, sum)
    experiseDF$freq <- experiseDF$freq/18
    experiseDF$cohort <- factor(experiseDF$cohort, levels=c("student", "skilled"))
    experiseDF$expertise <- factor(experiseDF$expertise, levels=c("Layman", "Beginner", "Trained", "Expert"))
    par(mfrow=c(1, 3), oma=c(0,0,0,0), mar=c(4,4,2,1)) # b l t r
    #barplot(freq ~ expertise + cohort, data=experiseDF, beside=TRUE
    #        , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
    #        , legend.text=TRUE, ylab="Sample size", xlab=""
    #        , main="Within-cohort expertise")
    print(experiseDF[order(experiseDF$cohort),])
    print("")
    
    # [SC] calculate accuracy per participant
    partAccDF <- aggregate(correct ~ partId + cohort + expertise, fullDF, sum)
    
    # [SC] plot mean and sd of average accuracies by cohort
    partAccMeanDF <- aggregate(correct ~ cohort, partAccDF, mean)
    colnames(partAccMeanDF)[colnames(partAccMeanDF)=="correct"] <- "correctMean"
    partAccMeanDF <- merge(partAccMeanDF, aggregate(correct ~ cohort, partAccDF, se))
    partAccMeanDF <- partAccMeanDF[order(factor(partAccMeanDF$cohort, levels = c("student", "skilled"))),]
    barplot2(partAccMeanDF$correctMean, names.arg=partAccMeanDF$cohort, beside = TRUE
             , plot.ci = TRUE, ci.l = partAccMeanDF$correctMean-partAccMeanDF$correct
             , ci.u = partAccMeanDF$correctMean+partAccMeanDF$correct
             , main="Mean of correct answers", xlab="cohort", ylab="# of correct answers"
             , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
             , ylim=c(0, 16))
    
    print("6 contrast groups - Distribution of correct answers per participant by cohorts")
    histBreaks <- c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5)
    hist(subset(partAccDF, cohort=="student")$correct, breaks=histBreaks
         , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
         , main="Accuracy - student", ylab="Frequency", xlab="# of correct answers")
    hist(subset(partAccDF, cohort=="skilled")$correct, breaks=histBreaks
         , cex.axis=cexVal, cex.names=cexVal, cex.lab=cexVal, cex.main=cexVal
         , main="Accuracy - skilled", ylab="Frequency", xlab="# of correct answers")
    
    ## [SC] summarize per participant accuracies by cohorts
    partAccDF$cohort <- ordered(partAccDF$cohort, levels = c("student", "skilled"))
    sumResult <- group_by(partAccDF, cohort) %>%
      summarise(
        count = n(),
        mean = mean(correct, na.rm = TRUE),
        sd = sd(correct, na.rm = TRUE),
        median = median(correct, na.rm = TRUE),
        IQR = IQR(correct, na.rm = TRUE)
      )
    print(sumResult)
    
    ## [SC] non-parametric ANOVA on per participant accuracies by cohorts
    print(kruskal.test(correct ~ cohort, data = partAccDF))
    ## [SC] follow-up pairwise tests with Holm-Bonferroni P-value adjustment on per participant accuracies between cohorts
    ## [SC the Dunn's test is for independent groups
    print(dunn_test(correct ~ cohort, data = partAccDF, p.adjust.method = "bonferroni")) # [SC] uses rstatix package
    ## [SC] follow-up pairwise tests with Holm-Bonferroni P-value adjustment on per participant accuracies between cohorts
    print(pairwise.wilcox.test(partAccDF$correct, partAccDF$cohort, p.adjust.method = "BH"))
    print("")
    
    
    tempAccDF <- aggregate(correct ~ qIdShort + group + cohort, fullDF, mean)
    tempAccDF$correct <- tempAccDF$correct * 100
    groupAccDF <- aggregate(correct ~ group + cohort, tempAccDF, mean)
    colnames(groupAccDF)[colnames(groupAccDF)=="correct"] <- "correctMean"
    groupAccDF <- merge(groupAccDF, aggregate(correct ~ group + cohort, tempAccDF, se))
    colnames(groupAccDF)[colnames(groupAccDF)=="correct"] <- "correctSE"
    groupAccDF$group <- ordered(groupAccDF$group, levels=c("PMPO", "LMLO", "RPRO", "COLA", "COVLA", "RFSL"))
    groupAccDF$cohort <- factor(groupAccDF$cohort, levels=c("student", "skilled"))
    par(mfrow=c(1,1))
    p <- ggplot(groupAccDF, aes(x=group, y=correctMean, fill=cohort)) + 
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      geom_errorbar(aes(ymin=correctMean-correctSE, ymax=correctMean+correctSE), width=.2,
                    position=position_dodge(.9))+
      labs(x="", y="Accuracy (%)", title="Accuracy by contrast groups and cohort")+
      theme(text = element_text(size = 15), plot.margin=margin(t=2, r=0, b=-15, l=0, unit="pt"))+
      ylim(0, 100)+
      geom_hline(yintercept=38.33, linetype="dashed", color = "red")+
      geom_hline(yintercept=33.33, linetype="solid", color = "red")+
      geom_hline(yintercept=28.33, linetype="dashed", color = "red")+
      scale_fill_grey()
    print(p)
    
    print(groupAccDF)
    
    ## [SC] test the proportion of correct answers against the chance probability
    print("6 contrast groups - Exact binomial test of significant difference from the chance probability:")
    for(currCohort in unique(fullDF$cohort)){
      for(currG in unique(fullDF$group)){
        tempDF <- subset(fullDF, cohort == currCohort & group == currG)
        
        ## [SC] non-parametric exact binomial test suitable for smaller sample sizes
        res <- binom.test(x = sum(tempDF$correct), n = nrow(tempDF), p = 0.33)
        print(paste(currCohort, currG, "success:", res$statistic, "total:", res$parameter, "p-value:"
                    , round(res$p.value, 3), "estimate:", round(res$estimate, 3)))
      }
      
      ## [SC] non-parametric exact binomial test suitable for smaller sample sizes
      tempDF <- subset(fullDF, cohort == currCohort)
      res <- binom.test(x = sum(tempDF$correct), n = nrow(tempDF), p = 0.33)
      print(paste(currCohort, "success:", res$statistic, "total:", res$parameter, "p-value:"
                  , round(res$p.value, 3), "estimate:", round(res$estimate, 3)))
    }
    
    
    qAccDF <- aggregate(correct ~ qIdShort + cohort, fullDF, mean)
    qAccDF$correct <- qAccDF$correct * 100
    qAccDF$qIdShort <- factor(qAccDF$qIdShort)
    qAccDF$cohort <- factor(qAccDF$cohort, levels=c("student", "skilled"))
    #par(mfrow=c(1,1))
    #barplot(correct ~ cohort + qIdShort, data=qAccDF, beside=TRUE
    #        , legend.text=TRUE, ylab="Accuracy", xlab="", ylim=c(0,100)
    #        , main="Accuracy by contrast groups per each question")
    #abline(h=38.33, lty=2, col="red")
    #abline(h=33.33, lty=1, col="red")
    #abline(h=28.33, lty=2, col='red')
    par(mfrow=c(1,1))
    p <- ggplot(qAccDF, aes(x=qIdShort, y=correct, fill=cohort)) + 
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      labs(x="", y="Accuracy (%)", title="Accuracy by contrast groups per each question")+
      ylim(0, 100)+
      geom_hline(yintercept=38.33, linetype="dashed", color = "red")+
      geom_hline(yintercept=33.33, linetype="solid", color = "red")+
      geom_hline(yintercept=28.33, linetype="dashed", color = "red")+
      scale_fill_grey()
    print(p)
  
    
    # [SC] calculate proportions of choices among three triplets
    for(currCohort in unique(fullDF$cohort)){
      cohortDF <- subset(fullDF, cohort == currCohort)
      print(paste0("############# COHORT: ", currCohort))
      
      for(currGroup in unique(cohortDF$group)){
        groupDF <- subset(cohortDF, group == currGroup)
        print(paste0("############# GROUP: ", currGroup))
        
        for(currId in unique(groupDF$qIdShort)){
          qDF <- subset(groupDF, qIdShort == currId)
          print(
            paste(currId, round(mean(qDF$opOne), 2)*100, round(mean(qDF$opTwo), 2)*100, round(mean(qDF$opThree), 2)*100)
          )
        }
      }
    }
    
  }
}

#cleanHeader()
analyzeQOnePooled()


