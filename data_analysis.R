pacman::p_load(rio, dplyr, ggpubr, ggplot2)
library("ggplot2")
library("ggpubr")
install.packages("hrbrthemes")
data <- import("data.csv")

#####descriptive statistics#####

##age
age <- data %>% filter((subproblem == "age"))
age$studentInput <- as.integer(age$studentInput)
mean(age$studentInput)
sd(age$studentInput)
median(age$studentInput)
range(age$studentInput)


##sex
sex <- data %>% filter((subproblem == "ctatdiv18"))
female = 0
male = 0
prefer_not_to_answer = 0
for(i in 1:nrow(sex)) {
  if (sex[i, 8] == "female") {
    female <- female + 1
  }
  
  if (sex[i, 8] == "male") {
    male <- male + 1
  }
  if (sex[i, 8] == "prefer not to answer") {
    prefer_not_to_answer <- prefer_not_to_answer +1
  }
}

female
male
prefer_not_to_answer


##education
ed <- data %>% filter((subproblem == "ctatdiv25"))
hs = 0
bd = 0
md = 0
for(i in 1:nrow(ed)) {
  if (ed[i, 8] == "High School") {
    hs <- hs + 1
  }
  
  if (ed[i, 8] == "Bachelor's Degree") {
    bd <- bd + 1
  }
  if (ed[i, 8] == "Master's Degree") {
    md <- md +1
  }
}
hs
bd
md


View(ed)



##############Data frame computation for Mann-Whitney-U-Tests##########
temp <- data %>% filter((result == "INCORRECT" | result == "CORRECT") & (subproblemResult == "task1-1" | subproblemResult == "task1-2" | subproblemResult == "task1-3" | subproblemResult == "task2-1" | subproblemResult == "task2-2" | subproblemResult == "task2-3" | subproblemResult == "task3-1" | subproblemResult == "task3-2" | subproblemResult == "task3-3" | subproblemResult == "task4-1" | subproblemResult == "task4-2" | subproblemResult == "task5-1" | subproblemResult == "task5-2"))

temp2 <- data.frame(ID = numeric(0), 
                 Condition = numeric(0),
                 Correct = numeric(0),
                 Incorrect = numeric(0))


for(i in 1:nrow(temp)) {
  temp2[i, 1] <- temp[i, 1]
  temp2[i, 2] <- temp[i, 3]
  if (temp[i, 13] == "INCORRECT") {
    temp2[i, 3] <- 0
    temp2[i, 4] <- 1
  }
  if (temp[i, 13] == "CORRECT") {
    temp2[i, 3] <- 1
    temp2[i, 4] <- 0
  }
}


for(i in 1:nrow(temp2)) {
  if(temp2[i, 2] == "controlGroup") {
    temp2[i, 2] <- "Control"
  }
  else {
    temp2[i, 2] <- "Experimental"
  }
}



grp_tbl <- temp2 %>% group_by(ID, Condition)
agg_tbl <- grp_tbl %>% summarise(sum(Correct), sum(Incorrect))
r1 <- agg_tbl %>% as.data.frame()


names(r1)[names(r1) == "sum(Correct)"] <- "Correct"
names(r1)[names(r1) == "sum(Incorrect)"] <- "Incorrect"

for(i in 1:nrow(r1)) {
  r1[i, 5] <- (r1[i, 3]/(r1[i, 4]+r1[i, 3]))
}

names(r1)[names(r1) == "V5"] <- "Ratio"



control <- r1 %>% filter (Condition == "Control")
experimental <- r1 %>% filter (Condition == "Experimental")


##############Mann-Whitney-U-Test for correct responses#######
mean(control$Correct)
sd(control$Correct)
mean(experimental$Correct)
sd(experimental$Correct)
median(control$Correct)
median(experimental$Correct)

ggboxplot(r1, x = "Condition", y = "Correct", 
          color = "Condition", palette = c("cornflowerblue", "indianred"), size=0.8,
          ylab = "Correct responses", xlab = "Condition", add = "jitter")  + theme(axis.text=element_text(size=12),
                                                                                axis.title=element_text(size=14,))

wilcox.test(r1$Correct ~ r1$Condition, exact = FALSE, correct = FALSE, conf.int = FALSE, alternative="less") 



##############Mann-Whitney-U-Test for incorrect responses#####
mean(control$Incorrect)
sd(control$Incorrect)
mean(experimental$Incorrect)
sd(experimental$Incorrect)
median(control$Incorrect)
median(experimental$Incorrect)


ggboxplot(r1, x = "Condition", y = "Incorrect", 
          color = "Condition", palette = c("cornflowerblue", "indianred"), size = 0.8,
          ylab = "Incorrect responses", xlab = "Condition", add = "jitter") + theme(axis.text=element_text(size=12),
                                                                                 axis.title=element_text(size=14,))

wilcox.test(r1$Incorrect ~ r1$Condition, exact = FALSE, correct = FALSE, conf.int = FALSE, alternative="greater") 




##############Mann-Whitney-U-Test for ratio of correct responses to total of responses####
mean(control$Ratio)
sd(control$Ratio)
mean(experimental$Ratio)
sd(experimental$Ratio)

median(control$Ratio)
median(experimental$Ratio)

ggboxplot(r1, x = "Condition", y = "Ratio", 
          color = "Condition", palette = c("cornflowerblue", "indianred"), size=0.8,
          ylab = "Ratio of correct responses to total of responses given", xlab = "Condition", add = "jitter") + theme(axis.text=element_text(size=12),
                                                                                                                    axis.title=element_text(size=14,))

wilcox.test(r1$Ratio ~ r1$Condition, exact = FALSE, correct = FALSE, conf.int = FALSE, alternative="less") 













##############
##############
##############Data frame computation for User Experience calculations###############
temp <- data %>% filter(action == "UpdateRadioButton")

for(i in 1:nrow(temp)) {
  value <- temp[i, 8]
  if (value == "rb1-1:" | value == "rb1-1:" | value == "rb2-1:" | value == "rb3-1:" | value == "rb4-1:" | value == "rb5-1:" | value == "rb6-1:" | value == "rb7-1:" | value == "rb8-1:") {
    temp[i, 8] <- 1  }
  
  if (value == "rb1-2:" | value == "rb1-2:" | value == "rb2-2:" | value == "rb3-2:" | value == "rb4-2:" | value == "rb5-2:" | value == "rb6-2:" | value == "rb7-2:" | value == "rb8-2:") {
    temp[i, 8] <- 2  }
  
  if (value == "rb1-3:" | value == "rb1-3:" | value == "rb2-3:" | value == "rb3-3:" | value == "rb4-3:" | value == "rb5-3:" | value == "rb6-3:" | value == "rb7-3:" | value == "rb8-3") {
    temp[i, 8] <- 3  }
  
  if (value == "rb1-4:" | value == "rb1-4:" | value == "rb2-4:" | value == "rb3-4:" | value == "rb4-4:" | value == "rb5-4:" | value == "rb6-4:" | value == "rb7-4:" | value == "rb8-4:") {
    temp[i, 8] <- 4  }
  
  if (value == "rb1-5:" | value == "rb1-5:" | value == "rb2-5:" | value == "rb3-5:" | value == "rb4-5:" | value == "rb5-5:" | value == "rb6-5:" | value == "rb7-5:" | value == "rb8-5:") {
    temp[i, 8] <- 5  }
  
  if (value == "rb1-6:" | value == "rb1-6:" | value == "rb2-6:" | value == "rb3-6:" | value == "rb4-6:" | value == "rb5-6:" | value == "rb6-6:" | value == "rb7-6:" | value == "rb8-6:") {
    temp[i, 8] <- 6  }
  
  if (value == "rb1-7:" | value == "rb1-7:" | value == "rb2-7:" | value == "rb3-7:" | value == "rb4-7:" | value == "rb5-7:" | value == "rb6-7:" | value == "rb7-7:" | value == "rb8-7:") {
    temp[i, 8] <- 7  }
  
  if(temp[i, 3] == "controlGroup") {
    temp[i, 3] <- "Control"  }
  else if(temp[i, 3] == "experimentalGroup") {
    temp[i, 3] <- "Experimental" }
  
}


temp$studentInput <- as.numeric(as.character(temp$studentInput))

ux <- data.frame(ID = numeric(0),    
                 Condition = character(0),
                 Dimension = numeric(0),
                 StudentInput = numeric(0))


for(i in 1:nrow(temp)) {
  ux[i, 1] <- temp[i, 1]
  ux[i, 2] <- temp[i, 3]
  ux[i, 3] <- temp[i, 6]
  ux[i, 4] <- temp[i, 8]
}

# removes double entries, leaves only the last given answer
ux <- ux[-c(15, 28, 29, 31, 58, 74, 75, 109, 113, 121, 129, 163, 164, 166, 168, 188, 191, 192, 208, 212, 216, 225, 226, 227), ]


for(i in 1:nrow(ux)) {
  if(ux[i, 3] == "item1") {
    ux[i, 3] <- "(1) obstructive - (7) supportive"
  }
  if(ux[i, 3] == "item2") {
    ux[i, 3] <- "(1) complicated - (7) easy"
  }
  if(ux[i, 3] == "item3") {
    ux[i, 3] <- "(1) inefficient - (7) efficient"
  }
  if(ux[i, 3] == "item4") {
    ux[i, 3] <- "(1) confusing - (7) clear"
  }
  if(ux[i, 3] == "item5") {
    ux[i, 3] <- "(1) boring - (7) exciting"
  }
  if(ux[i, 3] == "item6") {
    ux[i, 3] <- "(1) not interesting - (7) interesting"
  }
  if(ux[i, 3] == "item7") {
    ux[i, 3] <- "(1) conventional - (7) inventing"
  }
  if(ux[i, 3] == "item8") {
    ux[i, 3] <- "(1) usual - (7) leading edge"
  }
}

grp_tbl <- ux %>% group_by(Condition, Dimension)
agg_tbl <- grp_tbl %>% summarise(sum(StudentInput))
ux2 <- agg_tbl %>% as.data.frame()


names(ux2)[names(ux2) == "sum(StudentInput)"] <- "Result"


for(i in 1:nrow(ux2)) {
  if(ux2[i, 2] == "Control") {
    ux2[i, 3] <- ux2[i, 3]/14
  }
  else {
    ux2[i, 3] <- ux2[i, 3]/13
  }
}

#sd

ux2$sd<- NA
ux2$sd <- as.numeric(as.character(ux2$sd))

for(i in 1:nrow(ux2)) {
  tempux <- ux %>% filter(ux$Dimension == ux2[i, 2] & ux$Condition == ux2[i, 1]) 
  ux2[i, 4] <- sd(tempux$StudentInput)
}




##############Bar plot with different user experience dimensions######



ggplot(ux2, color = "Condition",                                    
       aes(x = Dimension,
           y = Result,
           fill = Condition)) +
  geom_bar(stat = "identity",
           position = "dodge",
           ) + theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=14,)) +
  scale_fill_manual(values=c("cornflowerblue",
                             "firebrick"))  +
  coord_flip() + 
  
  geom_errorbar( aes(x=Dimension, ymin=Result-sd, ymax=Result+sd), width=0.4, colour="black", alpha=0.9, size=0.7, position = position_dodge(0.9))




##############Mann-Whitney-U-Test with the ux dimension "inefficient - efficient"#####
uxcontrol <- ux %>% filter (Condition == "Control" & Dimension == "(1) inefficient - (7) efficient")
uxexperimental <- ux %>% filter (Condition == "Experimental" & Dimension == "(1) inefficient - (7) efficient")
mean(uxcontrol$StudentInput)
sd(uxcontrol$StudentInput)
mean(uxexperimental$StudentInput)
sd(uxexperimental$StudentInput)
median(uxexperimental$StudentInput)
median(uxcontrol$StudentInput)

dimension1 <- ux %>% filter(Dimension == "(1) inefficient - (7) efficient")
ggboxplot(dimension1, x = "Condition", y = "StudentInput", 
          color = "Condition", palette = c("cornflowerblue", "indianred"),
          ylab = "Score in efficiency", xlab = "Groups", add = "jitter")


uxd1 <- ux %>% filter (Dimension == "(1) inefficient - (6) efficient")
wilcox.test(uxd1$StudentInput ~ uxd1$Condition, exact = FALSE, correct = FALSE, conf.int = FALSE, alternative="less") 



##############Mann-Whitney-U-Test with the ux dimension "not interesting - interesting"#####

uxcontrol <- ux %>% filter (Condition == "Control" & Dimension == "(1) not interesting - (7) interesting")
uxexperimental <- ux %>% filter (Condition == "Experimental" & Dimension == "(1) not interesting - (7) interesting")
mean(uxcontrol$StudentInput)
sd(uxcontrol$StudentInput)
mean(uxexperimental$StudentInput)
sd(uxexperimental$StudentInput)
median(uxexperimental$StudentInput)
median(uxcontrol$StudentInput)

dimension2 <- ux %>% filter(Dimension == "(1) not interesting - (7) interesting")
ggboxplot(dimension2, x = "Condition", y = "StudentInput", 
          color = "Condition", palette = c("cornflowerblue", "indianred"),
          ylab = "Score in efficiency", xlab = "Groups", add = "jitter")

uxd2 <- ux %>% filter (Dimension == "(1) not interesting - (7) interesting")
wilcox.test(uxd2$StudentInput ~ uxd2$Condition, exact = FALSE, correct = FALSE, conf.int = FALSE, alternative="less") 



##############Correlation between dimension "efficient - inefficient" and correct responses ######
testcor <-merge(r1, dimension1)

cor.test(testcor$StudentInput, testcor$Correct,  method = "spearman", use = "complete.obs")

library("ggpubr")
ggscatter(testcor, x = "StudentInput", y = "Correct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Score in the dimension 'efficient - inefficient'", ylab = "Correct answers")

##############
##############I like/I wish/I wonder entries#######
ilike <- data %>% filter(subproblem =="question5-1")
View(ilike)

iwish <- data %>% filter(subproblem =="question5-2")
View(iwish)

iwonder <- data %>% filter(subproblem =="question5-3")
View(iwonder)



##############
##############
##############Data frame computation for hints usage##########
hints <- data %>% filter(condition == "experimentalGroup" & ((result == "CORRECT" | result == "INCORRECT") & (subproblemResult == "task1-1" | subproblemResult == "task1-2" | subproblemResult == "task1-3" | subproblemResult == "task2-1" | subproblemResult == "task2-2" | subproblemResult == "task2-3" | subproblemResult == "task3-1" | subproblemResult == "task3-2" | subproblemResult == "task3-3" | subproblemResult == "task4-1" | subproblemResult == "task4-2" | subproblemResult == "task5-1" | subproblemResult == "task5-2")) | attempt == "HINT_REQUEST")

for(i in 1:nrow(hints)) {
  if(is.na(hints[i, 13])) {
    hints[i, 13] <- "HINT"
  }
}

hints2 <- data.frame(id = numeric(0),  
                     correct = numeric(0),
                     hints = numeric(0))


for(i in 1:nrow(hints)) {
  hints2[i, 1] <- hints[i, 1]
  if (hints[i, 13] == "INCORRECT") {
    hints2[i, 2] <- 0
    hints2[i, 3] <- 0
  }
  if (hints[i, 13] == "CORRECT") {
    hints2[i, 2] <- 1
    hints2[i, 3] <- 0
  }
  if (hints[i, 13] == "HINT") {
    hints2[i, 2] <- 0
    hints2[i, 3] <- 1
  }
}


grp_tbl <- hints2 %>% group_by(id)
agg_tbl <- grp_tbl %>% summarise(sum(correct), sum(hints))
hints3 <- agg_tbl %>% as.data.frame()

names(hints3)[names(hints3) == "sum(correct)"] <- "correct"
names(hints3)[names(hints3) == "sum(hints)"] <- "hints"






##############Hints used per participant & perception of interventions#####
View(hints3)

intervention1<- data %>% filter(subproblem == "question1")
View(intervention1)

intervention1perception <- data %>% filter(subproblem == "question2")
View(intervention1perception)


intervention2 <- data %>% filter(subproblem == "question3")
View(intervention2)

intervention2perception <- data %>% filter(subproblem == "question4")
View(intervention2perception)




##############Correlation between hints and correct responses#####
mean(hints3$hints)
sd(hints3$hints)

cor(hints3$correct, hints3$hints,  method = "pearson", use = "complete.obs")

library("ggpubr")
ggscatter(hints3, x = "correct", y = "hints", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "correct", ylab = "hints used")



##############Responses after hint usage#####
hints4 <- hints2

for(i in 1:nrow(hints4)) {
  if (hints4[i, 3] == 1) {
    if (hints4[i, 1] == hints4[i+1, 1]) {
      if (hints4[i+1, 2]== 1) {
        hints4[i, 4] <- "hc"}
      if (hints4[i+1, 2]== 0) {
        hints4[i, 4] <- "hi"}
    }
  }
  else hints4[i, 4] <- 0
}


hintcorrect <- 0
hintincorrect <- 0

for(i in 1:nrow(hints4)) {
  if(hints4[i, 4] == "hc") {
    hintcorrect <- hintcorrect + 1
  }
  if(hints4[i, 4] == "hi") {
    hintincorrect <- hintincorrect + 1
  }
}

hintcorrect
hintincorrect












##############Time frames: before/after hint appearance sequences#####
##############General pre computation: date stamp is computed to time, where the beginning of a pre-task is 0 seconds#########
data1 <- data
data1[2507, 10] <- "preTask5-1"
data1[2507, 13] <- "INCORRECT"
data1 <- data1[-c(1584:1589, 2454:2478, 2740), ]
tf <- data1 %>% filter(condition == "experimentalGroup" 
                      & (((result == "CORRECT" | result == "INCORRECT") 
                           & (subproblemResult == "task1-1" | subproblemResult == "task1-2" | subproblemResult == "task1-3" | subproblemResult == "task2-1" | subproblemResult == "task2-2" | subproblemResult == "task2-3" | subproblemResult == "task3-1" | subproblemResult == "task3-2" | subproblemResult == "task3-3" | subproblemResult == "task4-1" | subproblemResult == "task4-2" | subproblemResult == "task5-1" | subproblemResult == "task5-2" | subproblemResult == "preTask5-1")) | (problem == "LearningTask1" | problem == "LearningTask2" | problem == "LearningTask3" | problem == "LearningTask4" | problem == "LearningTask5" | problem == "PreTask1"))      
                           | (attempt == "HINT_REQUEST"))

for( i in 1:nrow(tf)) {
  tf[i, 2] <- as.POSIXct(tf[i, 2])
  }

tf$date = as.numeric(as.character(tf$date))
currentID = 0
startTimestamp = 0

for (i in 1:nrow(tf)){
  if (tf[i, 1] == currentID) {
    tf[i, 2] <- tf[i, 2] - startTimestamp
}

else if (tf[i, 1] != currentID){
  currentID <- tf[i, 1]
  startTimestamp <-tf[i, 2]
  tf[i, 2] <- tf[i, 2] - startTimestamp
}
}



##############Learning task 1: Diagram with event sequences per participant, where all sequences are aligned by the time of hint button appearance##########
tf1 <- tf
tf1 <- tf1[-c(46, 63, 79, 85, 91, 109, 142, 150, 183, 256, 286, 291, 297, 375, 383, 426, 357, 358, 272, 273, 274), ] #manual removal of hint requests for other task, since these entries are all the same, and can not be removed other way & 357, 358, 272, 273, 274 <- participants refreshed the page or returned back to the page
task1 <- tf1 %>% filter(subproblemResult =="preTask5-1" | subproblemResult == "task1-1" | subproblemResult == "task1-2" | subproblemResult == "task1-3" | attempt == "HINT_REQUEST" | problem == "LearningTask1")


for (i in 1:nrow(task1)) {
  if (is.na(task1[i, 10])) {
    if (!is.na(task1[i, 5])) {
    task1[i, 10] <- "HINT"
    task1[i, 13] <- "HINT"}
  
  
    if (!is.na(task1[i, 4])) {
    task1[i, 10] <- "task"
    task1[i, 13] <- "START"}
}
}


id = 0
starttime = 0
for (i in 1:nrow(task1)) {
  if (task1[i, 10] != "preTask5-1") {
  if (task1[i, 1] == id) {
    task1[i, 2] <- task1[i, 2] - starttime
  }
  else if (task1[i, 1] != id) {
    id <- task1[i, 1]
    starttime <- task1[i, 2]
    task1[i, 2] <- task1[i, 2] - starttime 
    }
  }
}


currentID = 0
hintbutton = 0
for (i in 1:nrow(task1)) {
  if (task1[i, 1]) {
       if (task1[i, 1] == currentID) {
           task1[i, 2] <- task1[i, 2] - hintbutton
        }
          else if (task1[i, 1] != currentID){
              currentID <- task1[i, 1]
              hintbutton <-task1[i, 2]/5 * 0.75
              task1[i, 2] <- hintbutton
         }
     }
}



learningTask1 <- data.frame( ID = numeric(0), 
                             Time  = numeric(0),
                             Event = character(0))

for(i in 1:nrow(task1)) {  
  if (task1[i, 10] != "preTask5-1") {
  learningTask1[i, 1] <- task1[i, 1]
  learningTask1[i, 2] <- task1[i, 2]
  if (task1[i, 13] == "CORRECT") {
    learningTask1[i, 3] <- "Correct response"
  }
  if (task1[i, 13] == "INCORRECT") {
    learningTask1[i, 3] <- "Incorrect response"
  }
  if (task1[i, 13] == "HINT") {
    learningTask1[i, 3] <- "Hint request"
  }
  if (task1[i, 13] == "START") {
    learningTask1[i, 3] <- "Starting point"
  }
}
}

learningTask1 <- na.omit(learningTask1)

library(ggplot2)
library(hrbrthemes)
p <- ggplot(learningTask1, aes(x=Time, y=ID, color=Event, shape=Event)) + 
  xlab ("Time span in seconds interval") +
  
  geom_point() +
  geom_point(data=learningTask1 %>% filter(ID == 26), color="grey", pch = 15,
             size=8,) + 
  geom_point(aes(color=Event), size=6) +
  scale_color_manual(values=c('forestgreen', 'steelblue', 'indianred', 'gold' )) + geom_vline(xintercept = 0, linetype="dotted", 
                                                                                              color = "black", size=1,) + theme(axis.text=element_text(size=12),
                                                                                                                                axis.title=element_text(size=14,))+
  scale_shape_manual(values = c(16, 17, 16, 18)) +
  geom_text(aes(3, 14, label = "← Approximated time of hint button appearance", hjust = -0.01)) 

p + scale_y_continuous(breaks=c(14:27))

 

##############Learning task 2: Diagram with event sequences per participant, where all sequences are aligned by the time of hint button appearance#########
tf2 <- tf
tf2 <- tf2[-c(37, 63, 72, 85, 91, 102, 150, 178, 256, 274, 291, 297, 310, 367, 383, 426), ] #manual removal of hint requests for other task, since these entries are all the same, and can not be removed other way 
task2 <- tf2 %>% filter(subproblemResult =="preTask5-1" | subproblemResult == "task2-1" | subproblemResult == "task2-2" | subproblemResult == "task2-3" | attempt == "HINT_REQUEST" | problem == "LearningTask2")


for (i in 1:nrow(task2)) {
  if (is.na(task2[i, 10])) {
    if (!is.na(task2[i, 5])) {
      task2[i, 10] <- "HINT"
      task2[i, 13] <- "HINT"}
    
    
    if (!is.na(task2[i, 4])) {
      task2[i, 10] <- "task"
      task2[i, 13] <- "START"}
  }
}


id = 0
starttime = 0
for (i in 1:nrow(task2)) {
  if (task2[i, 10] != "preTask5-1") {
    
    if (task2[i, 1] == id) {
      task2[i, 2] <- task2[i, 2] - starttime
    }
    else if (task2[i, 1] != id) {
      id <- task2[i, 1]
      starttime <- task2[i, 2]
      task2[i, 2] <- task2[i, 2] - starttime 
    }
  }
}


currentID = 0
hintbutton = 0
for (i in 1:nrow(task2)) {
  if (task2[i, 1]) {
    if (task2[i, 1] == currentID) {
      task2[i, 2] <- task2[i, 2] - hintbutton
    }
    else if (task2[i, 1] != currentID){
      currentID <- task2[i, 1]
      hintbutton <-task2[i, 2]/5 * 0.75
      task2[i, 2] <- hintbutton
    }
  }
}


learningTask2 <- data.frame( ID = numeric(0), 
                             Time  = numeric(0),
                             Event = character(0))

for(i in 1:nrow(task2)) {
  if (task2[i, 10] != "preTask5-1") {
  learningTask2[i, 1] <- task2[i, 1]
  learningTask2[i, 2] <- task2[i, 2]
  if (task2[i, 13] == "CORRECT") {
    learningTask2[i, 3] <- "Correct response"
  }
  if (task2[i, 13] == "INCORRECT") {
    learningTask2[i, 3] <- "Incorrect response"
  }
  if (task2[i, 13] == "HINT") {
    learningTask2[i, 3] <- "Hint request"
  }
  if (task2[i, 13] == "START") {
    learningTask2[i, 3] <- "Starting point"
  }
}
}

learningTask2 <- na.omit(learningTask2)

library(ggplot2)
library(hrbrthemes)
p <- ggplot(learningTask2, aes(x=Time, y=ID, color=Event, shape=Event)) + 
  geom_point() +
  geom_point(data=learningTask2 %>% filter(ID == 19 | ID == 24), color="grey", pch = 15,
             size=8,) + 
  geom_point(aes(color=Event), size=6) +
  scale_color_manual(values=c('forestgreen', 'steelblue', 'indianred', 'gold' )) + geom_vline(xintercept = 0, linetype="dotted", 
                                                                                      color = "black", size=1,) + theme(axis.text=element_text(size=12),
                                                                                                                        axis.title=element_text(size=14,))+ 
  scale_shape_manual(values = c(16, 17, 16, 18)) +
  geom_text(aes(3, 14, label = "← Approximated time of hint button appearance", hjust = -0.01)) + 
  xlab ("Time span in seconds interval")

p + scale_y_continuous(breaks=c(14:27))
 

##############Learning task 3: Diagram with event sequences per participant, where all sequences are aligned by the time of hint button appearance##########
tf3 <- tf
tf3 <- tf3[-c(37, 46, 63, 72, 79, 91, 102, 109, 142, 178, 183, 274, 286, 297, 310, 367, 375), ] #manual removal of hint requests for other task, since these entries are all the same, and can not be removed other way 
task3 <- tf3 %>% filter(subproblemResult =="preTask5-1" | subproblemResult == "task3-1" | subproblemResult == "task3-2" | subproblemResult == "task3-3" | attempt == "HINT_REQUEST" | problem == "LearningTask3")


for (i in 1:nrow(task3)) {
  if (is.na(task3[i, 10])) {
    if (!is.na(task3[i, 5])) {
      task3[i, 10] <- "HINT"
      task3[i, 13] <- "HINT"}
    
    
    if (!is.na(task3[i, 4])) {
      task3[i, 10] <- "task"
      task3[i, 13] <- "START"}
  }
}


id = 0
starttime = 0
for (i in 1:nrow(task3)) {
  if (task3[i, 1] == id & task3[i, 10] != "preTask5-1") {
    task3[i, 2] <- task3[i, 2] - starttime
  }
  else if (task3[i, 1] != id & task3[i, 10] != "preTask5-1") {
    id <- task3[i, 1]
    starttime <- task3[i, 2]
    task3[i, 2] <- task3[i, 2] - starttime
  }
}


currentID = 0
hintbutton = 0
for (i in 1:nrow(task3)) {
  if (task3[i, 1]) {
    if (task3[i, 1] == currentID) {
      task3[i, 2] <- task3[i, 2] - hintbutton
    }
    else if (task3[i, 1] != currentID){
      currentID <- task3[i, 1]
      hintbutton <-task3[i, 2]/5 * 0.75
      task3[i, 2] <- hintbutton
    }
  }
}


learningTask3 <- data.frame( ID = numeric(0), 
                             Time  = numeric(0),
                             Event = character(0))

for(i in 1:nrow(task3)) {
  learningTask3[i, 1] <- task3[i, 1]
  learningTask3[i, 2] <- task3[i, 2]
  if (task3[i, 10] != "preTask5-1") {
  if (task3[i, 13] == "CORRECT") {
    learningTask3[i, 3] <- "Correct response"
  }
  if (task3[i, 13] == "INCORRECT") {
    learningTask3[i, 3] <- "Incorrect response"
  }
  if (task3[i, 13] == "HINT") {
    learningTask3[i, 3] <- "Hint request"
  }
  if (task3[i, 13] == "START") {
    learningTask3[i, 3] <- "Starting point"
  }
}
}

learningTask3 <- na.omit(learningTask3)


library(ggplot2)
library(hrbrthemes)
p <- ggplot(learningTask3, aes(x=Time, y=ID, color=Event, shape=Event)) + 
  
  geom_point() +
  geom_point(data=learningTask3 %>% filter(ID == 23), color="grey", pch = 15,
             size=8,) + 
  geom_point(aes(color=Event), size=6) +
  scale_color_manual(values=c('forestgreen', 'steelblue', 'indianred', 'gold' )) + geom_vline(xintercept = 0, linetype="dotted", 
                                                                                              color = "black", size=1,) + theme(axis.text=element_text(size=12),
                                                                                                                                axis.title=element_text(size=14,))+ 
  scale_shape_manual(values = c(16, 17, 16, 18)) +
  geom_text(aes(3, 14, label = "← Approximated time of hint button appearance", hjust = -0.01)) + 
  xlab ("Time span in seconds interval")

p + scale_y_continuous(breaks=c(14:27))



##############Learning task 4: Diagram with event sequences per participant, where all sequences are aligned by the time of hint button appearance###########
tf4 <- tf
tf4 <- tf4[-c(37, 46, 63, 72, 79, 85, 102, 109, 142, 150, 178, 183, 256, 274, 286, 291, 310, 367, 375, 383, 426), ] #manual removal of hint requests for other task, since these entries are all the same, and can not be removed other way 
task4 <- tf4 %>% filter(subproblemResult =="preTask5-1" | subproblemResult == "task4-1" | subproblemResult == "task4-2" | subproblemResult == "task4-3" | attempt == "HINT_REQUEST" | problem == "LearningTask4")


for (i in 1:nrow(task4)) {
  if (is.na(task4[i, 10])) {
    if (!is.na(task4[i, 5])) {
      task4[i, 10] <- "HINT"
      task4[i, 13] <- "HINT"}
    
    
    if (!is.na(task4[i, 4])) {
      task4[i, 10] <- "task"
      task4[i, 13] <- "START"}
  }
}


id = 0
starttime = 0
for (i in 1:nrow(task4)) {
  if (task4[i, 1] == id & task4[i, 10] != "preTask5-1") {
    task4[i, 2] <- task4[i, 2] - starttime
  }
  else if (task4[i, 1] != id & task4[i, 10] != "preTask5-1") {
    id <- task4[i, 1]
    starttime <- task4[i, 2]
    task4[i, 2] <- task4[i, 2] - starttime
  }
}


currentID = 0
hintbutton = 0
for (i in 1:nrow(task4)) {
  if (task4[i, 1]) {
    if (task4[i, 1] == currentID) {
      task4[i, 2] <- task4[i, 2] - hintbutton
    }
    else if (task4[i, 1] != currentID){
      currentID <- task4[i, 1]
      hintbutton <-task4[i, 2]/5 * 0.75
      task4[i, 2] <- hintbutton
    }
  }
}


learningTask4 <- data.frame( ID = numeric(0), 
                             Time  = numeric(0),
                             Event = character(0))

for(i in 1:nrow(task4)) {  
  if (task4[i, 10] != "preTask5-1") {
  learningTask4[i, 1] <- task4[i, 1]
  learningTask4[i, 2] <- task4[i, 2]
  if (task4[i, 13] == "CORRECT") {
  learningTask4[i, 3] <- "Correct response"
  }
  if (task4[i, 13] == "INCORRECT") {
    learningTask4[i, 3] <- "Incorrect response"
  }
  if (task4[i, 13] == "HINT") {
    learningTask4[i, 3] <- "Hint request"
  }
  if (task4[i, 13] == "START") {
    learningTask4[i, 3] <- "Starting point"
  }
}
}

learningTask4 <- na.omit(learningTask4)

library(ggplot2)
library(hrbrthemes)
p <- ggplot(learningTask4, aes(x=Time, y=ID, color=Event, shape=Event)) + 
  
  geom_point() +
  geom_point(data=learningTask4 %>% filter(ID == 18 | ID == 21), color="grey", pch = 15,
             size=8,) + 
  geom_point(aes(color=Event), size=6) +
  scale_color_manual(values=c('forestgreen', 'steelblue', 'indianred', 'gold' )) + geom_vline(xintercept = 0, linetype="dotted", 
                                                                                              color = "black", size=1,) + theme(axis.text=element_text(size=12),
                                                                                                                                axis.title=element_text(size=14,)) + 
  scale_shape_manual(values = c(16, 17, 16, 18)) +
  geom_text(aes(3, 14, label = "Approximated time of hint button appearance ➝", hjust = 1.05)) + 
  xlab ("Time span in seconds interval")

p + scale_y_continuous(breaks=c(14:27))





##############Learning task 5: Diagram with event sequences per participant, where all sequences are aligned by the time of hint button appearance##########
tf5 <- tf
tf5 <- tf5[-c(37, 46, 91, 72, 79, 85, 102, 109, 142, 150, 178, 183, 256, 274, 286, 291, 297, 310, 367, 375, 383, 426), ] #manual removal of hint requests for other task, since these entries are all the same, and can not be removed other way 
task5 <- tf5 %>% filter(subproblemResult =="preTask5-1" | subproblemResult == "task5-1" | subproblemResult == "task5-2" | subproblemResult == "task5-3" | attempt == "HINT_REQUEST" | problem == "LearningTask5")


for (i in 1:nrow(task5)) {
  if (is.na(task5[i, 10])) {
    if (!is.na(task5[i, 5])) {
      task5[i, 10] <- "HINT"
      task5[i, 13] <- "HINT"}
    
    
    if (!is.na(task5[i, 4])) {
      task5[i, 10] <- "task"
      task5[i, 13] <- "START"}
  }
}


id = 0
starttime = 0
for (i in 1:nrow(task5)) {
  if (task5[i, 1] == id & task5[i, 10] != "preTask5-1") {
    task5[i, 2] <- task5[i, 2] - starttime
  }
  else if (task5[i, 1] != id & task5[i, 10] != "preTask5-1") {
    id <- task5[i, 1]
    starttime <- task5[i, 2]
    task5[i, 2] <- task5[i, 2] - starttime
  }
}


currentID = 0
hintbutton = 0
for (i in 1:nrow(task5)) {
  if (task5[i, 1]) {
    if (task5[i, 1] == currentID) {
      task5[i, 2] <- task5[i, 2] - hintbutton
    }
    else if (task5[i, 1] != currentID){
      currentID <- task5[i, 1]
      hintbutton <-task5[i, 2]/5 * 0.75
      task5[i, 2] <- hintbutton
    }
  }
}


learningTask5 <- data.frame( ID = numeric(0), 
                             Time  = numeric(0),
                             Event = character(0))

for(i in 1:nrow(task5)) {
  learningTask5[i, 1] <- task5[i, 1]
  learningTask5[i, 2] <- task5[i, 2]
  if (task5[i, 10] != "preTask5-1") {
  if (task5[i, 13] == "CORRECT") {
    learningTask5[i, 3] <- "Correct response"
  }
  if (task5[i, 13] == "INCORRECT") {
    learningTask5[i, 3] <- "Incorrect response"
  }
  if (task5[i, 13] == "HINT") {
    learningTask5[i, 3] <- "Hint request"
  }
  if (task5[i, 13] == "START") {
    learningTask5[i, 3] <- "Starting point"
  }
}
}
learningTask5 <- na.omit(learningTask5)

library(ggplot2)
library(hrbrthemes)
p <- ggplot(learningTask5, aes(x=Time, y=ID, color=Event, shape=Event)) + 
  
  geom_point() +
  geom_point(data=learningTask5 %>% filter(ID == 25 | ID == 24 | ID == 18), color="grey", pch = 15,
             size=8,) + 
  geom_point(aes(color=Event), size=6) +
  scale_color_manual(values=c('forestgreen', 'steelblue', 'indianred', 'gold' )) + geom_vline(xintercept = 0, linetype="dotted", 
                                                                                              color = "black", size=1,) + theme(axis.text=element_text(size=12),
                                                                                                                                axis.title=element_text(size=14,))+ 
  scale_shape_manual(values = c(16, 17, 16, 18)) +
  geom_text(aes(3, 14, label = "Approximated time of hint button appearance ➝", hjust = 1.05)) + 
  xlab ("Time span in seconds interval")

p + scale_y_continuous(breaks=c(14:27))










######################################SUMME ALLER ANTWORTEN


temp <- data %>% filter((result == "INCORRECT" | result == "CORRECT") & (subproblemResult == "task1-1" | subproblemResult == "task1-2" | subproblemResult == "task1-3" | subproblemResult == "task2-1" | subproblemResult == "task2-2" | subproblemResult == "task2-3" | subproblemResult == "task3-1" | subproblemResult == "task3-2" | subproblemResult == "task3-3" | subproblemResult == "task4-1" | subproblemResult == "task4-2" | subproblemResult == "task5-1" | subproblemResult == "task5-2"))

temp2 <- data.frame(ID = numeric(0), 
                    Condition = numeric(0),
                    Summe = numeric(0))


for(i in 1:nrow(temp)) {
  temp2[i, 1] <- temp[i, 1]
  temp2[i, 2] <- temp[i, 3]
  if (temp[i, 13] == "INCORRECT" | temp[i, 13] == "CORRECT" ) {
    temp2[i, 3] <- 1
  }
}


for(i in 1:nrow(temp2)) {
  if(temp2[i, 2] == "controlGroup") {
    temp2[i, 2] <- "Control"
  }
  else {
    temp2[i, 2] <- "Experimental"
  }
}



grp_tbl <- temp2 %>% group_by(ID, Condition)
agg_tbl <- grp_tbl %>% summarise(sum(Summe))
r1 <- agg_tbl %>% as.data.frame()


names(r1)[names(r1) == "sum(Summe)"] <- "Summe"


control <- r1 %>% filter (Condition == "Control")
experimental <- r1 %>% filter (Condition == "Experimental")





mean(control$Summe)
sd(control$Summe)
mean(experimental$Summe)
sd(experimental$Summe)
median(control$Summe)
median(experimental$Summe)

ggboxplot(r1, x = "Condition", y = "Summe", 
          color = "Condition", palette = c("cornflowerblue", "indianred"), size=0.8,
          ylab = "Sum of all responses", xlab = "Condition", add = "jitter")  + theme(axis.text=element_text(size=12),
                                                                                axis.title=element_text(size=14,))

wilcox.test(r1$Summe ~ r1$Condition, exact = FALSE, correct = FALSE, conf.int = FALSE, alternative="less") 

