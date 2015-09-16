fname=file.choose()
data=read.csv(fname,header=T)

## I. Data Cleaning ####
## 1. Sex
#Define a categorical variable as a factor 
data$sex <- factor(data$x2sex,levels = c(1, 2),labels = c("Male", "Female"))
## 2. Race
data$race <- data$x2race
## recode Hispanic 
#Define a categorical variable as a factor
data$race[data$x2race == 5] <- 4
data$race <- factor(data$race, levels = c(8, 1, 2, 3, 4, 6, 7),labels = c("White", "Native", "Asian", "Black", "Hispanic", "Multiracial", "Pacific"))

## 3. Language at Home from Birth
data$homeLang <- data$x2duallang
## recode "First Language is English and non-English equally" as 0
data$homeLang[data$homeLang == 1] <- 0
## recode "First language is English only" as 0
data$homeLang[data$homeLang == 3] <- 0
## recode "First language is a non-English language only" to 1
data$homeLang[data$homeLang == 2] <- 1
## recode missing as NA
data$homeLang[data$homeLang == -9] <- NA
#Define a categorical variable as a factor
data$homeLang <- factor(data$homeLang,levels = c(0, 1),labels = c("English", "NotEnglish"))

## 4. Parents' Highest Level of Education
data$parEdu <- data$x2paredu
data$parEdu[data$parEdu == -8] <- NA

## HighSchool = High School diploma or GED, LessHS = less than HS
## Vocat = Certificate/diploma from occupational training school
## AA = Associates, BA = Bachelors, MA = Masters, DR = Ph.D, MD, Law, etc
data$parEdu <- factor(data$parEdu,levels = c(2, 1, 3, 4, 5, 6, 7),labels = c("HighSchool", "LessHS", "Vocat", "AA", "BA", "MA", "DR"))
## 5. Family Income
## continuous. 1 = less than or equal to 15k, each additional number = 20k more income
data$famIncome <- data$x2famincome
data$famIncome[data$famIncome == -8] <- NA
data$logFamIncome <- log(data$famIncome)
hist(data$famIncome,main="Family Imcome")
hist(log(data$famIncome),main="Log of Family Income")
## 6. Math Interest
## a continuous composite variable centered at mean 0 with SD 1 
data$mathInt <- data$x2mthint
data$mathInt[data$mathInt == -8] <- NA
data$mathInt[data$mathInt == -9] <- NA
data$mathInt[data$mathInt == -7] <- NA


## 7. Math Self-efficacy
## a continuous composite variable centered at mean 0 with SD 1 
data$mathEff <- data$x2mtheff
data$mathEff[data$mathEff == -8] <- NA
data$mathEff[data$mathEff == -9] <- NA
data$mathEff[data$mathEff == -7] <- NA

## 8. Friend's grades
data$fGrade<-data$s2frgrades
data$fGrade[data$fGrade == -9] <- NA
data$fGrade[data$fGrade == -8] <- NA
data$fGrade[data$fGrade == 5] <- NA
data$fGrade <- factor(data$fGrade,levels = c(0,1,2,3,4),labels = c("None","Less than half","About half","More than half","All of them"))

## 9. Hours spent on homework
data$hoursHW<-data$s2mhomewrk
data$hoursHW[data$hoursHW == -9] <- NA
data$hoursHW[data$hoursHW == -8] <- NA
data$hoursHW[data$hoursHW == -7] <- NA
data$hoursHW <- factor(data$hoursHW,levels = c(0,1,2,3,4,5,6,7),labels = c("No time", "Less than 1/2 hour", "1/2 to 1 hour", "1 to 2 hours", "2 to 3 hours","4 to 6 hours","7 to 9 hours","More than 9 hours"))

##10. Taken calculus
data$calculus<-data$s2apcalc12
data$calculus[data$calculus == -9] <- NA
data$calculus[data$calculus == -8] <- NA
data$calculus[data$calculus == -7] <- NA
data$calculus <- factor(data$calculus,levels = c(0,1),labels = c("No", "Yes"))


## 11. Number of siblings
data$sibling<-data$p2sibnum
data$sibling[data$sibling < 0] <- NA

## 12. Whether Parents have divorced or not
data$divorce<-data$p2pardivorce
data$divorce[data$divorce == -9] <- NA
data$divorce[data$divorce == -8] <- NA
data$divorce[data$divorce == -6] <- NA
data$divorce[data$divorce == -4] <- NA
#Define a categorical variable as a factor
data$divorce <- factor(data$divorce,levels = c(1,0),labels = c("Yes", "No"))

## 13. Nummber of times students/parents discussed college
data$discuss<-data$p2discclgapp
data$discuss[data$discuss == -9] <- NA
data$discuss[data$discuss == -8] <- NA
data$discuss[data$discuss == -6] <- NA
data$discuss[data$discuss == -4] <- NA
#Define a categorical variable as a factor
data$discuss <- factor(data$discuss,levels = c(1,2,3,4),labels = c("Never", "Once or Twice","Three/four","More"))

## 14. Highest degree obtained by math teacher
data$degreeTE<-data$m1hideg
data$degreeTE[data$degreeTE == -9] <- NA
data$degreeTE[data$degreeTE == -8] <- NA
#Define a categorical variable as a factor
data$degreeTE <- factor(data$degreeTE,levels = c(3,4,5,6),labels = c("Bachelor", "Master","Specialist","PhD,MD"))

## 15. Whether school gives scholarships to students or not
data$scholarship<-data$c2haschshp
data$scholarship[data$scholarship == -9] <- NA
data$scholarship[data$scholarship == -8] <- NA
data$scholarship[data$scholarship == -6] <- NA
#Define a categorical variable as a factor
data$scholarship <- factor(data$scholarship,levels = c(0,1),labels = c("No", "Yes"))

## 16. Number of years of teaching
data$mathYears<-data$m1mthyrs912
data$mathYears[data$mathYears <0] <- NA
data$logmathYears <- log(data$mathYears)
qqnorm(data$mathYears)
qqline(data$mathYears)
hist(data$mathYears,main="Teaching Years")
hist(log(data$mathYears),main="Log of Teaching Years")

##  17. Percentage of students at the school who entered the higher education
data$studentsHE<-data$a2highered
data$studentsHE[data$studentsHE < 0] <- NA
qqnorm(data$studentsHE)
qqline(data$studentsHE)
hist(data$studentsHE)
hist((data$studentsHE)^2)
data$sqrtstudentsHE<-(data$studentsHE)^2
hist((data$studentsHE),main="Percent of students who entered higher education")
hist((data$studentsHE)^2,main="Square of Percent of students who entered higher education")

## 18. Average ACT score at school
data$avgACTMath<-data$c2avgactmath
data$avgACTMath[data$avgACTMath < 0] <- NA
qqnorm(data$avgACTMath)
qqline(data$avgACTMath)
hist(data$avgACTMath)

## 19. Average SAT score at school
data$avgSATMath<-data$c2avgsatmath
data$avgSATMath[data$avgSATMath < 0] <- NA
qqnorm(data$avgSATMath)
qqline(data$avgSATMath)
hist(data$avgSATMath)

## 20. Caseload of each school counselor
data$caseLoad<-data$c2caseload
data$caseLoad[data$caseLoad < 0] <- NA
hist(data$caseLoad)
qqnorm(data$caseLoad)
qqline(data$caseLoad)

## Outcome - math score
data$score<-data$x2txmscr
data$score[data$score < 0] <- NA

## II. Fitting a Model ####

## Full Model
model1 <- lm(score  ~ race + sex+homeLang+logFamIncome+mathInt +
            mathEff+fGrade+hoursHW+calculus+sibling+divorce+discuss+degreeTE+logmathYears+scholarship
          +sqrtstudentsHE+caseLoad+avgSATMath+avgACTMath, data=data)

summary(model1)

## Output
# Call:
 # lm(formula = score ~ race + sex + homeLang + logFamIncome + mathInt + 
  #     mathEff + fGrade + hoursHW + calculus + sibling + divorce + 
   #    discuss + degreeTE + logmathYears + scholarship + sqrtstudentsHE + 
    #   caseLoad + avgSATMath + avgACTMath, data = data)
------------------------------------------------------------------------

# Residual standard error: 14.24 on 1158 degrees of freedom
#(22219 observations deleted due to missingness)
# Multiple R-squared:  0.4319,  Adjusted R-squared:  0.4137 
# F-statistic: 23.79 on 37 and 1158 DF,  p-value: < 2.2e-16

#The degrees of freedom are very low due to the missing variables.
#We need to consider removing variables that have many missing observations.
#See how many missing observations each variable has:
missing <- apply(is.na(data),2,mean)

#  x2txmscr          x2sex         x2race     x2duallang       x2paredu 
#  0.0000000000   0.0000000000   0.0000000000   0.0000000000   0.0000000000 
#  x2famincome       x2mthint       x2mtheff    s2apexamnum     s2frgrades 
#  0.0000000000   0.0000000000   0.0000000000   0.0000000000   0.0000000000 
#  s2mhomewrk     s2apcalc12   s2enrollhs12       p2sibnum   p2pardivorce 
#  0.0000000000   0.0000000000   0.0000000000   0.0000000000   0.0000000000 
#  p2discclgapp        m1hideg    m1mthyrs912     a2highered     c2caseload 
#  0.0000000000   0.0000000000   0.0000000000   0.0000000000   0.0000000000 
#  c2haschshp   c2avgsatmath   c2avgactmath     x2txmscr.1            sex 
#  0.0000000000   0.0000000000   0.0000000000   0.0000000000   0.0000000000 
#  race       homeLang         parEdu      famIncome   logFamIncome 
#  0.0000000000   0.0004697843   0.1065983344   0.1065983344   0.1065983344 
#  mathInt        mathEff         fGrade        hoursHW       calculus 
#  0.2805039505   0.1556267350   0.1554559043   0.2265641683   0.2556053812 
#  enroll        sibling        divorce        discuss       degreeTE 
#  0.1205637412   0.6545803972   0.6556480888   0.6561178732   0.2711082639 
#  scholarship      mathYears   logmathYears     studentsHE sqrtstudentsHE 
#  0.1821481956   0.2731155242   0.2731155242   0.1385436686   0.1385436686 
#  avgACTMath     avgSATMath       caseLoad          score 
#  0.6480034166   0.5803544736   0.1824044416   0.1204783259 

#  Need to remove(Probability of missing data>0.3)
#  remove avgACTMath
#  remove avgSATMath
#  #  remove siblings
#  remove divorce
#  remove discuss

## Removed avgACTMath avgSATMath siblings divorce discuss from model1
model2 <- lm(score  ~ race + sex+homeLang+logFamIncome+mathInt +
            mathEff+fGrade+hoursHW+calculus+degreeTE+logmathYears+scholarship
          +sqrtstudentsHE+caseLoad, data=data)

summary(model2)
#  Call:
#    lm(formula = score ~ race + sex + homeLang + logFamIncome + mathInt + 
#         mathEff + fGrade + hoursHW + calculus + degreeTE + logmathYears + 
#         scholarship + sqrtstudentsHE + caseLoad, data = data)

## Perform the backward procedure
newdata <- na.omit(data)
fullmodel <- lm(score  ~ race + sex+homeLang+logFamIncome+mathInt +
                  mathEff+fGrade+hoursHW+calculus+degreeTE+logmathYears+scholarship
                +sqrtstudentsHE+caseLoad, data=newdata)
model3<-step(fullmodel,direction="backward")
summary(model3)
#Output: 
#  Call:
#    lm(formula = score ~ race + sex + homeLang + logFamIncome + mathInt + 
#         mathEff + fGrade + hoursHW + calculus + logmathYears + scholarship + 
#         sqrtstudentsHE, data = newdata)

#Perform the forward procedure starting with model3 with the scope
#for the final model set to include all the two-way interaction terms

model4<-step(model3,scope=list(upper=~(race + sex + homeLang + logFamIncome + mathInt + mathEff + fGrade + 
                                         hoursHW + calculus + logmathYears + scholarship + sqrtstudentsHE)^2),direction="forward",data=data)

summary(model4)
#  Call:
#  lm(formula = score ~ race + sex + homeLang + logFamIncome + mathInt + 
#       mathEff + fGrade + hoursHW + calculus + logmathYears + scholarship + 
#       sqrtstudentsHE + sex:mathEff + race:homeLang + logFamIncome:sqrtstudentsHE + 
#       sex:fGrade + mathInt:scholarship + mathInt:mathEff, data = newdata)