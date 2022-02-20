library(foreign, pos=14); library(graphics); library(Hmisc); library(pwr); library(psych); library(pastecs); library(graphics); library(ggplot2); library(Hmisc); library(car); library(lsr); library(ppcor); library(MASS); library(bda); library(readxl); library(nnet); library(ez); library(multcomp); library(gmodels); library(carData); library(boot)
str(reshaped_scored)
# 결과변수
# credit_final (최종 credit)
table(reshaped_scored$credit)
ggplot(reshaped_scored, aes(credit))+geom_bar()
cont_credit_1_or_2<-read_excel("/Users/jeongmoonwon/Downloads/통세/con_credit12.xlsx")
head(score)
g <- ggplot(score, aes(x=reorder(CNT, ratio), y = ratio, fill=cont)) + geom_bar(stat='identity') + coord_flip() + scale_fill_grey()
plot(g)
head(logdata_scored)

# credit_dia (다이어그램 정답 여부)
table(reshaped_scored$credit_dia)
ggplot(reshaped_scored, aes(credit_dia))+geom_bar()
# credit_dia2 (다이어그램 정답 여부)
table(reshaped_scored$credit_dia2)
13616 /31677
15592 /31677
2469 / 31677
reshaped_scored$credit_dia2=as.factor(reshaped_scored$credit_dia2) 
ggplot(reshaped_scored, aes(credit_dia2))+geom_bar()
# n_diaCL (다이어그램에서 맞게 그린 선의 개수)
reshaped_scored$n_diaCL=as.factor(reshaped_scored$n_diaCL) 
table(reshaped_scored$n_diaCL)
ggplot(reshaped_scored, aes(n_diaCL))+geom_bar()
2494 /31677
1923 /31677
5537 /31677
19254 /31677
2469  /31677

# 실행변수
# num_total	전체 실행 횟수
reshaped_scored<-read_excel("/Users/jeongmoonwon/Downloads/통세/final.xlsx")
head(reshaped_scored)
quantile(reshaped_scored$num_total, na.rm=TRUE)
describe(reshaped_scored$num_total)
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$num_total > 123 ), ]
hist(reshaped_scored_out$num_total, breaks=300)
ggplot(reshaped_scored, aes(credit))+geom_bar()


# time_total 전체 실행 시간
quantile(reshaped_scored$time_total, na.rm=TRUE)
describe(reshaped_scored$time_total)
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$time_total > 2150 ), ]
hist(reshaped_scored_out$time_total, breaks=300)
cor(reshaped_scored$num_total,reshaped_scored$time_total, use = 'complete.obs')
# act_apply	Apply 버튼 클릭 횟수
quantile(reshaped_scored$num_cancel, na.rm=TRUE)
describe(reshaped_scored$num_cancel)
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$num_cancel > 400 ), ]
hist(reshaped_scored_out$num_cancel, breaks=350)
# act_dia	다이어그램 클릭 횟수
quantile(reshaped_scored$act_dia, na.rm=TRUE)
describe(reshaped_scored$act_dia)
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$act_dia > 60 ), ]
hist(reshaped_scored_out$act_dia, breaks=100)
# timetoD	올바른 다이어그램을 그린 학생들이 최종 답까지 걸린 시간
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$timetoD ==99 ), ]
reshaped_scored_out <- reshaped_scored_out[!(reshaped_scored_out$timetoD == 999 ), ]
quantile(reshaped_scored_out$timetoD, na.rm=TRUE)
describe(reshaped_scored_out$timetoD)
hist(reshaped_scored_out$timetoD, breaks=300)
# timetoFA	첫번째 행동까지 걸린 시간
quantile(reshaped_scored$timetoFA, na.rm=TRUE)
describe(reshaped_scored$timetoFA)
cor(reshaped_scored$timetoFA,reshaped_scored$time_total, use = 'complete.obs')
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$timetoFA > 2100 ), ]
hist(reshaped_scored$timetoFA, breaks=300)


# 행동의미파악변수
reshaped_scored$credit_dia2<-factor(reshaped_scored$credit_dia2, levels = c(999,0,1), labels = c("No Diagram", "Wrong","Correct"))
#VOTAT_e	VOTAT 적용 후, control 변화 없이 Apply 버튼을 클릭한 횟수
quantile(reshaped_scored$total_e_raio, na.rm=TRUE)
describe(reshaped_scored$total_e_raio)
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$total_e_raio ==0.00 ), ]
hist(reshaped_scored_out$total_e_raio, breaks=300)
bar <- ggplot(reshaped_scored, aes(credit_final, total_e_raio))
bar + stat_summary(fun = mean, geom = 'bar', fill = 'White', color = 'black') + stat_summary(fun.reshaped_scored = mean_cl_normal, geom = 'pointrange') + scale_y_continuous()
bar <- ggplot(reshaped_scored, aes(credit_dia2, total_e_raio))
bar + stat_summary(fun = mean, geom = 'bar', fill = 'White', color = 'black') + stat_summary(fun.reshaped_scored = mean_cl_normal, geom = 'pointrange') + scale_y_continuous()
#VOTAT_ev	VOTAT 적용 후, 같은 control만 조작하여 Apply 버튼을 클릭한 횟수
quantile(reshaped_scored$total_ev_raio, na.rm=TRUE)
describe(reshaped_scored$total_ev_raio)
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$total_ev_raio ==0.00 ), ]
hist(reshaped_scored_out$total_ev_raio, breaks=60)
bar <- ggplot(reshaped_scored, aes(credit_dia2, total_ev_raio))
bar + stat_summary(fun = mean, geom = 'bar', fill = 'White', color = 'black') + stat_summary(fun.reshaped_scored = mean_cl_normal, geom = 'pointrange') + scale_y_continuous()
#VOTAT_ec	VOTAT 적용 후, 다른 control을 조작하여Apply 버튼을 클릭한 횟수
quantile(reshaped_scored$total_ec_raio, na.rm=TRUE)
describe(reshaped_scored$total_ec_raio)
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$total_ec_raio ==0.00 ), ]
hist(reshaped_scored_out$total_ec_raio, breaks=300)
bar <- ggplot(reshaped_scored, aes(credit_dia2, total_ec_raio))
bar + stat_summary(fun = mean, geom = 'bar', fill = 'White', color = 'black') + stat_summary(fun.reshaped_scored = mean_cl_normal, geom = 'pointrange') + scale_y_continuous()
#VOTAT_ecv	VOTAT 적용 후, 원래 control은 초기값으로 두고, 다른 control을 조작하여Apply 버튼을 클릭한 횟수
quantile(reshaped_scored$total_ecv_raio, na.rm=TRUE)
describe(reshaped_scored$total_ecv_raio)
reshaped_scored_out <- reshaped_scored[!(reshaped_scored$total_ecv_raio ==0.00 ), ]
hist(reshaped_scored_out$total_ecv_raio, breaks=100)
bar <- ggplot(reshaped_scored, aes(credit_dia2, total_ecv_raio))
bar + stat_summary(fun = mean, geom = 'bar', fill = 'White', color = 'black') + stat_summary(fun.reshaped_scored = mean_cl_normal, geom = 'pointrange') + scale_y_continuous()

cat1<-which(reshaped_scored[,"credit_dia2"]==1)
cat2<-which(reshaped_scored[,"credit_dia2"]==0)
reshaped_scored$credit<-0
reshaped_scored$credit[cat1]<-1
o_logit <- glm(credit ~ total_e_raio+total_ev_raio+total_ec_raio+total_ecv_raio, family = binomial(link = "logit"), reshaped_scored = reshaped_scored)
summary(o_logit)
o_logit <- glm(credit ~ votat_e_ratio+votat_ev_ratio+votat_ec_ratio+votat_ecv_ratio, family = binomial(link = "logit"), reshaped_scored = reshaped_scored)
summary(o_logit)
#process1	Control 조작(Apply)과 다이어그램 작성(Diagram)을 얼마나 번갈아 하는지
#process2 Control 조작(Apply)과 다이어그램 작성(Diagram)을 얼마나 번갈아 하는지 (process1 변수를 세분화)
table(reshaped_scored$process2)
6090 /30760
7874/30760 + 13971/30760
2825/30760
reshaped_scored$credit_final<-factor(reshaped_scored$credit_final, levels = c(0,1,2), labels = c("No Credit", "Partial Credit","Full Credit"))
reshaped_scored$process2=as.factor(reshaped_scored$process2) 
reshaped_scored$credit_dia2<-factor(reshaped_scored$credit_dia2, levels = c(999,0,1), labels = c("No Diagram", "Wrong","Correct"))
ggplot(reshaped_scored, aes(credit_dia2, fill=process2))+geom_bar(position='fill')
ggplot(reshaped_scored, aes(credit_final, fill=process2))+geom_bar(position='fill')
ggplot(reshaped_scored, aes(process2))+geom_bar()

