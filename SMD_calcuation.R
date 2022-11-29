
##SMD = Difference in mean outcome between groups / standard deviation of outcome among participants

covariate_balance1 <- readRDS(".rds")
covariate_balance2 <- readRDS(".rds")
covariate_balance3 <- readRDS(".rds")

data1 <- covariate_balance1 %>% filter(covariate_id == '')
data2 <- covariate_balance2 %>% filter(covariate_id == '')
data3 <- covariate_balance3 %>% filter(covariate_id == '')

#비교군의 population은 직접 찾아서 넣었습니다. 아래는 before/after와 target/comparator별 population수 입니다.
before_t_data1_pop <- ''
before_t_data2_pop <- ''
before_t_data3_pop <- ''

before_c_data1_pop <- ''
before_c_data2_pop <- ''
before_c_data3_pop <- ''

after_t_data1_pop <- ''
after_t_data2_pop <- ''
after_t_data3_pop <- ''

after_c_data1_pop <- ''
after_c_data2_pop <- ''
after_c_data3_pop <- ''

####
target_mean <- (data1$target_mean_before*before_t_data1_pop + data2$target_mean_before*before_t_data2_pop + data3$target_mean_before*before_t_data3_pop)/(before_t_data1_pop+before_t_data2_pop+before_t_data3_pop)
comparator_mean <- (data1$comparator_mean_before*before_c_data1_pop + data2$comparator_mean_before*before_c_data2_pop + data3$comparator_mean_before*before_c_data3_pop)/(before_c_data1_pop+before_c_data2_pop+before_c_data3_pop)
total_mean <- (target_mean*(before_t_data1_pop+before_t_data2_pop+before_t_data3_pop) + comparator_mean*(before_c_data1_pop+before_c_data2_pop+before_c_data3_pop))/(before_t_data1_pop+before_t_data2_pop+before_t_data3_pop + before_c_data1_pop+before_c_data2_pop+before_c_data3_pop)
total_with_covariate <- total_mean*(before_t_data1_pop+before_t_data2_pop+before_t_data3_pop + before_c_data1_pop+before_c_data2_pop+before_c_data3_pop)
total_without_covariate <- (before_t_data1_pop+before_t_data2_pop+before_t_data3_pop + before_c_data1_pop+before_c_data2_pop+before_c_data3_pop) - total_with_covariate


smd_before <- abs(target_mean-comparator_mean)/sqrt((total_mean*total_mean*total_without_covariate+(1-total_mean)*(1-total_mean)*total_with_covariate)/(before_t_data1_pop+before_t_data2_pop+before_t_data3_pop + before_c_data1_pop+before_c_data2_pop+before_c_data3_pop)) ##sd계산은 편차 제곱의 평균을 이용

  
####

target_mean <- (data1$target_mean_after*after_t_data1_pop + data2$target_mean_after*after_t_data2_pop + data3$target_mean_after*after_t_data3_pop)/(after_t_data1_pop+after_t_data2_pop+after_t_data3_pop)
comparator_mean <- (data1$comparator_mean_after*after_c_data1_pop + data2$comparator_mean_after*after_c_data2_pop + data3$comparator_mean_after*after_c_data3_pop)/(after_c_data1_pop+after_c_data2_pop+after_c_data3_pop)
total_mean <- (target_mean*(after_t_data1_pop+after_t_data2_pop+after_t_data3_pop) + comparator_mean*(after_c_data1_pop+after_c_data2_pop+after_c_data3_pop))/(after_t_data1_pop+after_t_data2_pop+after_t_data3_pop + after_c_data1_pop+after_c_data2_pop+after_c_data3_pop)
total_with_covariate <- total_mean*(after_t_data1_pop+after_t_data2_pop+after_t_data3_pop + after_c_data1_pop+after_c_data2_pop+after_c_data3_pop)
total_without_covariate <- (after_t_data1_pop+after_t_data2_pop+after_t_data3_pop + after_c_data1_pop+after_c_data2_pop+after_c_data3_pop) - total_with_covariate


smd_after <- abs(target_mean-comparator_mean)/sqrt((total_mean*total_mean*total_without_covariate+(1-total_mean)*(1-total_mean)*total_with_covariate)/(after_t_data1_pop+after_t_data2_pop+after_t_data3_pop + after_c_data1_pop+after_c_data2_pop+after_c_data3_pop)) ##sd계산은 편차 제곱의 평균을 이용

