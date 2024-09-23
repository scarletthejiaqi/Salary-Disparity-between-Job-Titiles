library(tidyverse)
library(dplyr)
monthly_salary = function(data) {
  data %>% 
    mutate(monthly_salary = Salary / 12)
}


