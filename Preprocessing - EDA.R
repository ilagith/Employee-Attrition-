
# Load packages ---------------------------------------------------------------
library(caret)
library(cowplot)
library(creditmodel)
library(data.table)
library(dplyr)
library(fastDummies)
library(ggcorrplot)
library(ggplot2)
library(psych)
library(styler)
library(tidytable)

# Load the IBM HR Employee Attrition & Performance dataset --------------------
setwd("~/Desktop/Thesis/input")
employees <- read.csv(
  "WA_Fn-UseC_-HR-Employee-Attrition.csv",
  stringsAsFactors = FALSE
)

# Explore employees data structure:
dim(employees)
head(employees)
str(employees)
describe(employees)
summary(employees)
table(employees$Attrition)

# Data Cleaning ---------------------------------------------------------------

# From the summary statistics there is evidence that that the variables
# 'EmployeeCount', 'Over18', 'StandardHours' are constants. Further, we noticed
# that 'EmployeeNumber' is only composed of ordered unique values as each
# employee has unique id code assigned. Indeed:
apply(employees[c("EmployeeCount", "Over18", "StandardHours")], 2, table)
table(employees$EmployeeNumber)
table(employees$Over18)
table(employees$EmployeeCount)
table(employees$StandardHours)
# Hence, we decided to remove 'EmployeeCount', 'Over18','StandardHours', and
# 'EmployeeNumber' since they don't provide useful insights for our analysis.
employees <- employees %>%
  select(-c(EmployeeCount, EmployeeNumber, Over18, StandardHours))

# Use a function written to detect NA values: it is a complete dataset!
detect_na <- function(x) {
  sum(is.na(x))
}
apply(employees, MARGIN = 2, detect_na)

# Explore the dataset to delete duplicated rows: no duplicated rows found!
anyDuplicated(employees)

# Pearson's r correlation matrix to check collinearities between numeric
# variables
employees %>%
  select(where(is.numeric)) %>%
  cor(method = "pearson") %>%
  ggcorrplot(
    lab_size = 1.4, tl.cex = 6, hc.order = TRUE, type = "lower",
    lab = TRUE, colors = c("steelblue", "white", "firebrick"),
  )

# JobLevel and MonthlyIncome are very strong correlated. Since JobLevel is 
# almost a duplicate of the variable JobRole, we decided to remove JobLevel from 
# our dataset. Indeed, a manager has a JobLevel of 5, which is an information 
# already available in our data. Moreover the time variable 'YearsAtCompany' is 
# strong correlated with YearsInCurrentRole and YearsWithCurrManager. As such, 
# we willl remove both variables from our dataset. 
employees <- employees %>%
  select(-c("JobLevel"))

employees <- employees %>%
  select(-c("YearsInCurrentRole", "YearsWithCurrManager", "PercentSalaryHike"))

# Cramers' V correlation matrix with categorical variables
employees %>%
  select(where(negate(is.numeric))) %>%
  char_cor() %>%
  ggcorrplot(
    lab_size = 3, tl.cex = 8, hc.order = TRUE, type = "lower",
    lab = TRUE, colors = c("steelblue", "white", "firebrick"),
  )

# Department and JobRole are very strong correlated variables. Indeed, they
# carry similar information as a jobrole depends on the department an employee 
# is assigned to. Since JobRole is most suited to potentially answer to our 
# second research question, we decided to retain JobRole in our dataset.
employees <- employees %>%
  select(-c("Department"))

# Exploratory Data Analysis ---------------------------------------------------

# Barplot of employee attrition by years at the company: the majority of the
# churner employees left before working 10 years for the firm. Moreover, a high
# proportion of employees who remained at the organization for 23-24 year or
# or 31-32-33 years left the company. Overall, the company has the highest
# number of employees remaining for 10 years.
employees %>%
  ggplot(aes(x = YearsAtCompany, fill = Attrition)) +
  geom_bar() +
  theme_light() +
  scale_fill_grey(#values = c("steelblue", "#F9C014")) +
    start = 0.8, end = 0.1) + 
  ggtitle("Employee attrition count by years at the company") +
  scale_x_continuous(name = "Years at the company", breaks = seq(0, 40, by = 5))

# Density plot of employee attrition by total working years: churner employees
# are more densily distributed at the beginning of their career.
employees %>%
  ggplot(aes(x = TotalWorkingYears, fill = Attrition)) +
  geom_density(alpha = 0.9) +
  scale_fill_grey(#values = c("steelblue", "#F9C014")) +
    start = 0.8, end = 0.1) + 
  theme_light() +
  ggtitle("Employee attrition density by total working years") +
  scale_x_continuous(name = "Total working years")

# Barplot of job role by attrition count and percentages: the highest number of
# churner employees are laboratory technicians, sales executives and research
# scientists. However, in terms of percentages sales representative is the job
# role that experienced attrition the most (40% of employees left), followed by
# laboratory technicians (24%), and human resourches (23%) staff. Managers and
# research directors tend to leave the company the least.
employees %>%
  group_by(JobRole, Attrition) %>%
  summarise(total = n()) %>%
  mutate(prop = ifelse(
    Attrition == "Yes", scales::percent(total / sum(total)), ""
  )) %>%
  ggplot(aes(x = JobRole, y = total, fill = Attrition)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "count") +
  geom_text(aes(label = prop),
    size = 3, hjust = -0.6, nudge_y = -7, colour = "#F9C014"
  ) +
  theme_light() +
  ggtitle("Job role by attrition count and churners' percentages") +
  scale_fill_manual(values = c("steelblue", "#F9C014")) +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 8)
  ) +
  coord_flip()

# Boxplots of job role by years at the company filled with attrition: sales
# representatives, laboratory technicians, human resources and research
# scientistis are the job roles for which employees stay less years at the
# company. On the contrary, managers and research directors are the roles for
# which employees remain at the company for longer.
ggplot(employees, aes(x = JobRole, y = YearsAtCompany, fill = Attrition)) +
  geom_boxplot() +
  theme_light() +
  ggtitle("Job role by years at the company attrition") +
  scale_x_discrete(name = "Overtime") +
  scale_y_continuous(name = "Years at the company") +
  scale_fill_grey(#values = c("steelblue", "#F9C014")) +
    start = 0.8, end = 0.1) + 
  
  theme(
    axis.title.y = element_blank(),
    panel.grid.major = element_line(size = 0.1),
    panel.grid.minor = element_line(size = 0.1),
    panel.background = element_rect(size = 0.5), 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size=14)
  ) +
  coord_flip()

# Pies of overtime by employee attrition %: employees with overtime tend to
# leave the firm more than employees who only work without overtiming.
no_overtime <- employees %>%
  group_by(OverTime, Attrition) %>%
  summarise(total = n()) %>%
  filter(OverTime == "No") %>%
  mutate(prop = ifelse(Attrition == "Yes", scales::percent(total / sum(total)),
    ""
  )) %>%
  ggplot(aes(x = "", y = total, fill = Attrition)) +
  geom_bar(stat = "identity", colour = "white") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  geom_text(aes(x = 1, y = total, label = prop, vjust = -1.3), colour = 'white', size = 2) +
  ggtitle("No overtime employee attrition %") +
  #scale_fill_manual(values = c("steelblue", "#F9C014")) +
  scale_fill_grey(#values = c("steelblue", "#F9C014")) +
    start = 0.8, end = 0.1) +
  theme(plot.title = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)
  )

employees %>%
  group_by(OverTime, Attrition) %>%
  summarise(total = n()) %>%
  filter(OverTime == "Yes") %>%
  mutate(prop = ifelse(Attrition == "Yes", scales::percent(total / sum(total)),
    ""
  )) %>%
  ggplot(aes(x = "", y = total, fill = Attrition)) +
  geom_bar(stat = "identity", colour = "white") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  geom_text(aes(x = 1.1, y = total, label = prop, vjust = -4), colour = 'white', size = 6) +
  ggtitle("Overtime employee attrition %") +
  #scale_fill_manual(values = c("steelblue", "#F9C014")) +
  scale_fill_grey(#values = c("steelblue", "#F9C014")) +
    start = 0.8, end = 0.1)+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

plot_grid(no_overtime, overtime, scale = 0.8)

# Barplot of business travel by age attrition: despite differences in age, non-
# travel employees tends to churn way less compared with travelling employees.
# Travelling employees between 18 and 30 tend to leave the company more than
# their older colleagues. A partiucularly high attrition rate is observed for
# young frequent travellers employees. Overall, in the company employees do not
# travel often.
employees %>%
  ggplot(aes(x = Age, fill = Attrition)) +
  geom_bar() +
  facet_grid(BusinessTravel ~ .) +
  theme_light() +
  scale_fill_manual(values = c("steelblue", "#F9C014")) +
  ggtitle("Business travel by age attrition")

# Scatterplot of monthly income by years at the company attrition: employees
# that remained at the company for more than 20 years had an income above 10k.
# Employees with relatively low monthly income tends to churn more than better
# remunerated employees.
employees %>%
  ggplot(aes(x = YearsAtCompany, y = MonthlyIncome, colour = Attrition)) +
  geom_point() +
  theme_light() +
  ggtitle("Monthly income by years at the company attrition") +
  scale_x_continuous(name = "Years at the company") +
  scale_y_continuous(name = "Monthly income") +
  scale_colour_manual(values = c("steelblue", "#F9C014")) +
  theme(
    panel.grid.major = element_line(size = 0.1),
    panel.grid.minor = element_line(size = 0.1),
    panel.background = element_rect(size = 0.5)
  )

# Barplot of employee attrition counting years in current role: as the number of
# years spent working in the current role increases, the quantity of churner
# employees tend to decrease.
employees %>%
  ggplot(aes(x = YearsInCurrentRole, fill = Attrition)) +
  geom_bar() +
  scale_fill_manual(values = c("steelblue", "#F9C014")) +
  theme_light() +
  ggtitle("Employee attrition counting years in current role") +
  scale_x_continuous(name = "Years in current role")

# Histogram showing job involvement employee attrition: the least employees are
# involved in doing their job, the most they tend to churn. Overall, most of
# employees are involved at the company.
employees %>%
  ggplot(aes(x = JobInvolvement, fill = Attrition)) +
  geom_histogram(bins = 10) +
  theme_light() +
  ggtitle("Job involvement employee attrition") +
  scale_fill_manual(
    values = c("steelblue", "#F9C014")
  )

# Barplot of marital status by years at the company attrition: the largest
# number of employee attrition tend to happen among single people. Married
## people are the most employed category per marital status. Divorced tend to
# remain at the company for longer.
employees %>%
  ggplot(aes(x = YearsAtCompany, fill = Attrition)) +
  geom_bar() +
  facet_grid(MaritalStatus ~ .) +
  scale_fill_manual(values = c("steelblue", "#F9C014")) +
  theme_light() +
  ggtitle("Marital status by years at the company attrition") +
  scale_x_continuous(name = "Years at the company")

# Distance from home by monthly income attrition jitterplot: regardless how
# far they are from home, employees with monthly income over 15k churn way
# less than employees with monthly income below 15k. Eployees with a low
# monthly income tend to leave more than better paid employees nonetheless
# their distance from home.
employees %>%
  ggplot(aes(x = MonthlyIncome, y = DistanceFromHome, colour = Attrition)) +
  geom_jitter() +
  theme_light() +
  ggtitle("Distance from home by monthly income attrition") +
  scale_x_continuous(name = "Monthly income") +
  scale_y_continuous(name = "Distance from Home") +
  scale_colour_manual(values = c("steelblue", "#F9C014")) +
  theme(
    panel.grid.major = element_line(size = 0.1),
    panel.grid.minor = element_line(size = 0.1),
    panel.background = element_rect(size = 0.5)
  )

employees %>%
  ggplot(aes(y = NumCompaniesWorked, x = Attrition)) +
  geom_violin() +
  scale_fill_grey() +
  theme_light() +
  ggtitle("Monthly income attrition") +
  scale_y_continuous(name = "Monthly income")

# Modeling --------------------------------------------------------------------- 

# encoding event variable: 1 for Attrition == 'Yes', 0 for Attrition == 'No'
employees$Attrition <- ifelse(employees$Attrition == "Yes", 1, 0)

# dummy encode categorical variables without including them twice
employees <- employees %>%
  dummy_cols(remove_first_dummy = TRUE) %>%
  select(-c(
    "BusinessTravel", "EducationField", "Gender", "JobRole",
    "MaritalStatus", "OverTime"
  )) %>%
  rename_with(~ gsub(" ", "_", .x, fixed = TRUE))

# Split train and test set
set.seed(21)
train_index <- createDataPartition(
  y = employees$Attrition, p = .7, list = FALSE
)
train_employees <- employees[train_index, ]
test_employees <- employees[-train_index, ]

# Export train and test set 
write.csv(train_employees, "~/Desktop/Thesis/input/train_employees.csv")
write.csv(test_employees, "~/Desktop/Thesis/input/test_employees.csv")