library(ggplot2)
library(ggthemes)
library(extrafont)

# Asthma-related hospitalization
df <- data.frame(study=c('10 year, 1:1 (main setting)', '10 year, 1:2', '10 year, 1:4', '10 year, stratification', '1 year, 1:1', '5 year, 1:1', 'ITT, 1:1', 'As treated, 1:1'),
                 index=8:1,
                 effect=c(0.76, 0.82, 0.85, 0.78, 0.61, 0.77, 0.76, 0.69),
                 lower=c(0.53, 0.59, 0.62, 0.61, 0.39, 0.53, 0.53, 0.46),
                 upper=c(1.09, 1.14, 1.15, 0.99, 0.94, 1.11, 1.09, 1.03))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  #scale_x_continuous(limits = c(0, 1.2), breaks = c(0, 0.25, 0.5, 0.75, 1)) + 
  theme(text=element_text(size=14, family="Arial"))

# New-onset T2DM
df <- data.frame(study=c('10 year, 1:1 (main setting)', '10 year, 1:2', '10 year, 1:4', '10 year, stratification', '1 year, 1:1', '5 year, 1:1', 'ITT, 1:1', 'As treated, 1:1'),
                 index=8:1,
                 effect=c(2.33, 1.27, 1.46, 1.99, 1.67, 2.80, 2.33, 1.20),
                 lower=c(0.94, 0.64, 0.77, 1.25, 0.41, 1.07, 0.94, 0.52),
                 upper=c(6.59, 2.49, 2.73, 3.16, 8.13, 8.66, 6.59, 2.84))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  scale_x_continuous(limits = c(0, 8.7)) + 
  theme(text=element_text(size=14, family="Arial"))

# New-onset HTN
df <- data.frame(study=c('10 year, 1:1 (main setting)', '10 year, 1:2', '10 year, 1:4', '10 year, stratification', '1 year, 1:1', '5 year, 1:1', 'ITT, 1:1', 'As treated, 1:1'),
                 index=8:1,
                 effect=c(1.71, 1.42, 1.48, 2.41, 0.90, 1.67, 1.82, 1.75),
                 lower=c(0.95, 0.87, 0.96, 1.78, 0.36, 0.89, 1.02, 0.96),
                 upper=c(3.17, 2.31, 2.25, 3.25, 2.23, 3.23, 3.37, 3.31))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  scale_x_continuous(limits = c(0, 3.4)) + 
  theme(text=element_text(size=14, family="Arial"))
