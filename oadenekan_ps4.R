library(readr)
library(dplyr)
library(ggplot2)

# part 1 : sample statistics
gss = read.csv("http://politicaldatascience.com/PDS/Datasets/GSS-data.csv")

# convert poleff11 to numeric
gss_num = gss %>%
  mutate(poleff11 = recode(poleff11, "Strongly agree" = 1,
                           "Agree" = 2,
                           "Neither agree nor disagree" = 3,
                           "Disagree" = 4,
                           "Strongly disagree" = 5,
                           .default = NA_real_))

# proportion of people who feel like they have  say : agree and storngly agree
prop_with_say = sum(((gss_num$poleff11 == 1) | (gss_num$poleff11 == 2)), na.rm = TRUE)/nrow(gss_num)
# the proportion of people who feel like they have a say is 0.2017

# proportion of people who have a say in sample of 25
samp_indices = sample(1:nrow(gss_num), 25)
gss_sample = gss_num[samp_indices,]
prop_with_say = sum(((gss_sample$poleff11 == 1) | (gss_sample$poleff11 == 2)), na.rm = TRUE)/nrow(gss_sample)
# the proportion of people who feel like they have a say is 0.24

# 500 trials of a given sample size
sampling = function(sample_size, trials, gss_num) {
  mean_feeling = c()
  for (trial in 1:trials) {
    samp_indices = sample(1:nrow(gss_num), sample_size)
    gss_sample = gss_num[samp_indices,]
    trial_mean = mean(gss_sample$poleff11, na.rm = TRUE)
    mean_feeling = c(mean_feeling, trial_mean)
  }
  return(mean_feeling)
}
trials_25 = data.frame(means = sampling(25, 500, gss_num)) # 500 trials of sample of 25
trials_100 = data.frame(means = sampling(100, 500, gss_num)) # 500 trials of sample of 25

# plotting histograms
ggplot(data=trials_25, aes(x=means))+
  geom_histogram() +
  theme_minimal() +
  xlab("bins") +
  ylab("mean of sample")
ggsave("trials_25.png")

ggplot(data=trials_100, aes(x=means))+
  geom_histogram() +
  theme_minimal() +
  xlab("bins") +
  ylab("mean of sample")
ggsave("trials_100.png")

# when we use a larger sample size, the peak of the distribution is more clearly Gaussian:
# more samples fall in the middle bins


# part 2 : supervised learning
polling = read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/
PollingCandidateData92-16.csv")


