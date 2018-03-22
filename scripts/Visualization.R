source('common.R')
library(raw)
data("RegionExperience")

plot(RegionExperience$PolicyYear, RegionExperience$NumClaims)
plot(
  RegionExperience$PolicyYear
  , RegionExperience$NumClaims
  , pch = 19
  , xlab = 'Policy Year'
  , ylab = '# of claims')
library(ggplot2)
library(dplyr)
plt_base <- ggplot(RegionExperience)

plt_base

data(COTOR2)
ggplot(COTOR2)
ggplot(1:10)
plt_base <- plt_base + aes(x = PolicyYear, y = NumClaims)

plt_base <- ggplot(RegionExperience, aes(PolicyYear, NumClaims))
plt_base <- plt_base + geom_point()
plt_base
RegionExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumPolicies)) + 
  geom_point()
RegionExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumPolicies)) + 
  geom_point() + 
  geom_line()
RegionExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumPolicies)) + 
  geom_point() + 
  geom_line(aes(color = Region))
RegionExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumPolicies)) + 
  geom_point() + 
  geom_smooth(method = lm, aes(color = Region))
plt <- RegionExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumClaims)) + 
  geom_point()
plt + scale_y_continuous(labels = scales::comma)
plt + scale_x_continuous(breaks = 2001:2010)
my_breaks <- function(lims) {
  seq(ceiling(lims[1]), floor(lims[2]), by = 1)
}
plt + scale_x_continuous(breaks = my_breaks)
ggplot(RegionExperience, aes(x = PolicyYear, y = NumClaims, color = Region)) + 
  geom_point() + 
  facet_wrap(~ Region)
plt + labs(x = 'Policy year', y = '# of claims')
