source('common.R')
dist_mean <- 10e3
dist_cv <- 2.5
dist_sd <- dist_mean * dist_cv

sims <- 5e3
norms <- rnorm(sims, dist_mean, dist_sd)

norms %>% head()
hist(norms)
tbl_sims <- data.frame(
    x_exp = rexp(sims, 1 / dist_mean)
  , x_norm = rnorm(sims, dist_mean, dist_sd)
)
tbl_sims %>% 
  ggplot(aes(x = x_norm)) +
  geom_histogram()
tbl_sims %>% 
  ggplot(aes(x_norm)) + 
  geom_density()
tbl_sims %>% 
  ggplot() + 
  geom_density(aes(x_exp), alpha = 0.8, fill = 'red') + 
  geom_density(aes(x_norm), alpha = 0.8, fill = 'blue')
plot_points <- 500
x_lims <- dist_mean + c(-1, 1) * 3 * dist_sd
tbl_plot <- data.frame(
    x = seq(x_lims[1], x_lims[2], length.out = plot_points)
  )
tbl_plot$density_norm <- dnorm(tbl_plot$x, dist_mean, dist_sd)
tbl_plot %>% 
  ggplot(aes(x, density_norm)) + 
  geom_line()
tbl_plot$cdf_norm <- pnorm(tbl_plot$x, dist_mean, dist_sd)
tbl_plot %>% 
  ggplot(aes(x, cdf_norm)) + 
  geom_line()
tails <- c(0.95, 0.98, 0.99)
qnorm(tails, dist_mean, dist_sd)
set.seed(1234)
policy_years <- 2001:2010
freq <- 1e3
num_claims <- rpois(length(policy_years), freq)
dist_means <- dist_mean * 1.05 ^ (policy_years - min(policy_years))

severity <- mapply(rnorm, num_claims, dist_means, MoreArgs = list(sd = dist_sd))
tbl_claim <- data.frame(
    policy_year = rep(policy_years, num_claims)
  , severity = unlist(severity))
tbl_claim %>% 
  ggplot(aes(severity)) + 
  geom_density(alpha = 0.6)
tbl_claim %>% 
  ggplot(aes(severity, fill = as.factor(policy_year))) + 
  geom_density(alpha = 0.6)
tbl_claim %>% 
  ggplot(aes(severity, fill = as.factor(policy_year))) + 
  geom_density() + 
  facet_wrap(~ policy_year)
tbl_claim %>% 
  group_by(policy_year) %>% 
  summarise(med = median(severity))
set.seed(1234)
sample(1:100, 10)
sample(1:3, prob = c(1, 1, 100), replace = TRUE)
set.seed(1234)
letters[sample(length(letters))]
