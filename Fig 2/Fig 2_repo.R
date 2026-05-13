setwd("C:/Users/memslie/OneDrive - Australian Institute of Marine Science/Desktop/working folder/change in coral assemblages/text/for submission/code and data for repo/Fig 2")

library(tidyverse)
library(INLA)
library(purrr)
library(tibble)


load(file='comp2021.w.RData')

head(comp2021.w)
str(comp2021.w)


newdata <- comp2021.w %>% 
  dplyr::select(A_SECTOR, SHELF, cREPORT_YEAR) %>%
  distinct() %>%
  mutate(n.points = NA,
         PROJREEF = NA, 
         PROJREEFSITE = NA, 
         PROJREEFSITETRANSECT = NA,
         total.points=1)

newdata.i <- 1:nrow(newdata) + (nrow(comp2021.w))
dat = comp2021.w %>% bind_rows(newdata) |> 
  mutate(tran_length=ifelse(P_CODE=='IN',30,50)) 


### richness INLA

rich.inla<-inla(Richness~A_SECTOR*SHELF*cREPORT_YEAR +
                  f(PROJREEF, model = 'iid') +
                  f(PROJREEFSITE, model = 'iid') +
                  f(PROJREEFSITETRANSECT, model = 'iid'),
                data=dat,
                #Ntrials = (dat$total.points),
                family="poisson",
                control.predictor = list(link = 1),
                control.compute = list(config = TRUE))

summary(rich.inla)


###########################################################################################
##comparing SR from start to end and max to end

## extract posteriors


X <- model.matrix(
  ~ A_SECTOR * SHELF * cREPORT_YEAR,
  data = newdata
)


pred_sum <- newdata %>%
  bind_cols(rich.inla$summary.fitted.values[newdata.i, ])

nsamp <- 2000
samps <- inla.posterior.sample(nsamp, rich.inla)


pred_names <- paste0("Predictor:", newdata.i)
all(pred_names %in% rownames(samps[[1]]$latent))

# Extract posterior draws for the appended prediction rows
eta_draws <- sapply(samps, function(s) {
  s$latent[pred_names, 1]
})

eta_draws <- t(eta_draws)
colnames(eta_draws) <- pred_names

dim(eta_draws)

mu_draws <- exp(eta_draws)


#Attach prediction ids to newdata
newdata_pred <- newdata %>%
  mutate(pred_col = seq_len(n()))

### summary table

rich_summary <- newdata_pred %>%
  group_by(A_SECTOR, SHELF) %>%
  group_modify(\(.x, .g) {
    
    yrs  <- as.numeric(as.character(.x$cREPORT_YEAR))
    cols <- .x$pred_col
    
    mu_sub <- mu_draws[, cols, drop = FALSE]
    
    first_col <- which.min(yrs)
    last_col  <- which.max(yrs)
    peak_col  <- apply(mu_sub, 1, which.max)
    
    first_draws <- mu_sub[, first_col]
    last_draws  <- mu_sub[, last_col]
    peak_draws  <- mu_sub[cbind(seq_len(nrow(mu_sub)), peak_col)]
    peak_years  <- yrs[peak_col]
    
    # absolute effect sizes
    diff_first_last <- last_draws - first_draws
    diff_peak_last  <- last_draws - peak_draws
    
    # percentage change of second relative to first / peak
    pct_first_last <- (last_draws - first_draws) / first_draws * 100
    pct_peak_last  <- (last_draws - peak_draws)  / peak_draws  * 100
    
    tibble(
      first_year = min(yrs),
      last_year  = max(yrs),
      peak_year  = as.numeric(names(which.max(table(peak_years)))),
      
      first_last_years = paste0(min(yrs), " - ", max(yrs)),
      peak_last_years  = paste0(
        as.numeric(names(which.max(table(peak_years)))),
        " - ",
        max(yrs)
      ),
      
      first_mean = mean(first_draws),
      first_lwr  = quantile(first_draws, 0.025),
      first_upr  = quantile(first_draws, 0.975),
      
      last_mean = mean(last_draws),
      last_lwr  = quantile(last_draws, 0.025),
      last_upr  = quantile(last_draws, 0.975),
      
      peak_mean = mean(peak_draws),
      peak_lwr  = quantile(peak_draws, 0.025),
      peak_upr  = quantile(peak_draws, 0.975),
      
      # absolute effects
      first_last_mean = mean(diff_first_last),
      first_last_lwr  = quantile(diff_first_last, 0.025),
      first_last_upr  = quantile(diff_first_last, 0.975),
      
      peak_last_mean = mean(diff_peak_last),
      peak_last_lwr  = quantile(diff_peak_last, 0.025),
      peak_last_upr  = quantile(diff_peak_last, 0.975),
      
      # percentage effects
      pct_first_last_mean = mean(pct_first_last),
      pct_first_last_lwr  = quantile(pct_first_last, 0.025),
      pct_first_last_upr  = quantile(pct_first_last, 0.975),
      
      pct_peak_last_mean = mean(pct_peak_last),
      pct_peak_last_lwr  = quantile(pct_peak_last, 0.025),
      pct_peak_last_upr  = quantile(pct_peak_last, 0.975),
      
      # posterior probability of decline
      prob_decline_first_last = mean(pct_first_last < 0),
      prob_decline_peak_last  = mean(pct_peak_last < 0),
      
      # optional summaries of peak year uncertainty
      peak_year_median = median(peak_years),
      peak_year_lwr    = quantile(peak_years, 0.025),
      peak_year_upr    = quantile(peak_years, 0.975)
    )
  }) %>%
  ungroup() %>%
  mutate(A_SECTOR = factor(A_SECTOR,
                           levels = c("CG","PC","CL","CA","IN","TO","WH","PO","SW","CB"))) %>%
  as.data.frame()

head(rich_summary)


######
# plot effect size

sector_cols <- c(
  CG = "#A6CEE3",  # Cape Grenville
  PC = "#1F78B4",  # Princess Charlotte Bay
  CL = "#B2DF8A",  # Cooktown-Lizard
  CA = "#33A02C",  # Cairns
  IN = "#FB9A99",  # Innisfail
  TO = "#E31A1C",  # Townsville
  WH = "#FDBF6F",  # Whitsunday
  PO = "#FF7F00",  # Pompey
  SW = "#CAB2D6",  # Swain
  CB = "#6A3D9A"   # Capricorn-Bunker
)


rich_summary_short  <- rich_summary |>
  dplyr::select(
    A_SECTOR, SHELF, first_year, last_year, peak_year,
    first_last_mean, first_last_lwr, first_last_upr,
    peak_last_mean,  peak_last_lwr,  peak_last_upr,
    prob_decline_first_last, prob_decline_peak_last
  )

effects_long <- rich_summary_short |>
  pivot_longer(
    cols = c(first_last_mean, first_last_lwr, first_last_upr,
             peak_last_mean,  peak_last_lwr,  peak_last_upr),
    names_to = c("comparison", ".value"),
    names_pattern = "(first_last|peak_last)_(mean|lwr|upr)"
  )

prob_long <- rich_summary_short |>
  dplyr::select(A_SECTOR, SHELF, prob_decline_first_last, prob_decline_peak_last) |>
  pivot_longer(
    cols = c(prob_decline_first_last, prob_decline_peak_last),
    names_to = "comparison",
    names_pattern = "prob_decline_(first_last|peak_last)",
    values_to = "prob_decline"
  )

rich_summary_short <- effects_long |>
  left_join(prob_long, by = c("A_SECTOR", "SHELF", "comparison")) |> 
  mutate(prob_lab = sprintf("P=%.2f", prob_decline),
         colour_use = ifelse(
           prob_decline >= 0.95 | prob_decline <= 0.5,
           "black",
           sector_cols[A_SECTOR]
         )) |> 
  
  as.data.frame()


head(rich_summary_short)

sector_labels <- setNames(locs$label, c("CG","PC","CL","CA","IN","TO","WH","PO","SW","CB"))

########
# 260326 Fig 2 ------------------------------------------------------------


sr_effects_plot <- ggplot(rich_summary_short,
                          aes(x=mean,y=A_SECTOR,shape=comparison,fill=A_SECTOR,colour=colour_use))+
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(
    aes(xmin = lwr , xmax = upr),
    size = 0.75,
    stroke=1.25,
    position=position_dodge(width=0.5)
  ) +
  
  facet_wrap(~SHELF, nrow = 1,
             labeller = labeller(SHELF = c(
               I = "Inner shelf",
               M = "Mid-shelf",
               O = "Outer shelf"
             )
             )
  )+
  scale_fill_manual(values = sector_cols) +
  scale_colour_identity() +
  scale_shape_manual(values=c(21,22),
                     name = "Comparison",
                     # values = c("first_last" = "black",
                     #            "peak_last"  = "grey50"),
                     labels = c("First vs last", "Peak vs last"))+
  scale_x_continuous('Difference in Richness (mean ± 95% UI)')+
  scale_y_discrete('Latitudinal sector',
                   limits = rev(levels(rich_summary$A_SECTOR)),
                   labels=sector_labels)+
  theme_classic()+
  theme(axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.title.y=element_text(size=12,face='bold'),
        axis.title.x=element_text(size=12,face='bold'),
        strip.text = element_text(size=12,face='bold'))+
  guides(fill='none')

sr_effects_plot

ggsave(sr_effects_plot,file='Main figs/sr_effects_plots.png',height=7,width=9)
