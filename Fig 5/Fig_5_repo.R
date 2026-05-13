setwd("C:/Users/memslie/OneDrive - Australian Institute of Marine Science/Desktop/working folder/change in coral assemblages/Update with 2025 data_Oct 2025/comp21 half decadal models")

library(tidyverse)
library(INLA)
library(posterior)
library(patchwork)
#load("comp2021.points.RData")
load("../comp2021_Oct2025.RData")

head(comp2021_Oct2025)


############################################################################################################################
##
##    Note - Feb 2026 Murray re-ran the analysis on HPC as there were issues with the code below including incorrect contrast matrices

              
##        created new script 19/02/2026
############################################################################################################################




###############################################################################
#### data wrangling

video_codes<-read.csv("../../video_codes.csv")
video_codes_short<-video_codes %>% dplyr::select(VIDEO_CODE,GROUP_CODE,COMP_2021,COMP_2021_DESCRIPTION) %>%
  filter(!GROUP_CODE %in% c('IN','WA','SG','OT','AB')) %>% dplyr::distinct(GROUP_CODE,COMP_2021,COMP_2021_DESCRIPTION)

load(file='../group_cover_transect.RData')

broad.gps <- read.csv(file='../../comp21_broad_groups_lookup.csv',strip.white=TRUE)

# comp2021_Oct2025 |> mutate(ms_group=factor(ms_group))

comp2021_Oct2025_df <- comp2021_Oct2025 |> 
  filter(!COMP_2021 %in% c('F_SC_SCl_LOB',"COR_CBCF","COR_CE","COR_CL","COR_CMCS")) |> 
  left_join(broad.gps) |> 
  mutate(ms_group=factor(ms_group)) |> 
  filter(ms_group %in% c("Acropora","branching coral","encrusting foliose coral","massive Porites",         
                         "massive submassive coral","solitary coral")) |> 
  dplyr::mutate(P_CODE=factor(P_CODE),
                A_SECTOR=factor(A_SECTOR),
                SHELF=factor(SHELF),
                AIMS_REEF_NAME=factor(AIMS_REEF_NAME),
                cREPORT_YEAR=factor(REPORT_YEAR),
                SITE_NO=factor(SITE_NO),
                TRANSECT_NO=factor(TRANSECT_NO),
                ms_group=factor(ms_group),
                cREPORT_YEAR=factor(REPORT_YEAR),
                cCOMP_2021=factor(COMP_2021),
                SITE_DEPTH=ifelse(P_CODE!="IN",9L,SITE_DEPTH),
                SITE_DEPTH=factor(SITE_DEPTH),
                Reef_unique=as.factor(paste(AIMS_REEF_NAME,REEF_ZONE,SITE_DEPTH))) |> 
  group_by(P_CODE,A_SECTOR,SHELF,AIMS_REEF_NAME,Reef_unique,SITE_DEPTH,REPORT_YEAR,SITE_NO,TRANSECT_NO,
           cREPORT_YEAR,ms_group,cCOMP_2021,COMP_2021) %>%
  dplyr::summarise(cover=sum(COVER),
                   n.points=sum(POINTS),
                   total.points=unique(total.points)) %>% ungroup() |> 
  mutate(Decade=ifelse(REPORT_YEAR %in% c(1993,1994,1995,1996,1997,1998,1999,2000),"1990s",
                       ifelse(REPORT_YEAR %in% c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010),"2000s",
                              ifelse(REPORT_YEAR %in% c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020),"2010s","2020s"))),
         Decade=factor(Decade)) %>% 
  mutate(Decade2=ifelse(REPORT_YEAR %in% c(1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003),"1993-2003",
                        ifelse(REPORT_YEAR %in% c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013),"2004-2013","2014-2023")),
         Decade2=factor(Decade2)) |> 
  mutate(half_decade=ifelse(REPORT_YEAR %in% c(1993,1994,1995),'early_90s',
                            ifelse(REPORT_YEAR %in% c(1996,1997,1998,1999,2000),'late_90s',
                                   ifelse(REPORT_YEAR %in% c(2001,2002,2003,2004,2005),'early_00s',
                                          ifelse(REPORT_YEAR %in% c(2006,2007,2008,2009,2010),'late_00s',
                                                 ifelse(REPORT_YEAR %in% c(2011,2012,2013,2014,2015),'early_10s',
                                                        ifelse(REPORT_YEAR %in% c(2016,2017,2018,2019,2020),'late_10s','early_20s')))))),
         half_decade=factor(half_decade,levels=c('early_90s','late_90s','early_00s','late_00s','early_10s','late_10s','early_20s'))) |> 
  left_join(video_codes_short,by='COMP_2021') |> 
  as.data.frame()


comp2021.points <- comp2021_Oct2025_df

head(comp2021.points)
str(comp2021.points)



secshelf.models <- comp2021.points |>
  dplyr::select(cCOMP_2021, A_SECTOR, SHELF) |>
  distinct() |> 
  #slice(1:10) |> 
  mutate(n = 1:n(), N = n()) |>                                 ##### purely so it can count
  mutate(data=pmap(.l=list(cCOMP_2021, A_SECTOR, SHELF, n, N),   ##### map = will run a loop for each combination of variables map works on one column only - pmap works on multiple
                   .f=~{                                          ## .f = function to apply (in this case its an unnamed function that creates the dataframe to model)
                     ## cCOMP_2021 <- ..1
                     ## A_SECTOR <- ..2
                     ## SHELF <- ..3
                     n <- ..4                                    
                     N <- ..5
                     print(paste("", n, "/", N))     #### poor person's progress counter using n from line 17
                     comp2021.points |>
                       filter(cCOMP_2021 == ..1,
                              A_SECTOR==..2,
                              SHELF==..3) |> 
                       mutate(A_SECTOR=factor(A_SECTOR,levels=c('CL','PC','CG','CA','IN','TO','WH','PO','SW','CB')),
                              ReefSite = factor(paste(Reef_unique, SITE_NO)),
                              ReefSiteTransect = factor(paste(Reef_unique, SITE_NO, TRANSECT_NO)),
                              secshelfhalfdecade=factor(paste(A_SECTOR,SHELF,half_decade))) |> 
                       droplevels()
                   }))

secshelf.models <- secshelf.models |> as_tibble()

secshelf.models[2, "data"][[1]][[1]] |> as.data.frame() |> head()   #####looking at df created give me row 2 in 'data'



secshelf.models <- secshelf.models |> 
  mutate(newdata.yr=purrr::map(.x=data,                #using map to create new column called newdata.yr prediction data for INLA (only has one input - column called 'data')
                               .f=~{
                                 .x %>% 
                                   dplyr::select(A_SECTOR, SHELF, half_decade) %>%
                                   distinct() %>%
                                   mutate(n.points = NA, 
                                          Reef_unique = NA, 
                                          ReefSite = NA, 
                                          ReefSiteTransect = NA,
                                          secshelfhalfdecade=factor(paste(A_SECTOR,SHELF,half_decade)),
                                          total.points=1)
                               }))

secshelf.models <- secshelf.models |>                             ## row indices for the new data - only want predictions for new data rows, not for the observed data - so its tacked on the end of the raw dataframe
  mutate(newdata.i.yr = map2(.x=newdata.yr,                        # uses map2 bevcause there's 2 inputs .x and .y
                             .y=data,
                             .f=~1:nrow(.x) + (nrow(.y))))
## secshelf.models[2, "newdata.i.yr"][[1]][[1]]

secshelf.models <- secshelf.models |>                                 ##### combining the input data plus newdata into a single df for input into INLA
  mutate(dat.yr = map2(.x=data,
                       .y=newdata.yr,
                       .f=~.x %>% bind_rows(.y)))

secshelf.models[1, "dat.yr"][[1]][[1]] |> as.data.frame() |> head()   #### having a look at the df  [[1]] is the first item in the list dat.yr
                                                                       #### could also use secshelf.models$dat.yr[[1]] |> summary() |> head()




##### running INLA


secshelf.models<- secshelf.models |> 
  #slice(1) |>                                      #### first row for test comment out when happy it works
  mutate(mod.inla = pmap(.l=list(dat.yr,n, N),                       ##### poor colleagues counter
    .f =  ~ {
      print(paste("", ..2, "/", ..3))
      mod_name <- paste0('models/mod_',..1$cCOMP_2021[1],'_',..1$A_SECTOR[1],'_',..1$SHELF[1],'.rda')
      print(mod_name)
      if (length(unique(..1$secshelfdecade)) == 1) {
        print("Only a single sector/shelf/decade present")
        mod <- 
          inla(n.points~1 + #secshelfdecade+#A_SECTOR*SHELF*Decade +
            ## offset(log(total.points))+
            f(Reef_unique, model = 'iid') +
              f(ReefSite, model = 'iid') +
              f(ReefSiteTransect, model = 'iid') +
              f(REPORT_YEAR,model='iid'),
            data=..1,     #.x,
            Ntrials = (dat.yr$total.points),
            family="binomial",
            control.compute = list(config = TRUE),
            control.predictor = list(link = 1))
        ## inla(n.points~1+#A_SECTOR*SHELF*Decade +
        ##        offset(log(total.points))+
        ##        f(REEF, model = 'iid') +
        ##        f(ReefSite, model = 'iid') +
        ##        f(ReefSiteTransect, model = 'iid') +
        ##        f(REPORT_YEAR,model='iid'),
        ##      data=.x,
        ##      #Ntrials = (dat.yr$total.points),
        ##      family="poisson",
        ##      control.predictor = list(link = 1))
      } else {                                            ###### give me a list of secshelfdec combinations that are all zeroes
        dex <- ..1 |> group_by(secshelfhalfdecade)|>
          summarise(sum=sum(n.points,na.rm=TRUE),
            length(n.points)) |> 
          filter(sum==0) |> pull(secshelfhalfdecade)
        print(dex)
        if (length(dex) == 0) {                                            ### if no zeros run normal model (default priors)
          print("No zero-only combindations present - regular model")
          mod <- 
            inla(n.points~-1 + secshelfhalfdecade+#A_SECTOR*SHELF*Decade +
                   ## offset(log(total.points))+
                   f(Reef_unique, model = 'iid') +
                   f(ReefSite, model = 'iid') +
                   f(ReefSiteTransect, model = 'iid') +
                   f(REPORT_YEAR,model='iid'),
              data=..1,#.x,
              Ntrials = (.x$total.points),
              family="binomial",
              control.compute = list(config = TRUE),
              control.predictor = list(link = 1))
        } else {                                                                      ## if there are some zero combinations run this model - specified strong priors
          print("Zero-only combinations present - informative priors model used")
          mod <- 
            inla(n.points~secshelfhalfdecade+#A_SECTOR*SHELF*Decade +
                   ## offset(log(total.points))+
                   f(Reef_unique, model = 'iid') +
                   f(ReefSite, model = 'iid') +
                   f(ReefSiteTransect, model = 'iid') +
                   f(REPORT_YEAR,model='iid'),
              data=..1,#.x,
              Ntrials = (.x$total.points),
              family="binomial",
              control.predictor = list(link = 1))
          prior_terms <- paste0('secshelfhalfdecade',dex) 
          prior_mean <- lapply(prior_terms, function(x) log(0.001))
          prior_mean <- append(prior_mean, 0)
          names(prior_mean) <- c(prior_terms, "default")
          prior_prec <- lapply(prior_terms, function(x) 1)
          prior_prec <- append(prior_prec, 0.000001)
          names(prior_prec) <- c(prior_terms, "default")
          mod<-inla(n.points~-1+secshelfhalfdecade+#A_SECTOR*SHELF*Decade +
                      f(Reef_unique, model = 'iid') +
                      f(ReefSite, model = 'iid') +
                      f(ReefSiteTransect, model = 'iid') +
                      f(REPORT_YEAR,model='iid'),
            data=..1,#.x,
            Ntrials = (.x$total.points),
            family="binomial",
            control.predictor = list(link = 1),
            control.compute = list(config = TRUE),
            control.fixed = list(mean = prior_mean,
              prec = prior_prec))
        }
        saveRDS(mod,file=mod_name)
        return(mod_name)
      }
    }
  ))

saveRDS(secshelf.models, "secshelf_models_all.rds")

secshelf.models <- readRDS("secshelf_models_all.rds")

str(secshelf.models)

###### putting predictions onto newdata

secshelf.models <- secshelf.models |>
 #test <- test |>  
  mutate(newdata1.yr = pmap(.l=list(newdata.yr,                 ### newdata1.yr is the model summaries (ie mean, median etc)  - these should be the same in theory to the estimates from code block 174-190
    mod.inla, newdata.i.yr),                                    ### don't use these predictions!!!!!
    .f =  ~ {
      mod <- readRDS(..2)
      ..1 %>% bind_cols(mod$summary.fitted.values[..3,]) %>% 
      mutate(A_SECTOR=factor(A_SECTOR,levels=c('CG','PC','CL','CA','IN','TO','WH','PO','SW','CB'))) 
      }
  ))



########  extracting full posteriors
## ignore the warnings about seed!=0

secshelf.models <- secshelf.models |>   
# test <- test |> 
  mutate(posteriors = pmap(.l=list(newdata.yr,
    mod.inla, newdata.i.yr),
    .f =  ~ {
      newdata_yr <- ..1
      mod <- readRDS(..2)
      newdata_i_yr <- ..3
      draws <- inla.posterior.sample(n = 1000, mod, seed = 123)
      cellmeans <- sapply(draws, function(x) x[[2]][newdata_i_yr])    #### rearranging posterior draws into a meaningful format

      ## back-transform to response scale
      posteriors <- newdata_yr |>
        cbind(plogis(cellmeans)) |> 
        pivot_longer(cols = matches("[0-9]"), names_to = ".draw")
      posteriors
    }
    ))

secshelf.models$posteriors[[5]]  ##### looking at the fifth set of posteriors (think its ACTO IN SECSHELF number 5)
#test$posteriors[[1]]
saveRDS(secshelf.models, "secshelf_models_all.rds")

secshelf.models <- readRDS("secshelf_models_all.rds")

########summarise posteriors

secshelf.models <- secshelf.models |>  
#  test <- test |>
  mutate(summary.dec = purrr::map(.x=posteriors,
                                  .f=~.x |> 
                                    dplyr::select(-n.points,-Reef_unique,-ReefSite,-ReefSiteTransect,-secshelfhalfdecade,-total.points) |> 
                                    group_by(A_SECTOR,SHELF,half_decade) |> 
                                    tidybayes::summarise_draws(median,mean,HDInterval::hdi))) 


secshelf.models[1,'summary.dec'][[1]][[1]]

#test$summary.dec[[1]]

#### make plot


secshelf.models <- secshelf.models |>
#  test <- test |> 
  mutate(decadal.plots = purrr::map2(.x=summary.dec,
                                     .y=cCOMP_2021,
                                  .f=~.x |> 
                                    ggplot(aes(y = mean*100, x = half_decade)) +
                                    geom_bar(stat='identity',position=position_dodge(width=1),show.legend = TRUE)+
                                    geom_errorbar(aes(ymin = lower*100, ymax = upper*100),width=0,
                                                  position=position_dodge(width=1),linewidth=0.01) +
                                    scale_y_continuous('Hard coral cover (mean ± 95% CI)')+
                                    scale_x_discrete('Decade')+
                                    facet_grid(~A_SECTOR ~ SHELF,scales='free_y')+
                                    ggtitle(.y)+
                                    theme_classic()+
                                    theme(axis.title = element_text(size=10,face='bold'),
                                          axis.text.x = element_text(),
                                          axis.title.x = element_blank(),
                                          legend.position = 'bottom',
                                          legend.title = element_blank(),
                                          legend.text = element_text(size=5.5),
                                          legend.key.size = unit(0.25,"cm"))+
                                    guides(fill = guide_legend(ncol = 8))
    )
  )
                                  
secshelf.models[1,'decadal.plots'][[1]][[1]]
#test$decadal.plots[[1]]

library(patchwork)

wrap_plots(secshelf.models$decadal.plots) ##### does all plots


wrap_plots(secshelf.models[1,'decadal.plots'][[1]][[1]],    ###### this gives comp-codes 1 and 3
           secshelf.models[3,'decadal.plots'][[1]][[1]])


wrap_plots(secshelf.models$decadal.plots[1:3]) ##gives 3 plots for first 3 comp codes


###faceted plots

secshelf.models.plots <- secshelf.models |>   
#  test <- test |> 
  ungroup() |> 
  dplyr::select(cCOMP_2021,summary.dec) |> 
  unnest('summary.dec') |> 
  group_by(cCOMP_2021) |>
  nest() |> 
  mutate(decadal.plots.facet = purrr::map2(.x=data,
                                           .y=cCOMP_2021,
                                           .f=~.x |>
                                             mutate(A_SECTOR=factor(A_SECTOR,levels=c('CG','PC','CL','CA','IN','TO','WH','PO','SW','CB')),
                                                    SHELF=factor(SHELF,levels=c('I','M','O'))) |> 
                                             ggplot(aes(y = mean*100, x = half_decade)) +
                                             geom_bar(stat='identity',position=position_dodge(width=1),show.legend = TRUE)+
                                             geom_errorbar(aes(ymin = lower*100, ymax = upper*100),width=0,
                                                           position=position_dodge(width=1),linewidth=0.01) +
                                             scale_y_continuous('Hard coral cover (mean ± 95% CI)')+
                                             scale_x_discrete('Half_decade')+
                                             facet_grid(~A_SECTOR ~ SHELF,scales='free_y')+
                                             ggtitle(.y)+
                                             theme_classic()+
                                             theme(axis.title = element_text(size=10,face='bold'),
                                                   axis.text.x = element_text(),
                                                   axis.title.x = element_blank(),
                                                   legend.position = 'bottom',
                                                   legend.title = element_blank(),
                                                   legend.text = element_text(size=5.5),
                                                   legend.key.size = unit(0.25,"cm"))+
                                             guides(fill = guide_legend(ncol = 8))
                                           
      )
  )

secshelf.models.plots[1,'decadal.plots.facet']

#test$decadal.plots.facet[[1]]

levels(secshelf.models.plots$cCOMP_2021)

wrap_plots(secshelf.models.plots$decadal.plots.facet[31])





###### taking full posteriors for the decadal comparisons and doing the comparison for full posteriors (ie 1000 draws)
## pairwise comparisons

secshelf.models <- secshelf.models |>   
  mutate(comp_posteriors = purrr::map(.x = posteriors,
    .f =  ~ {
      posteriors <- .x
      ## tukey matrix to compare every decade to every other decade
      decades <- levels(posteriors$half_decade)
      xmat <- emmeans:::tukey.emmc(decades)

      comp <-
        posteriors |>
        group_by(.draw) |>
        summarise(
          frac = exp(as.vector(as.vector(log(value)) %*% as.matrix(xmat))),
          perc = 100 * (frac - 1),
          value = as.vector(as.vector(value) %*% as.matrix(xmat)),
          Decade_comp = names(xmat)
        ) |>
        ungroup() |>
        group_by(.draw, Decade_comp) |> 
        pivot_longer(cols = c(frac, value, perc), names_to = "stat",
          values_to = "value") |> 
        ungroup()
      comp
    }
  )
  )

secshelf.models$comp_posteriors[[5]]

####### summary of posterior decadal comparison - ie whats median difference, CI and probabilites



secshelf.models <- secshelf.models |>   
  mutate(comp_sum = purrr::map(.x = comp_posteriors,
    .f =  ~ {
      comp_sum <- .x |>
        #mutate(value=value*-1) |> 
        group_by(Decade_comp, stat) |>
        tidybayes::summarise_draws(median,mean,
          HDInterval::hdi,
          Pl = ~ mean(.x < 0),
          Pg = ~ mean(.x > 0)
        ) |> 
          mutate(heat_map_cat = ifelse(Pl>=0.95,1,
                           ifelse(Pl>=0.9,2,
                              ifelse(Pg>=0.95,5,
                                 ifelse(Pg>=0.9,4,3))))
        )
      comp_sum
    }
  ))

secshelf.models$comp_sum[[5]]

test <- secshelf.models |> 
  mutate(n=1:n()) |> 
  dplyr::select(cCOMP_2021,A_SECTOR,SHELF,n)#,comp_sum) |> 
   filter(A_SECTOR=='WH',SHELF='O')
   

   
##############################################################################
##############################################################################
##############################################################################
   
# secshelf.models save_load -----------------------------------------------

   
   save(secshelf.models,file='secshelf.models.RData')
   load(file='secshelf.models.RData')
   

  ####

# extract dataframes from loop ---------------------------------------------

   
   
   comp_2021_decadal_comparison_summary <- secshelf.models |> 
     dplyr::select(cCOMP_2021,A_SECTOR,SHELF,comp_sum) |> 
     unnest('comp_sum') 
   
   head(comp_2021_decadal_comparison_summary)
   save(comp_2021_decadal_comparison_summary,file='comp_2021_decadal_comparison_summary.RData')
   
   load(file='../comp_2021_decadal_comparison_summary.RData')
   
   
   comp_2021_decadal_comparison_summary <- secshelf.models |> 
     dplyr::select(cCOMP_2021,mod.inla) |> 
     unnest('mod.inla') 
   
####Decade comp_21 estimates
   
   load(file='secshelf.models_hc_sum.RData')
   head(secshelf.models_hc_sum)

   
   
####
   
# create master summary ---------------------------------------------
   
   
   head(comp_2021_decadal_comparison_summary)
   
   heat_map_cat_summary <- comp_2021_decadal_comparison_summary |> 
     dplyr::select(cCOMP_2021,A_SECTOR,SHELF,Decade_comp,stat,heat_map_cat) |> 
     filter(stat=='perc') |> 
     pivot_wider(names_from = cCOMP_2021,values_from = heat_map_cat)
    
   
   head(heat_map_cat_summary)
   
   heat_map_cat_summary_9020 <- heat_map_cat_summary |> 
     filter(Decade_comp=='1990s - 2020s') |> 
     dplyr::mutate(likely_decline = rowSums(across(ACBX:S_POR_RUS, \(x) x == 5)) )
   
   
   
    write.csv(heat_map_cat_summary_9020,file='heat_map_cat_summary_9020.csv')
   
 save(heat_map_cat_summary_9020,file='heat_map_cat_summary_9020.RData')  
   
##############################################################################   
##### heatmap of results ------------------------------------------------------



group.lookup <- comp2021_Oct2025_df |> 
  dplyr::select(GROUP_CODE,cCOMP_2021,COMP_2021_DESCRIPTION) |> 
  distinct()

heatmap.dat <- secshelf.models |> 
  dplyr::select(cCOMP_2021,A_SECTOR,SHELF,comp_sum) |> 
  unnest('comp_sum') |> 
  left_join(group.lookup,by=c('cCOMP_2021')) |> 
  mutate(cCOMP_2021=factor(cCOMP_2021, levels=c('ACTO','ACBX','ACD','ACSE','G_ECH_CB','G_HYD_CB','G_ISO_B','G_POC','G_POR_B',
                                                'G_SER','G_STY','F_AGA_CF','F_FUN_CECF','G_ECH_OTH','G_MER','G_MON','G_MYC',
                                                'G_OXY_ECL_ECY','G_PAC','G_PEC','G_TUR_DUN','G_POR_M','F_AGA_CEMS','F_EUPH_PLU',
                                                'F_MER_CEMS','F_MER_PLE_PLOC','F_SID_COS_PSA','G_ACA_MIC_HOM','G_AST','G_DIP',
                                                'G_GAL','G_GON_ALV_BER','G_HYD_OTH','G_ISO_CSE','G_LEP','G_LOB_AUST','G_MOS',
                                                'G_POR_CECS','S_POR_RUS','F_FUN_CMR','G_TUB_HET'))) |> 
  filter(stat=='perc',
         GROUP_CODE=='HC') |> 
  group_by(Decade_comp) |> 
  nest()


heatmap.dat <- heatmap.dat |> 
  mutate(heatmap.fig=purrr::map2(.x=data,
                                 .y=Decade_comp,
                                .f=~{
                                  .x |> 
                                    ggplot(aes(x=cCOMP_2021,y=SHELF,fill=factor(heat_map_cat)))+
                                    geom_tile(alpha=0.5)+
                                    scale_fill_manual(labels=c('likely increase','possible increase','no change',
                                                               'possible decrease','likely decrease'),
                                                      values=c('blue','lightblue','white','orange','red'))+
                                    facet_grid(A_SECTOR~.,switch='y',space='free_y')+
                                    coord_cartesian(expand=0)+
                                    ggtitle(.y)+
                                    theme_bw()+
                                    theme(axis.text.x=element_text(size=8,angle=45,hjust=1),#vjust=0.5),
                                          strip.placement.y = 'outside',
                                          strip.background.y = element_blank(),
                                          panel.spacing.y = unit(0,'cm'),
                                          axis.title = element_blank(),
                                          legend.title = element_blank())
                                }))

#heatmap.dat[3,'data'][[1]][[1]]
comp21_decadal_comparison_plot <- heatmap.dat[23,'heatmap.fig'][[1]][[1]] ###first number is the decadal comparison
comp21_decadal_comparison_plot

ggsave(comp21_decadal_comparison_plot,file='comp21_decadal_comparison_plot_late_90s_to_late_10s.png',height=9,width=12)

heatmap.dat |> pull(Decade_comp)

################################################################################
####### model summary stats sec shelf

(file='comp_2021_decadal_comparison_summary.RData')

head(comp_2021_decadal_comparison_summary)


################################################################################
####### pulls the change data 


secshelf.models |> filter(cCOMP_2021=='ACTO',A_SECTOR=='CL',SHELF=='O') |> 
  pull(comp_sum)

 


## express parameter estimates as relative proportion of total hard coral cover


secshelf.models_hc <-
  secshelf.models |>
  mutate(GROUP_CODE = purrr::map(.x = data,
    .f =  ~ unique(.x$GROUP_CODE))) |>
  unnest(GROUP_CODE) ##|>


secshelf.models_hc_posterior <-
  secshelf.models_hc |>
  ## filter(A_SECTOR == "CA", SHELF == "O") |>
  dplyr::select(cCOMP_2021, GROUP_CODE, posteriors) |> 
  unnest(posteriors) |>
  dplyr::select(-n.points, -REEF, -ReefSite, -ReefSiteTransect, -total.points) |> 
  group_by(.draw, Decade) |>
  ## filter(.draw == 1, Decade == "2000s") |>
  mutate(total_hc = sum(value[GROUP_CODE == "HC"]),
    rel_to_hc = value / total_hc)

secshelf.models_hc_posterior <- secshelf.models_hc_posterior |> 
  filter(GROUP_CODE=='HC')

save(secshelf.models_hc_posterior,file='secshelf.models_hc_posterior.RData')
   
secshelf.models_hc_sum <-
  secshelf.models_hc_posterior |>
  ungroup(.draw) |>
  dplyr::select(-secshelfdecade, -total_hc) |>
  group_by(Decade, cCOMP_2021, GROUP_CODE, A_SECTOR, SHELF) |>
  pivot_longer(cols = c(value, rel_to_hc), names_to = "stat",
    values_to = "value") |> 
  group_by(stat, .add = TRUE) |> 
  tidybayes::summarise_draws(median,
    HDInterval::hdi
  ) 

secshelf.models_hc_sum <- secshelf.models_hc_sum |> 
  filter(GROUP_CODE=='HC')
  
save(secshelf.models_hc_sum,file='secshelf.models_hc_sum.RData')
load(file='secshelf.models_hc_sum.RData')

#############################

# ### checking WHO ACTO ---------------------------------------------------

setwd("C:/Users/memslie/OneDrive - Australian Institute of Marine Science/Desktop/working folder/change in coral assemblages/Update with 2025 data_Oct 2025/comp21 half decadal models/models")

who.mod <- readRDS(file='mod_ACTO_WH_O.rda')
summary(who.mod)

#plogis(-3.732)

who <- comp2021_Oct2025_df_barplot |> 
     filter(A_SECTOR=='WH' & SHELF=='O') |>
     filter(COMP_2021=='ACTO') |> 
     group_by(half_decade) |> 
     summarise(cover=mean(cover))


comp_summ <- long_series |> 
  pivot_longer(cols=ACBX:S_POR_RUS,names_to = 'COMP_2021',values_to = 'cover') |> 
  group_by(SECSHELF,half_decade,COMP_2021) |> 
  summarise(mean=mean(cover),
            sd=sd(cover),
            n=n(),
            se=sd/sqrt(n)) |> 
  mutate(half_decade=factor(half_decade,levels=c('early_90s','late_90s','early_00s','late_00s','early_10s','late_10s','early_20s')))

check.dat <- comp_summ |> 
  filter(SECSHELF=='WH O',half_decade %in% c('early_90s','early_20s')) |>    #,COMP_2021 %in% c('ACBX','ACTO','G_POR_B','G_ECH_OTH')
  arrange(SECSHELF,COMP_2021,half_decade) |> 
  as.data.frame()

ggplot(check.dat,aes(x=COMP_2021,y=mean,color=half_decade))+
  geom_point(stat='identity',position=position_dodge(width=0.5))+
  geom_errorbar(stat='identity',aes(ymin=mean-2*se,ymax=mean+2*se),width=0,position=position_dodge(width=0.5))
