setwd("C:/Users/memslie/OneDrive - Australian Institute of Marine Science/Desktop/working folder/change in coral assemblages/text/for submission/code and data for repo/Fig 4")

library(sf)
library(purrr)
library(tidyverse)


###########################################################################
#### calculate hull overlap in one hit -----------------------------------

load(file='site.scores.combined_full.RData')


gbr.sf.convert<-st_as_sf(site.scores.combined_full,
                         coords=c("CAP1","CAP2"),
                         dim="XY")


gbr.sf.poly <- gbr.sf.convert  |>  
  group_by(P_CODE2,secshelfdec_half)  |>  
  summarise(
    SECSHELF = dplyr::first(SECSHELF),  # retain SECSHELF in the attribute table
    .groups  = "drop"
  )  |>  
  st_convex_hull()


gbr.sf.convex<-gbr.sf.poly  |>  
  st_convex_hull() 

# ensure sf structure
gbr.sf.convex <- st_as_sf(gbr.sf.convex)
st_crs(gbr.sf.convex) <- NA_crs_

# function to compute overlap between polygons
compute_overlap <- function(a_name, b_name, data) {
  a <- data %>% filter(secshelfdec_half == a_name)
  b <- data %>% filter(secshelfdec_half == b_name)
  inter <- tryCatch(st_intersection(a, b), error = function(e) NULL)
  
  if (is.null(inter) || nrow(inter) == 0) {
    overlap_prop <- 0
  } else {
    overlap_prop <- as.numeric(st_area(inter)) / as.numeric(st_area(a))
  }
  
  tibble(
    A = a_name,
    B = b_name,
    overlap_prop = overlap_prop
  )
}

#main: loop across SECSHELF groups
hull.overlap.all <- gbr.sf.convex %>%
  group_by(P_CODE2,SECSHELF) %>%
  group_map(~{
    levels <- .x$secshelfdec_half %>% as.character()
    combos <- expand_grid(A = levels, B = levels) %>% filter(A != B)
    map2_dfr(combos$A, combos$B, compute_overlap, data = .x) %>%
      mutate(P_CODE2 = .y$P_CODE2,
             SECSHELF = .y$SECSHELF)   # attach group name back here
  }) %>%
  bind_rows() %>%
  ungroup()



hull.overlap.all <- hull.overlap.all %>%
  mutate(
    A = sub(".* ", "", A),
    B = sub(".* ", "", B)
  )


sector_levels <- c("CG", "PC", "CL", "CA", "IN", "TO", "WH", "PO", "SW", "CB")

hull.overlap.all <- hull.overlap.all %>%
  # split SECSHELF into Sector and Shelf
  separate(SECSHELF, into = c("Sector", "Shelf"), sep = "\\s+", remove = FALSE) %>%
  # enforce desired order for Sector
  mutate(
    Sector = factor(Sector, levels = sector_levels),
    Shelf  = factor(Shelf, levels = c("I", "M", "O")), # optional but clean
    A = factor(A, levels = c("early_90s", "late_90s", "early_00s", 
                             "late_00s", "early_10s", "late_10s", "early_20s")),
    B = factor(B, levels = c("early_90s", "late_90s", "early_00s", 
                             "late_00s", "early_10s", "late_10s", "early_20s"))
  ) %>%
  arrange(P_CODE2,Sector, Shelf, A, B)



#### adding MMP points separately

half_decade_levels <- c(
  "early_90s",
  "late_90s",
  "early_00s",
  "late_00s",
  "early_10s",
  "late_10s",
  "early_20s"
)

############## 
## making sure different starting points get captured

start_map <- c(RM = "early_90s", IN = "early_00s")

# special-case starts for particular subregions
start_override <- tibble::tribble(
  ~Sector_raw, ~Shelf, ~start_hd_override,
  "PO",        "M",    "late_00s",
  "IN",        "M",    "late_00s"
)


hull_overlap_start <- hull.overlap.all %>%
  mutate(
    Sector_raw = as.character(Sector),
    Shelf      = as.character(Shelf),
    A = factor(A, levels = half_decade_levels),
    B = factor(B, levels = half_decade_levels)
  ) %>%
  left_join(start_override, by = c("Sector_raw", "Shelf")) %>%
  mutate(
    start_hd = dplyr::coalesce(
      start_hd_override,
      unname(start_map[as.character(P_CODE2)])
    ),
    start_hd  = factor(start_hd, levels = half_decade_levels),
    A_num     = as.integer(A),
    B_num     = as.integer(B),
    start_num = as.integer(start_hd)
  ) %>%
  filter(P_CODE2 %in% c("RM", "IN")) %>%
  filter(as.character(A) == as.character(start_hd)) %>%
  filter(B_num > A_num) %>%
  mutate(
    comparison = paste(A, B, sep = " - "),
    comparison = factor(
      comparison,
      levels = c(
        paste0("early_90s - ", half_decade_levels[2:7]),
        paste0("early_00s - ", half_decade_levels[4:7])
      )
    ),
    comparison_lab = factor(as.character(B), levels = half_decade_levels[-1]),
    Sector = factor(
      dplyr::recode(
        Sector_raw,
        "CL" = "Cooktown-Lizard",
        "CA" = "Cairns",
        "IN" = "Innisfail",
        "TO" = "Townsville",
        "WH" = "Whitsunday",
        "PO" = "Pompey",
        "SW" = "Swain",
        "CB" = "Capricorn-Bunker"
      ),
      levels = c(
        "Cooktown-Lizard",
        "Cairns",
        "Innisfail",
        "Townsville",
        "Whitsunday",
        "Pompey",
        "Swain",
        "Capricorn-Bunker"
      )
    ),
    Shelf = factor(Shelf, levels = c("I", "M", "O"))
  )

#### original shelf based plot

overlap.plot <- ggplot(hull_overlap_start,aes(x=B,y=overlap_prop*100,shape=P_CODE2))+
  geom_point(
    stat = "summary",
    fun  = "mean",
    aes(shape = P_CODE2),
    fill  = "black",
    colour = "black",
    size = 4,
    position = position_dodge(width = 0.5),
    show.legend = FALSE   # avoid duplicate project legend (we'll take it from jitter layer)
  ) +
  
  geom_errorbar(
    stat = "summary",
    fun.data = mean_se,
    width = 0,
    position = position_dodge(width = 0.5),
    show.legend = FALSE
  ) +
  
  geom_point(
    stat='identity',
    aes(fill=Sector,colour=Sector,shape=P_CODE2),
    show.legend = TRUE,
    position = position_jitter(width = 0.2, height = 0),
    #shape=21,
    size=2)+
  
  
  scale_fill_manual(values=c('#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A'))+
  scale_colour_manual(values=c('#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A'))+
  #scale_colour_brewer(palette='Set3')+
  scale_y_continuous('Hull overlap (%)')+
  scale_x_discrete('Comparison to start')+
  scale_shape_manual(
    name   = "Project",
    values = c(IN = 21, RM = 22),
    labels = c(IN = "MMP",
               RM = "LTMP")
  )+
  facet_wrap(~ Shelf,
             labeller=as_labeller(
               c('I'='Inner shelf',
                 'M'='Mid-shelf',
                 'O'='Outer shelf')
             ))+
  theme_classic()+
  theme(legend.position='right',
        panel.background=element_rect(color='black'),
        legend.key = element_blank(),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        strip.text=element_text(size=14,face='bold'),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        #axis.text.x = element_text(angle=45,hjust=1,size=12)) 
        axis.text.x = element_blank(),
        axis.title.x = element_blank())

overlap.plot +
  guides(
    fill = guide_legend(
      title = "Sector",
      override.aes = list(
        shape = 21,          # force circle
        #colour = "grey30",   # outline only
        alpha = 1
      )
    ),
    shape = guide_legend(
      title = "Project",
      override.aes = list(
        fill = "white",      # neutral fill
        colour = "grey30",
        alpha = 1
      )
    )
  )


################################################################################
####  calculate distance between centroids -------------------------------


# define chronological order
half_levels <- c("early_90s","late_90s","early_00s","late_00s",
                 "early_10s","late_10s","early_20s")



centroid.distances_full <- centroid.scores_half_decade_full %>%
  mutate(
    half_decade = factor(half_decade, levels = half_levels),
    SECSHELF = paste(A_SECTOR, SHELF)
  ) %>%
  group_by(P_CODE2,A_SECTOR, SHELF, SECSHELF) %>%
  summarise(
    distances = list({
      df <- cur_data()
      
      expand_grid(
        A = df$half_decade,
        B = df$half_decade
      ) %>%
        filter(A != B) %>%
        mutate(
          A_num = df$half_decade_num[match(A, df$half_decade)],
          B_num = df$half_decade_num[match(B, df$half_decade)],
          
          CAP1A = df$CAP1[match(A, df$half_decade)],
          CAP2A = df$CAP2[match(A, df$half_decade)],
          CAP1B = df$CAP1[match(B, df$half_decade)],
          CAP2B = df$CAP2[match(B, df$half_decade)],
          
          euclidean = sqrt((CAP1A - CAP1B)^2 + (CAP2A - CAP2B)^2)
        )
    })
  ) %>%
  unnest(distances) %>%
  ungroup()

centroid.distances_full <- centroid.distances_full |> 
  separate(SECSHELF, into = c("Sector", "Shelf"), sep = " ", remove = FALSE) %>%
  mutate(Sector=str_replace(Sector,'Capricorn-\nBunker','Capricorn-Bunker')) |> 
  # enforce desired order for Sector
  mutate(
    Sector = factor(Sector, levels = c('Cooktown-Lizard','Cairns','Innisfail','Townsville','Whitsunday','Pompey','Swain','Capricorn-Bunker')),
    Shelf  = factor(Shelf, levels = c("I", "M", "O")), # optional but clean
    A = factor(A, levels = c("early_90s", "late_90s", "early_00s", 
                             "late_00s", "early_10s", "late_10s", "early_20s")),
    B = factor(B, levels = c("early_90s", "late_90s", "early_00s", 
                             "late_00s", "early_10s", "late_10s", "early_20s"))
  ) %>%
  arrange(Sector, Shelf, A, B)

sector_levels <- c('Cooktown-Lizard','Cairns','Innisfail','Townsville','Whitsunday','Pompey','Swain','Capricorn-Bunker')





cent_dist_90s_full <- centroid.distances_full |> 
  mutate(Sector = factor(Sector, levels = c('Cooktown-Lizard','Cairns','Innisfail','Townsville','Whitsunday','Pompey','Swain','Capricorn-Bunker'))) |> 
  filter(A=="early_90s") |> 
  arrange(match(Sector, sector_levels), Shelf, B) |> 
  mutate(comparison=paste(A,B,sep=' - '),
         comparison=factor(comparison,levels=c('early_90s - late_90s','early_90s - early_00s','early_90s - late_00s',
                                               'early_90s - early_10s','early_90s - late_10s','early_90s - early_20s'),
                           # labels=c('early_90s -\n late_90s','early_90s -\n early_00s','early_90s -\n late_00s',
                           #          'early_90s -\n early_10s','early_90s -\n late_10s','early_90s -\n early_20s'),
                           labels=c('late_90s','early_00s','late_00s','early_10s','late_10s','early_20s')))


half_decade_levels <- c(
  "early_90s","late_90s","early_00s","late_00s",
  "early_10s","late_10s","early_20s"
)


################################
## Adding MMP data separate

start_map <- c(RM = "early_90s", IN = "early_00s")

cent_dist_start_full <- centroid.distances_full |>
  mutate(
    Sector = factor(Sector, levels = sector_levels),
    A = factor(A, levels = half_decade_levels),
    B = factor(B, levels = half_decade_levels),
    
    start_hd  = unname(start_map[as.character(P_CODE2)]),
    start_hd  = factor(start_hd, levels = half_decade_levels)
  ) |>
  # keep only RM and IN comparisons you care about
  filter(P_CODE2 %in% c("RM", "IN")) |>
  # keep comparisons where A equals the relevant start decade
  filter(as.character(A) == as.character(start_hd)) |>
  # drop self-comparisons (A == B) if present
  filter(A != B) |>
  # (optional) only compare forward in time
  filter(as.integer(B) > as.integer(A)) |>
  mutate(
    comparison = paste(A, B, sep = " - "),
    # make x-axis order chronological by destination decade
    comparison = factor(
      comparison,
      levels =
        c(paste0("early_90s - ", half_decade_levels[2:7]),
          paste0("early_00s - ", half_decade_levels[4:7]))
    ),
    # optional short label = destination decade only
    comparison_lab = factor(
      as.character(B),
      levels = half_decade_levels[2:7]
    )
  ) |>
  arrange(P_CODE2, match(Sector, sector_levels), Shelf, as.integer(B))


cent_dist_seq_full <- centroid.distances_full %>%
  mutate(
    Sector = factor(Sector, levels = sector_levels),
    A = factor(A, levels = half_decade_levels),
    B = factor(B, levels = half_decade_levels),
    A_num = as.integer(A),
    B_num = as.integer(B),
    start_hd = unname(start_map[as.character(P_CODE2)]),
    start_num = as.integer(factor(start_hd, levels = half_decade_levels))
  ) %>%
  filter(B_num == A_num + 1) %>%
  # keep only comparisons from start..end for each P_CODE2
  filter(is.na(start_num) | A_num >= start_num) %>%
  mutate(
    # build label
    comparison = paste(A, B, sep = " - "),
    # make comparison an ordered factor (chronological by A_num)
    comparison = factor(
      comparison,
      levels = centroid.distances_full %>%
        mutate(
          A = factor(A, levels = half_decade_levels),
          B = factor(B, levels = half_decade_levels),
          A_num = as.integer(A),
          B_num = as.integer(B)
        ) %>%
        filter(B_num == A_num + 1) %>%
        distinct(A_num, A, B) %>%
        arrange(A_num) %>%
        transmute(comp = paste(A, B, sep = " - ")) %>%
        pull(comp)
    ),
    # optional: "destination" labels, also ordered
    comparison_lab = factor(
      as.character(B),
      levels = half_decade_levels[-1]
    )
  ) %>%
  arrange(P_CODE2, Sector, Shelf, A_num)



cent_dist_seq_full


###############################################################################################
#### capture IN and PO sectors
start_map <- c(RM = "early_90s", IN = "early_00s")

start_override <- tibble::tribble(
  ~Sector_raw, ~Shelf, ~start_hd_override,
  "PO",        "M",    "early_00s"
)

## ---------------------------------------------------------
## 1. distances from start period to later periods
## ---------------------------------------------------------

cent_dist_start_full <- centroid.distances_full %>%
  mutate(
    Sector_raw = as.character(Sector),
    Shelf      = as.character(Shelf),
    SECSHELF   = paste(Sector_raw, Shelf),
    Sector     = factor(Sector, levels = sector_levels),
    A          = factor(A, levels = half_decade_levels),
    B          = factor(B, levels = half_decade_levels)
  ) %>%
  left_join(start_override, by = c("Sector_raw", "Shelf")) %>%
  group_by(SECSHELF, P_CODE2) %>%
  mutate(
    project_start = unname(start_map[as.character(P_CODE2)]),
    start_hd = case_when(
      !is.na(start_hd_override) & any(as.character(A) == start_hd_override) ~ start_hd_override,
      any(as.character(A) == project_start) ~ project_start,
      TRUE ~ as.character(A[which.min(as.integer(A))])
    ),
    start_hd = factor(start_hd, levels = half_decade_levels)
  ) %>%
  ungroup() %>%
  filter(P_CODE2 %in% c("RM", "IN")) %>%
  filter(as.character(A) == as.character(start_hd)) %>%
  filter(A != B) %>%
  filter(as.integer(B) > as.integer(A)) %>%
  mutate(
    comparison = paste(A, B, sep = " - "),
    comparison = factor(
      comparison,
      levels = c(
        paste0("early_90s - ", half_decade_levels[2:7]),
        paste0("early_00s - ", half_decade_levels[4:7])
      )
    ),
    comparison_lab = factor(
      as.character(B),
      levels = half_decade_levels[2:7]
    )
  ) %>%
  arrange(P_CODE2, match(as.character(Sector), sector_levels), Shelf, as.integer(B))

cent_dist_start_full



######   fig 4a update with MMP reefs separated

dist.plot <- ggplot(cent_dist_start_full,aes(x=B,y=euclidean,shape=P_CODE2))+
  geom_point(
    stat = "summary",
    fun  = "mean",
    aes(shape = P_CODE2),
    fill  = "black",
    colour = "black",
    size = 4,
    position = position_dodge(width = 0.5),
    show.legend = FALSE   # avoid duplicate project legend (we'll take it from jitter layer)
  ) +
  
  geom_errorbar(
    stat = "summary",
    fun.data = mean_se,
    width = 0,
    position = position_dodge(width = 0.5),
    show.legend = FALSE
  ) +
  
  geom_point(
    stat='identity',
    aes(fill=Sector,colour=Sector,shape=P_CODE2),
    show.legend = TRUE,
    position = position_jitter(width = 0.1, height = 0),
    #shape=21,
    size=2)+
  
  scale_fill_manual(values=c('#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A'))+
  scale_colour_manual(values=c('#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A'))+
  #scale_colour_brewer(palette='Set3')+
  scale_y_continuous('Euclidean distance')+
  scale_x_discrete('Comparison to start')+
  scale_shape_manual(
    name   = "Project",
    values = c(IN = 21, RM = 22),
    labels = c(IN = "MMP",
               RM = "LTMP")
  )+
  facet_wrap(~ Shelf,
             labeller=as_labeller(
               c('I'='Inner shelf',
                 'M'='Mid-shelf',
                 'O'='Outer shelf')
             ))+
  theme_classic()+
  theme(legend.position='right',
        panel.background=element_rect(color='black'),
        legend.key = element_blank(),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        strip.text=element_text(size=14,face='bold'),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(angle=45,hjust=1,size=12)) 

dist.plot + 
  guides(
    fill = guide_legend(
      title = "Sector",
      override.aes = list(
        shape = 21,          # force circle
        #colour = "grey30",   # outline only
        alpha = 1
      )
    ),
    shape = guide_legend(
      title = "Project",
      override.aes = list(
        fill = "white",      # neutral fill
        colour = "grey30",
        alpha = 1
      )
    )
  )

#### add hcc change vs dissim plots 

load(file='subregion_recovery_centroid_dist.RData')

rel_recovery_diss_plot <- ggplot(subregion_recovery_centroid_dist,
                                 aes(x=pct_recovery_median ,y=diss_before_recovery))+
  geom_point()+
  geom_smooth(method='gam')+
  geom_vline(xintercept = 100,linetype='dashed')+
  scale_y_continuous("Community change",limits=c(0,1))+
  scale_x_continuous('Percent recovery of hard coral cover',limits = c(0,200))+
  #facet_wrap(~SHELF)+
  theme_classic()+
  theme(axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(hjust=0.5,size=12)) 

rel_recovery_diss_plot

# relative decline v dissim
rel_decline_diss_plot <- ggplot(subregion_recovery_centroid_dist,
                                aes(x=pct_decline_median,y=diss_before_after))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_continuous("Community change")+
  scale_x_continuous('Post disturbance decline in hard coral cover')+
  theme_classic()+
  theme(axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(hjust=0.5,size=12)) 
rel_decline_diss_plot




### combine plots

layout.again <- "AAAA
                 AAAA
                 BBBB
                 BBBB
                 CCDD
                 CCDD"

comb.dist.hull.plot <- overlap.plot+dist.plot+rel_decline_diss_plot+rel_recovery_diss_plot+
  plot_layout(guides = 'collect',design=layout.again) +
  plot_annotation(tag_levels = 'a') &
  theme(
    plot.tag = element_text(
      face = "bold",
      size = 14,
      hjust = 0,
      vjust = 1
    ),
    plot.tag.position = c(0, 1)
  )
comb.dist.hull.plot


ggsave(comb.dist.hull.plot,file='Fig_4_comb.dist.hull.plot.png',height=9,width=12)
