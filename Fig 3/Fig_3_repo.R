setwd("C:/Users/memslie/OneDrive - Australian Institute of Marine Science/Desktop/working folder/change in coral assemblages/text/for submission/code and data for repo/Fig 3")


library(tidyverse)
library(ggbreak)
library(vegan)
library(patchwork)
library(INLA)
library(svglite)
library(posterior)

load(file='long_series_full.RData')

response_full <- long_series_full[,11:53]

response2_full <- response_full[rowSums(response_full) > 0, ]

data2_full <- long_series_full[rowSums(response_full) > 0, ] |> 
  ungroup()

data2_full <- data2_full %>%
  mutate(siteID = rownames(response2_full))

sector_lookup <- c(
  #"Cape Grenville"      = "CG",
  #"Princess Charlotte" = "PC",
  "Cooktown Lizard"    = "CL",
  "Cairns"             = "CA",
  "Innisfail"          = "IN",
  "Townsville"         = "TO",
  "Whitsundays"        = "WH",
  "Pompeys"       = "PO",
  "Swains"             = "SW",
  "Capricorn Bunkers"  = "CB"
)

# adjusting palette to match Fig 1

locs <- tribble(
  ~label, ~color, ~long, ~lat,
  # "Cape Grenville", "#A6CEE3", 144.5, -11.00,
  #"Princess Charlotte Bay", "#1F78B4", 144.2, -13.3,
  "Cooktown-Lizard","#B2DF8A",146,-14.7,
  "Cairns","#33A02C",146.2,-16,
  "Innisfail","#FB9A99",147,-17.3,
  "Townsville","#E31A1C",148,-18.2,
  "Whitsunday","#FDBF6F",150,-19.3,
  "Pompey","#FF7F00",151.5,-20,
  "Swain","#CAB2D6",153.5,-21.5,
  "Capricorn-\nBunker","#6A3D9A",153.5,-23.5
)


sector_cols <- setNames(locs$color, locs$label)
sector_cols


sector_map <- c(
  # CG = "Cape Grenville",
  # PC = "Princess Charlotte Bay",
  CL = "Cooktown-Lizard",
  CA = "Cairns",
  IN = "Innisfail",
  TO = "Townsville",
  WH = "Whitsunday",
  PO = "Pompey",
  SW = "Swain",
  CB = "Capricorn-\nBunker"
)

##############################################################################
## capscale

###########
### hellinger + eucliadean

benthic.capscale_full <- capscale(
  decostand(response2_full, method = "hellinger") ~                               
    P_CODE2+A_SECTOR + SHELF + Reef_unique + cREPORT_YEAR + half_decade,
  distance = "bray",#"euclidean", 
  data = data2_full
)


site.scores_full <- as.data.frame(scores(benthic.capscale_full, display = "sites")) %>%
  rownames_to_column(var = "siteID")

#Combine CAP scores with metadata

site.scores.combined_full <- data2_full %>%
  #dplyr::select(-P_CODE) |> 
  left_join(site.scores_full, by = "siteID") %>%
  mutate(A_SECTOR = dplyr::recode(as.character(A_SECTOR),
                                  CL = "Cooktown-Lizard",
                                  CA = "Cairns",
                                  IN = "Innisfail",
                                  TO = "Townsville",
                                  WH = "Whitsunday",
                                  PO = "Pompey",
                                  SW = "Swain",
                                  CB = "Capricorn-\nBunker"
  ),
  A_SECTOR = factor(A_SECTOR),
  subregion = factor(
    paste(P_CODE2,A_SECTOR, SHELF),
    levels = c(
      "RM Cooktown-Lizard I","RM Cooktown-Lizard M","RM Cooktown-Lizard O",
      "RM Cairns I","IN Cairns I","RM Cairns M","RM Cairns O",
      "IN Innisfail I","RM Innisfail M",
      "RM Townsville I","IN Townsville I","RM Townsville M","RM Townsville O",
      "RM Whitsunday I","IN Whitsunday I","RM Whitsunday M","RM Whitsunday O",
      "RM Pompey M",
      "RM Swain M","RM Swain O",
      "RM Capricorn-\nBunker O", "IN Capricorn-\nBunker O"
    )
  ),
  secshelfdec_half=factor(secshelfdec_half),
  half_decade=factor(half_decade,
                     levels=c(
                       "early_90s","late_90s","early_00s","late_00s",
                       "early_10s","late_10s","early_20s"
                     )))|> 
  as.data.frame()

site.scores.combined_full 

save(site.scores.combined_full,file='site.scores.combined_full.RData')

####

centroid.scores_half_decade_full <- site.scores.combined_full %>%
  group_by(P_CODE2,A_SECTOR, SHELF, half_decade) %>%                   ### had included P_CODE here to split off reefs that don't start in 90s but causes problems down code
  dplyr::summarise(CAP1 = mean(CAP1), CAP2 = mean(CAP2)) %>%
  ungroup |> 
  mutate(A_SECTOR = dplyr::recode(as.character(A_SECTOR),
                                  CL = "Cooktown-Lizard",
                                  CA = "Cairns",
                                  IN = "Innisfail",
                                  TO = "Townsville",
                                  WH = "Whitsunday",
                                  PO = "Pompey",
                                  SW = "Swain",
                                  CB = "Capricorn-\nBunker"
  ),
  A_SECTOR = factor(A_SECTOR, levels = names(sector_cols)),
  Label = factor(paste(P_CODE2,A_SECTOR, SHELF, half_decade)),
  subregion = factor(
    paste(P_CODE2,A_SECTOR, SHELF),
    levels = c(
      "RM Cooktown-Lizard I","RM Cooktown-Lizard M","RM Cooktown-Lizard O",
      "RM Cairns I","IN Cairns I","RM Cairns M","RM Cairns O",
      "IN Innisfail I","RM Innisfail M",
      "RM Townsville I","IN Townsville I","RM Townsville M","RM Townsville O",
      "RM Whitsunday I","IN Whitsunday I","RM Whitsunday M","RM Whitsunday O",
      "RM Pompey M",
      "RM Swain M","RM Swain O",
      "RM Capricorn-\nBunker O", "IN Capricorn-\nBunker O"
    )
  ),
  half_decade = factor(
    half_decade,
    levels = c(
      "early_90s","late_90s","early_00s","late_00s",
      "early_10s","late_10s","early_20s"
    )
  ),
  half_decade_num = case_when(
    half_decade == "early_90s" ~ 1,
    half_decade == "late_90s"  ~ 2,
    half_decade == "early_00s" ~ 3,
    half_decade == "late_00s"  ~ 4,
    half_decade == "early_10s" ~ 5,
    half_decade == "late_10s"  ~ 6,
    half_decade == "early_20s" ~ 7,
    TRUE ~ NA_real_
  )
  ) %>%
  arrange(A_SECTOR, SHELF, half_decade)

centroid.scores_half_decade_full


########################

# -----------------------------
# Settings
# -----------------------------

hd_levels <- c(
  "early_90s","late_90s",
  "early_00s","late_00s",
  "early_10s","late_10s",
  "early_20s"
)

end_hd <- "early_20s"

# Start decade by P_CODE2
start_map <- c(
  RM = "early_90s",
  IN = "early_00s"
)

# -----------------------------
# Ensure ordered factor
# -----------------------------

site.scores.combined_full <- site.scores.combined_full %>%
  mutate(
    half_decade = factor(as.character(half_decade),
                         levels = hd_levels)
  )

# -----------------------------
# Centroids
# -----------------------------

centroids_pc <- site.scores.combined_full %>%
  group_by(P_CODE2, A_SECTOR, SHELF, half_decade) %>%
  summarise(
    CAP1 = mean(CAP1, na.rm = TRUE),
    CAP2 = mean(CAP2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # numeric half-decade label
    hd_num = as.integer(half_decade),
    
    # start decade rule
    start_hd = unname(start_map[as.character(P_CODE2)]),
    start_hd = ifelse(is.na(start_hd), "early_90s", start_hd),
    
    is_start = as.character(half_decade) == start_hd,
    is_end   = as.character(half_decade) == end_hd,
    is_full  = is_start | is_end,
    
    alpha_pt = ifelse(is_full, 1, 0.15),
    text_col = ifelse(A_SECTOR == "CB", "white", "black")
  )

# -----------------------------
# Arrows from start -> ... -> end
# -----------------------------

arrows_pc <- centroids_pc %>%
  group_by(P_CODE2, A_SECTOR, SHELF) %>%
  arrange(hd_num) %>%
  mutate(
    start_ord = as.integer(factor(start_hd, levels = hd_levels)),
    end_ord   = as.integer(factor(end_hd, levels = hd_levels))
  ) %>%
  filter(hd_num >= start_ord,
         hd_num <= end_ord) %>%
  mutate(
    xend = lead(CAP1),
    yend = lead(CAP2)
  ) %>%
  ungroup() %>%
  filter(!is.na(xend))

# -----------------------------
# Plot
# -----------------------------

g_start_end <- ggplot(site.scores.combined_full,
                      aes(x = CAP1, y = CAP2)) +
  
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  # background site points
  #geom_point(colour = "grey70",
  # size = 1.6,
  # alpha = 0.12,
  # show.legend = FALSE) +
  # 
  # arrows (sector coloured)
  geom_segment(
    data = arrows_pc,
    aes(x = CAP1, y = CAP2,
        xend = xend, yend = yend,
        colour = A_SECTOR,
        group = interaction(P_CODE2, A_SECTOR, SHELF)),
    linewidth = 0.9,
    arrow = arrow(type = "closed",
                  length = unit(2, "mm")),
    show.legend = FALSE
  ) +
  
  # centroids (alpha varies)
  geom_point(
    data = centroids_pc,
    aes(fill = A_SECTOR,
        
        #alpha = alpha_pt
        shape=P_CODE2),
    colour = 'black',
    size = 5,
    stroke = 0.8
  ) +
  
  # numeric half-decade labels on centroids
  # Non-CB labels (black)
  geom_text(
    data = centroids_pc %>% filter(A_SECTOR != "Capricorn-\nBunker"),
    aes(label = hd_num),
    colour = "black",
    fontface = "bold",
    size = 4,
    show.legend = FALSE
  ) +
  
  # CB labels (white)
  geom_text(
    data = centroids_pc %>% filter(A_SECTOR == "Capricorn-\nBunker"),
    aes(label = hd_num),
    colour = "white",
    fontface = "bold",
    size = 3.2,
    show.legend = FALSE
  )+
  
  scale_fill_manual(name = "Sector",
                    values = sector_cols,
                    breaks = names(sector_cols),
                    drop = FALSE) +
  scale_colour_manual(name = "Sector",
                      values = sector_cols,
                      breaks = names(sector_cols),
                      drop = FALSE) +
  
  scale_alpha_identity() +
  
  scale_shape_manual(
    name   = "Project",
    values = c(IN = 21, RM = 22),
    labels = c(IN = "MMP",
               RM = "LTMP")
  )+
  
  facet_wrap(~SHELF, scales = "free") +
  
  coord_cartesian(clip = "off") +
  
  theme_classic() +
  theme(
    legend.position = "top",
    panel.background = element_rect(color = "black"),
    legend.key = element_blank()
  )+
  guides(
    fill  = guide_legend(title = "", nrow = 1, override.aes = list(shape = 21, colour = "black"))
  )

g_start_end



#######################################################################################
# vector plot  ----------------------------------------------------------

load(file='vec.labels.RData')


ef_full<-envfit(benthic.capscale_full,response2_full,permu=999,p.max=0.001) 
scores(ef_full,"vectors") %>% as.data.frame() %>% head

vector_full <- data.frame(scores(ef_full, "vectors"), R = sqrt(ef_full$vectors$r), pval = ef_full$vectors$pvals) %>% #filter(R>0.5)
  mutate(flag = R>0.5)

vector_full$COMP_2021<-rownames(vector_full)


vector_full <- vector_full |> 
  left_join(vec.labels,by='COMP_2021')

vec2_full<-ggplot(site.scores.combined_full,aes(y=CAP2, x=CAP1))+
  geom_hline(yintercept=0, linetype='dotted')+
  geom_vline(xintercept=0, linetype='dotted')+
  geom_segment(data=vector_full %>% filter(R<=0.4),aes(x=0,xend=CAP1,y=0,yend=CAP2),show.legend = F,
               arrow=arrow(length=unit(0.15,'cm')),color='gray60')+
  geom_text(data=vector_full %>% filter(R<=0.4),aes(y=CAP2, x=CAP1,label=COMP_2021_DESCRIPTION),size=2.5,show.legend = F,
            color = "gray60")+#,hjust=0.4*(1-sign(RDA2)),vjust=0.2*(1-sign(RDA1))))+
  geom_segment(data=vector_full %>% filter(R>0.4),aes(x=0,xend=CAP1,y=0,yend=CAP2),arrow=arrow(length=unit(0.15,'cm')),show.legend = F)+
  geom_text(data=vector_full %>% filter(R>0.4),aes(y=CAP2, x=CAP1,label=COMP_2021_DESCRIPTION),
            size=4,show.legend = F)+#,hjust=0.4*(1-sign(RDA2)),vjust=0.2*(1-sign(RDA1))))+
  xlim(-0.7,0.65)+
  ylim(-0.75,0.75)+
  theme_classic()+
  theme(legend.position='bottom',
        panel.background=element_rect(color='black'),
        axis.text=element_text(size=12))+
  guides(fill='none',colour='none')
vec2_full


mar26_capscale <- g_start_end/vec2_full

mar26_capscale

ggsave(mar26_capscale,file='Main figs/mar26_capscale.pdf',height=11,width=11)
ggsave(mar26_capscale,file='Main figs/mar26_capscale.png',height=11,width=11)

#### note final formatting done outside R to improve readability
