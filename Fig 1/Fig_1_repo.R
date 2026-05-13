setwd("C:/Users/memslie/OneDrive - Australian Institute of Marine Science/Desktop/working folder/change in coral assemblages/text/for submission/code and data for repo/Fig 1")



library(tidyverse)
library(ggbreak)
library(vegan)
library(patchwork)
library(INLA)
library(ggmap)
library(maps)
library(mapdata)
library(sp)
library(sf)
library(gridExtra)
library(raster)
library(lwgeom)
library(lubridate)
library(rgeos)
library(rgdal)
library(ggspatial)
library(oz)
library(rnaturalearth)
library(shapefiles)
library(patchwork)
library(stringr)
library(colorspace)
library(ggtext)
library(scales)
library(purrr)



#############################################################################
# #build map --------------------------------------------------------------



towns.sf <- get(load("towns.sf.RData"))
reefs <- get(load("reefs.RData"))
reefsec <- get(load("reefsec.RData"))
sectors <- get(load("sectors.RData"))
reeflatlong <- get(load(file='reeflatlong.RData'))


######################################
# Oz map

aus<-map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),
         ylim=c(-45,-5), mar=c(0,0,0,0))

oz.plot<-  ggplot(map_data(aus), aes(y=lat, x=long, group=group)) + 
  geom_polygon(colour='grey20',fill='white')+
  geom_sf(data=sectors,inherit.aes=FALSE,fill='grey80',colour='grey20',linewidth=0.5)+  
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(0,0,0,0),'cm'))
oz.plot

######################################
# colours for Sectors

crs_string = "+proj=omerc +lat_0=-1 +lonc=155 +alpha=0 +k_0=1
+datum=WGS84 +units=m +no_defs +gamma=35"

locs <- tribble(
  ~label, ~color, ~long, ~lat,
  "Cape Grenville", "#A6CEE3", 144.5, -11.00,
  "Princess Charlotte Bay", "#1F78B4", 144.2, -13.3,
  "Cooktown-Lizard","#B2DF8A",146,-14.7,
  "Cairns","#33A02C",146.2,-16,
  "Innisfail","#FB9A99",147,-17.3,
  "Townsville","#E31A1C",148,-18.2,
  "Whitsunday","#FDBF6F",150,-19.3,
  "Pompey","#FF7F00",151.5,-20,
  "Swain","#CAB2D6",153,-21.5,
  "Capricorn-\nBunker","#6A3D9A",153.5,-23.5
)
save(locs,file='locs.RData')

locs$sector <- c("CG","PC","CL","CA","IN","TO","WH","PO","SW","CB")

locs_sf <- locs |>
  st_as_sf(coords = c("long", "lat"), crs = st_crs(towns.sf))

######################################
# plot map

reef_pts <- st_as_sf(reeflatlong,
                     coords = c("SITE_LONG","SITE_LAT"),
                     crs = st_crs(towns.sf))


bbox <- st_bbox(c(xmin = 143, ymin = -24, xmax = 146.5, ymax = -10),
                crs = st_crs(towns.sf))

rotated_crs <- st_crs(crs_string)
sf_use_s2(FALSE)
bbox <- bbox |> st_transform(crs = rotated_crs)

g1 <- ggplot()+
  geom_sf(data=towns.sf) +
  geom_sf(data=reefsec,aes(color = SECTOR_NAM),show.legend = FALSE)+
  geom_sf_text(data=towns.sf, aes(label=Town), size=5, hjust=1.1) +
  geom_sf(data=sectors,fill='white',linewidth=0.6,alpha=0) +
  geom_sf(data=reef_pts,size=1.75,shape=21,fill='grey30',alpha=0.8)+
  coord_sf(crs = st_crs(crs_string),
           expand = FALSE,
           label_axes = list(top = "E", bottom = "E", left = "N"),
           xlim = c(-2100000, bbox$xmax), ylim = c(-2200000, -60000)) +
  ggspatial::annotation_scale(text_cex=1)+
  ggspatial::annotation_north_arrow(which_north = "true",
                                    style = north_arrow_minimal(),
                                    height = unit(2, "cm"),
                                    width = unit(2, "cm"),
                                    pad_y=unit(1,"cm"),
                                    pad_x=unit(2,'cm')) +
  geom_sf_text(data = locs_sf |> filter(label == "Cape Grenville"),
               aes(label = label), color = "#A6CEE3", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Princess Charlotte Bay"),
               aes(label = label), color = "#1F78B4", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Cooktown-Lizard"),
               aes(label = label), color = "#B2DF8A", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Cairns"),
               aes(label = label), color = "#33A02C", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Innisfail"),
               aes(label = label), color = "#FB9A99", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Townsville"),
               aes(label = label), color = "#E31A1C", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Whitsunday"),
               aes(label = label), color = "#FDBF6F", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Pompey"),
               aes(label = label), color = "#FF7F00", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Swain"),
               aes(label = label), color = "#CAB2D6", size = 6, fontface = 2, hjust = 0) +
  geom_sf_text(data = locs_sf |> filter(label == "Capricorn-\nBunker"),
               aes(label = label), color = "#6A3D9A", size = 6, fontface = 2, hjust = 0) +
  scale_x_continuous(breaks=seq(140, 154, by = 2), #position="bottom",
                     sec.axis = sec_axis(transform = ~., breaks = c(145))) +
  scale_y_continuous(breaks = seq(-26, -10, by = 2)) +
  scale_colour_brewer(type='qual',palette = 'Paired')+
  scale_fill_brewer(type='qual',palette = 'Paired')+
  theme(axis.text=element_text(size=14))

g1 

g2 <-
  ggplot()+
  annotate(geom='text',label="Hard coral cover (%)",colour="black",x=155,y=-16.9,size=8,fontface=2,angle=90)+
  annotate(geom='text',label="Inner shelf",colour="black",x=159,y=-9.5,size=8,fontface=2)+
  annotate(geom='text',label="Mid-shelf",colour="black",x=165.5,y=-9.5,size=8,fontface=2)+
  annotate(geom='text',label="Outer shelf",colour="black",x=172,y=-9.5,size=8,fontface=2)+
  scale_colour_brewer(type='qual',palette = 'Paired')+
  scale_fill_brewer(type='qual',palette = 'Paired')+
  scale_x_continuous(position="bottom",limits=c(154,175), sec.axis = dup_axis())+
  scale_y_continuous(limits = c(-25, -9), sec.axis = dup_axis()) +
  coord_cartesian(expand = FALSE) #+

g2

map_vert <- (g1 + #ggtitle('a')+
               theme_void() +
               theme(
                 panel.spacing.x = unit(0, 'pt'),
                 plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
                 axis.line.y.right = element_blank(),
                 panel.border = element_blank(),
                 axis.line.x = element_line(color = "black", linewidth = 0.5),
                 axis.text.y.left = element_text(size=15),
                 axis.text.x = element_text(size=15),
                 axis.ticks.y.left = element_line(),
                 axis.ticks.x = element_line(),
                 axis.ticks.length = unit(2.75, 'pt'),
                 axis.line.y.left = element_line(),
                 panel.grid.major = element_line(color = "grey80"),
                 plot.title = element_text(hjust=-0.05,size=18,face = 'bold')
               )
) +
  
    (g2 + theme_void() +
     theme(
       axis.line.x = element_line(color = "black", linewidth = 0.5),
       axis.line.y.right = element_line(color = "black", linewidth = 0.5)
     ))

map_vert 



#################
## plot individ comp21_barplots

load(file='comp2021_Oct2025_df.RData')

comp2021_Oct2025_df_barplot <- comp2021_Oct2025_df %>%
  mutate(
    cCOMP_2021 = factor(cCOMP_2021, levels = unique(cCOMP_2021)),
    COMP_2021_DESCRIPTION = factor(COMP_2021_DESCRIPTION,
                                   levels = unique(COMP_2021_DESCRIPTION))
  )

# base colors by ms_group
base_colors <- c(
  "Acropora" = "#e41a1c",
  "branching coral" = "#ffd700",
  "encrusting foliose coral" = "#984ea3",
  "massive Porites" = "#377eb8",
  "massive submassive coral" = "#00bfc4",
  "free living coral" = "#f781bf",
  "free living_solitary coral" = "#a65628"
)

# per-category shades 
fill_values_tbl <- comp2021_Oct2025_df_barplot %>%
  distinct(ms_group, cCOMP_2021) %>%
  arrange(ms_group, cCOMP_2021) %>%
  group_by(ms_group) %>%
  mutate(color = lighten(base_colors[as.character(ms_group)],
                         amount = seq(0, 0.4, length.out = n()))) %>%
  ungroup()

fill_values <- setNames(fill_values_tbl$color, as.character(fill_values_tbl$cCOMP_2021))

# legend labels
legend_labels <- comp2021_Oct2025_df_barplot |>
  distinct(cCOMP_2021, COMP_2021_DESCRIPTION) |>
  mutate(
    cCOMP_2021 = as.character(cCOMP_2021),
    COMP_2021_DESCRIPTION = as.character(COMP_2021_DESCRIPTION)
  ) |>
  with(setNames(COMP_2021_DESCRIPTION, cCOMP_2021))

# adding italics


legend_labels_md <- purrr::imap_chr(
  legend_labels,
  ~ {
    code  <- .y
    label <- .x
    
    # 1. Skip italicising if family-level code
    if (str_starts(code, "F_")) {
      return(label)
    }
    
    # 2. Italicise full genera and abbreviated genus + species
    label2 <- label |>
      # Abbreviated genus + species: A. pachysepta → *A. pachysepta*
      str_replace_all("\\b([A-Z])\\.\\s+([a-z]+)", "*\\1. \\2*") |>
      # Full genera: Acropora, Porites, etc. → *Acropora*
      str_replace_all("\\b([A-Z][a-z]+)\\b", "*\\1*")
    
    label2
  }
)


# working data
comp.dat <- comp2021_Oct2025_df_barplot %>%
  mutate(
    sub_region = paste(A_SECTOR, SHELF, sep = "_"),
    A_SECTOR = factor(A_SECTOR, levels = c("CG","PC","CL","CA","IN","TO","WH","PO","SW","CB")),
    SHELF   = factor(SHELF,   levels = c("I","M","O")),
    year_num = as.integer(REPORT_YEAR)
  ) %>%
  arrange(A_SECTOR, SHELF, year_num)

# initialise list with correct names
subs <- unique(comp.dat$sub_region)
plots.comp <- vector("list", length(subs))
names(plots.comp) <- subs

# map sector codes in comp.dat to labels in locs
sector_key <- tibble(
  A_SECTOR = c("CG","PC","CL","CA","IN","TO","WH","PO","SW","CB"),
  label = c(
    "Cape Grenville",
    "Princess Charlotte Bay",
    "Cooktown-Lizard",
    "Cairns",
    "Innisfail",
    "Townsville",
    "Whitsunday",
    "Pompey",
    "Swain",
    "Capricorn-\nBunker"
  )
)

# join colours onto sector codes
sector_cols_df <- sector_key %>%
  left_join(locs %>% dplyr::select(label, color), by = "label")

plots.comp <- list()

bottom_subs <- c("CB_I","SW_M","CB_O")

#### subregion key for disturbance lookup

dist_lookup <- read.csv(file='subregion_dist_recovery.csv',strip.white=TRUE)

dist_lookup_short <- dist_lookup |> 
  dplyr::select(A_SECTOR,SHELF,Disturbance,Before_yr,After_yr,Recovery_yr) |> 
  mutate(Before_yr=factor(Before_yr),
         After_yr=factor(After_yr),
         Recovery_yr=factor(Recovery_yr))



# make lookup compatible with panel names
dist_lookup_short2 <- dist_lookup_short %>%
  mutate(
    sub_region = paste(A_SECTOR, SHELF, sep = "_"),
    Before_yr_plot   = Before_yr - 0.18,
    Recovery_yr_plot = Recovery_yr + 0.18
  )

#### loop through plots

for (f in subs) {
  
  dat_f <- comp.dat %>%
    dplyr::filter(sub_region == f)
  
  comp_year <- dat_f %>%
    dplyr::group_by(REPORT_YEAR, year_num, cCOMP_2021) %>%
    dplyr::summarise(cover_mean = mean(cover, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(REPORT_YEAR))
  
  # get sector and shelf for this panel directly from the data
  sector_f <- dat_f %>%
    dplyr::distinct(A_SECTOR) %>%
    dplyr::pull(A_SECTOR) %>%
    .[1]
  
  shelf_f <- dat_f %>%
    dplyr::distinct(SHELF) %>%
    dplyr::pull(SHELF) %>%
    .[1]
  
  # axis colour
  axis_col <- sector_cols_df %>%
    dplyr::filter(A_SECTOR == sector_f) %>%
    dplyr::pull(color) %>%
    .[1]
  
  # disturbance years for this panel
  dist_f <- dist_lookup_short2 %>%
    dplyr::filter(A_SECTOR == sector_f, SHELF == shelf_f)
  
  ## ----- 3 y-axis ticks based on TOTAL hard coral cover -----
  totals <- comp_year %>%
    dplyr::group_by(REPORT_YEAR) %>%
    dplyr::summarise(total_cover = sum(cover_mean, na.rm = TRUE), .groups = "drop")
  
  ymax_raw <- max(totals$total_cover, na.rm = TRUE)
  
  if (!is.finite(ymax_raw) || ymax_raw <= 0) {
    y_top <- 10
  } else {
    y_top <- ceiling(ymax_raw / 10) * 10
  }
  
  y_top_plot <- y_top * 1.10
  y_mid      <- y_top / 2
  ybreaks    <- c(0, y_mid, y_top)
  
  # positions for markers inside the panel
  y_before   <- y_top_plot * 0.985
  y_after    <- y_top_plot * 0.955
  y_recovery <- y_top_plot * 0.925
  
  # make one plotting dataframe for the markers
  marker_df <- bind_rows(
    dist_f %>%
      transmute(
        x = as.numeric(as.character(Before_yr)),
        y = y_before,
        marker_type = "Before"
      ),
    dist_f %>%
      transmute(
        x = as.numeric(as.character(After_yr)),
        y = y_after,
        marker_type = "After"
      ),
    dist_f %>%
      transmute(
        x = as.numeric(as.character(Recovery_yr)),
        y = y_recovery,
        marker_type = "Recovery"
      )
  ) %>%
    dplyr::filter(dplyr::between(x, 1992, 2026))
  
  p <- ggplot(comp_year, aes(x = REPORT_YEAR, y = cover_mean, fill = cCOMP_2021)) +
    geom_col(width = 0.9, colour = NA, show.legend = FALSE) +
    
    scale_fill_manual(
      values = fill_values,
      name = "Coral group",
      breaks = names(legend_labels),
      labels = legend_labels
    ) +
    
    scale_y_continuous(
      "",
      breaks = ybreaks,
      labels = scales::label_number(accuracy = 1),
      expand = c(0, 0)
    ) +
    
    scale_x_continuous(
      breaks = c(1995, 2000, 2005, 2010, 2015, 2020, 2025),
      expand = c(0, 0)
    ) +
    
    geom_vline(
      xintercept=c(1995,2000,2005,2010,2015,2020),
      linetype='dashed',
      colour='grey'
    )+
    
    # disturbance symbols at top of panel
    geom_point(
      data = marker_df,
      aes(x = x, y = y, shape = marker_type, colour = marker_type),
      inherit.aes = FALSE,
      size = 2.5,
      stroke = 0.6,
      show.legend = FALSE
    ) +
    
    scale_shape_manual(
      values = c(
        Before   = 16,  # filled circle
        After    = 16,  # filled square
        Recovery = 16   # filled triangle
      )
    ) +
    
    scale_colour_manual(
      values = c(
        Before   = "dodgerblue4",
        After    = "firebrick3",
        Recovery = "goldenrod3"
      )
    ) +
    
    coord_cartesian(
      xlim = c(1992, 2026),
      ylim = c(0, y_top_plot),
      clip = "off"
    ) +
    
    theme_classic() +
    theme(
      axis.title.x    = element_blank(),
      plot.margin     = margin(0.5, 0.1, 0.1, 0.1, "cm"),
      legend.position = "bottom",
      axis.line.x     = element_line(colour = axis_col, linewidth = 0.9),
      axis.line.y     = element_line(colour = axis_col, linewidth = 0.9),
      panel.border    = element_rect(colour = axis_col, linewidth = 0.9, fill = NA)
    )
  
  if (f %in% bottom_subs) {
    p <- p + theme(
      axis.text.x  = element_text(size = 14),
      axis.text.y  = element_text(size = 16),
      axis.ticks.x = element_line()
    )
  } else {
    p <- p + theme(
      axis.text.x  = element_blank(),
      axis.text.y  = element_text(size = 16),
      axis.ticks.x = element_line()
    )
  }
  
  plots.comp[[f]] <- p
}

plots.comp[['CA_O']]
wrap_plots(plots.comp)


## arrange plots

pl <- "
ZAB
ZCD
EFG
HIJ
KLM
NOP
QRS
#T#
#UV
W#X"

data.plots_comp <- wrap_plots(
  Z = oz.plot,
  A = plots.comp[['CG_M']],
  B = plots.comp[['CG_O']],
  C = plots.comp[['PC_M']],
  D = plots.comp[['PC_O']],
  E = plots.comp[['CL_I']],
  F = plots.comp[['CL_M']],
  G = plots.comp[['CL_O']],
  H = plots.comp[['CA_I']],
  I = plots.comp[['CA_M']],
  J = plots.comp[['CA_O']],
  K = plots.comp[['IN_I']],
  L = plots.comp[['IN_M']],
  M = plots.comp[['IN_O']],
  N = plots.comp[['TO_I']],
  O = plots.comp[['TO_M']],
  P = plots.comp[['TO_O']],
  Q = plots.comp[['WH_I']],
  R = plots.comp[['WH_M']],
  S = plots.comp[['WH_O']],
  T = plots.comp[['PO_M']],
  U = plots.comp[['SW_M']],
  V = plots.comp[['SW_O']],
  W = plots.comp[['CB_I']],
  X = plots.comp[['CB_O']],
  design = pl,
  guides = "collect"
) +
  theme(legend.position = "bottom")

data.plots_comp

#########################
## legend only plot

library(ggtext)

# Create a dummy dataset with one row per legend entry
df_leg <- data.frame(
  cCOMP_2021 = factor(names(legend_labels), levels = names(legend_labels))
)

legend_plot <- ggplot(df_leg, aes(x = 1, y = 1, fill = cCOMP_2021)) +
  geom_tile(show.legend = TRUE) +
  scale_fill_manual(
    values = fill_values,
    breaks = names(legend_labels_md),
    labels = legend_labels_md
  ) +
  guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_markdown(size = 14),
    legend.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )
# Remove everything except the legend
legend_plot <- cowplot::get_legend(legend_plot)

# To render just the legend
cowplot::plot_grid(legend_plot)


##### combine map and plots


gbr.map_rotated <- map_vert + 
  inset_element(data.plots_comp,left=0.075,bottom=0.04,right=0.97,top=0.93)+ 
  theme(legend.position = "none")

gbr.map_rotated


pl_legend <- "AAAA
              AAAA
              AAAA
              AAAA
              BBBB
              "


gbr.map_rotated <- gbr.map_rotated/legend_plot+
  plot_layout(design = pl_legend)
#plot_layout(height=c(0.9,0.1))

gbr.map_rotated


ggsave(gbr.map_rotated,file='Fig 1.png',height=20,width=22,type='cairo')
ggsave(gbr.map_rotated,file='Fig 1.pdf',device = cairo_pdf,height=20,width=22)
