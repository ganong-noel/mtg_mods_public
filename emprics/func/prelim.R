require(rdrobust)
require(KernSmooth)
require(rdd)
require(dplyr)
require(ggplot2)
require(readr)
require(RColorBrewer)
require(scales)
require(AER)
require(grid)
require(lazyeval) #PG added 2016-06-02
require(tidyr)
require(stringr) #CK moved this from master 2018-04-09

wd <- 6
ht <- 4

top_code <- function(vec, level=100) {
  ifelse(vec > level, level, vec)
}
bottom_code <- function(vec, level=0) {
  ifelse(vec < level, level, vec)
}


cbPalette <- c("#000000", "#0072B2", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")
cbPalette_drop2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")
cbPalette_drop1 <- c("#0072B2", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")
cbPalette_blue <- brewer.pal("Blues", n=3)[c(3,2)]
cbPalette_grey <- brewer.pal("Greys", n=3)
cbPalette_set2 <- brewer.pal("Set2",n=8)
cbPalette_set2 <- c(cbPalette_set2[1:5], cbPalette_set2[7:8]) 


fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = "white"
  color.grid.major = palette[3]
  color.axis.text = palette[7]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=12, base_family = "serif") +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(size=14,color=color.axis.title,family="serif")) +
    
    # Set title and axis labels, and format these and tick marks
    theme(axis.text=element_text(size = rel(1), color=color.axis.text)) +
    theme(axis.title.x=element_text(color=color.axis.title,vjust=0)) +
    theme(axis.title.y=element_text(color=color.axis.title,vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}


monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon 
}

mondf <- function(d1, d2) {
  monnb(d2) - monnb(d1)
}

coef_label <- function(text, loc, size) {
  if (loc == "ur") {
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1.07, family = "serif", 
             color = brewer.pal("Greys", n = 9)[7], size = size, label = text)
  }
  else if (loc == "lr") {
    annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -0.1, family = "serif", 
             color = brewer.pal("Greys", n = 9)[7],  size = size, label = text)
  }
  else if (loc == "ul") {
    annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1.07, family = "serif", 
             color = brewer.pal("Greys", n = 9)[7],  size = size, label = text)
  }
  else if (loc == "ll") {
    annotate("text", x = -Inf, y = -Inf, hjust = 0, vjust = -0.03, family = "serif", 
             color = brewer.pal("Greys", n = 9)[7],  size = size, label = text)
  }
}

#features needed in the future
#figure out a way to add arbitrary geoms and aesthetics. maybe pass a ... argument?
#figure out a way to automatically create labels from the group variable of a file
gg_coef <- function(df, dvs, file_suffix, title=NA, 
                    xlab = "Bandwidth", ylab = "RD Estimate and 95% CI", 
                    group_var = "polynomial", y_range = NA,
                    caption = "UST Projected Impact",
                    legend_title = "polynomial", legend_labels = c("linear","quadratic"), legend_pos = c(1,0),
                    out_path_f = out_path, samp_name = samp_file_name) {
  gg <-
    ggplot(df %>% filter(dv %in% dvs) %>% mutate_(group = group_var)) +
    geom_point(aes(x=bw,y=coef, shape=group,colour=group, ymax=coef), position=position_dodge(width=0.05)) +
    geom_errorbar(aes(x=bw,y=coef,colour=group,ymax=coef_max, ymin=coef_min, width = 0.05), position=position_dodge(width=0.05)) + 
    fte_theme() + geom_hline(yintercept=d_delin_ust) +  labs(x=xlab, y=ylab) + 
    scale_colour_manual(legend_title,values=cbPalette_blue, labels = legend_labels)  + 
    scale_shape_discrete(legend_title, labels = legend_labels) +
    theme(legend.position=legend_pos, legend.justification=legend_pos)
  if (!is.na(y_range[1])) {
    gg <- gg + coord_cartesian(ylim = c(y_range[1],y_range[2]))
  }
  ggsave(gg,file=paste0(out_path_f,"rd_",samp_name,file_suffix), width = wd, height = ht)
  #return(gg)
}
#features needed: 
#right now this code automatically alphabetizes the dependent variables, which can lead to errors in legend labels
#add automatic placement of text notes
gg_bin <- function(df, dvs, file_suffix, coef, se, 
                   xlab, ylab, bw = bw_preferred, 
                   x = "x", scales = NA, y_size=NA, 
                   caption = NA, loc = "ur", coef_size = 4.23, samp_name = samp_file_name,
                   out_path_f = out_path, n.cut = n_cut, ll = TRUE, title=NA,
                   legend_title = "Outcome", legend_labels = dvs, legend_pos = c(1,0),
                   y_range = NA, y_div=NA, grid=TRUE,  save_temp=FALSE) {
  tmp <- data.frame(cut = double(), group = character(), x = double(), y = double())
  for (dv_each in dvs) {
    tmp <- rbind.data.frame(tmp,binscatter(dv_each, x, df %>% filter(abs(x) < bw_window), n.cut = n.cut)$df_bin %>% 
                              mutate(group = dv_each))
  }
  #tmp %>% str
  #tmp %>% print
  #tmp %>% group_by(group) %>% summarise(n()) %>% print
  if (length(dvs) == 1) {
    gg <- ggplot(tmp, aes(x=x, y=y, group=group)) + geom_point(color="navy") +
      labs(x=xlab, y=ylab) + fte_theme() 
    if (ll == TRUE) {
      line <- loc_lin(df, y = dvs, bw = bw, cutoff = cutoff, range = c(-bw_window,bw_window))
      line_alt <- rbind(line$line_minus, line$line_plus) %>% mutate(left_side = row_number() <= gran%/%2 + 1)
      gg <- gg +  geom_line(data = line_alt,aes(x=x, y=y, group = left_side, colour="red"))
    }
  }
  else if (length(dvs) >= 2) {
    gg <- ggplot(tmp, aes(x=x, y=y, group=group,shape=group,colour=group)) + geom_point() + 
      labs(x=xlab, y=ylab) +fte_theme()
    gg <- gg + scale_colour_manual(legend_title,values=c("navy",cbPalette[2:3]), labels = legend_labels)  + 
      scale_shape_discrete(legend_title, labels = legend_labels) +
      theme(legend.position=legend_pos, legend.justification=legend_pos)
  }
  if (!is.na(y_range[1])) {
    gg <- gg + coord_cartesian(ylim = c(y_range[1],y_range[2]))
  }
  if (!is.na(caption)) {
    gg <- gg + coef_label(caption_text, loc, coef_size)
  }
  if (!is.na(title)) {
    gg <- gg +  ggtitle(title)
  }
  
  if (!is.na(scales)) {
    if (scales == "percent") {
      if (length(y_div)>1) {
      gg <- gg + scale_y_continuous(breaks=y_div, labels = scales::percent)
      } else {
        gg <- gg + scale_y_continuous(labels = scales::percent)
      }
    }
    if (scales == "dollar") {
      gg <- gg + scale_y_continuous(labels = scales::dollar)
    }
  }
  
  if (grid==FALSE) {
    gg <- gg + theme(
                    #axis.line = element_line(colour = "gray", linetype = "solid", size=0.5),
                     panel.grid.major= element_blank(), 
                     panel.grid.minor = element_blank())+ geom_vline(xintercept=0, color="gray", size=0.5)
    
  }
  
  if (save_temp==TRUE) {
    saveRDS(tmp, paste0(out_path_f,"rd_data_temp.RDS"))
    saveRDS(line_alt, paste0(out_path_f,"rd_data_line.RDS"))                 
  }
  
  ggsave(gg,file=paste0(out_path_f,"rd_",samp_name,file_suffix), width = wd, height = ht)
  #return(gg)
}

#Show all decimals even if rounding gives 0
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
