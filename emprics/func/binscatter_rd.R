

binscatter <- function(y, x, df, z, group, n.cut = 20, cluster, qt_use_all = TRUE, qt_samp = 10000) {
  require(multiwayvcov)
  if(missing(group) == TRUE) {
    df$group <- 1
  } else {
    df$group <- df[,group]
  }
  if (missing(z) == FALSE) {
    x_res_string <- paste0("df$",x," ~ ",paste0("df$",z, collapse= " + "))
    print(x_res_string)
    y_res_string <- paste0("df$",y," ~ ",paste0("df$",z, collapse= " + "))
    print(y_res_string)
    df$xRes <- residuals(lm(as.formula(x_res_string), na.action="na.exclude")) + mean(df[,x], na.rm = TRUE)
    df$yRes <- resuduals(lm(as.formula(y_res_string), na.action="na.exclude")) + mean(df[,y], na.rm = TRUE)
    reg_string <- paste0("df$",y," ~ df$",x," + ",paste0("df$",z, collapse= " + "))
  } else {
    df$xRes <- df[,x]
    df$yRes <- df[,y]
    reg_string <- paste0("df$",y," ~ df$",x)
  }
  
  print(paste0("Binscatter with ",reg_string))
  lin_model <- lm(as.formula(reg_string))
  if (missing(cluster) == FALSE) {
    lin_model.vcov_cluster <- cluster.vcov(lin_model, as.factor(df[,cluster]))
    cluster_se <- sqrt(lin_model.vcov_cluster[2,2])
  } else {
    cluster_se <- NULL
  }
  
  #being new code for RD
  #qts <- unique(quantile( df$xRes, seq(from=0,to=1,length.out=n.cut+1), na.rm = TRUE))
  qts <- c(unique(quantile( df[df$xRes<0,"xRes"] , seq(from=0,to=1,length.out=n.cut+1), na.rm = TRUE)),
               unique(quantile( df[df$xRes>=0,"xRes"], seq(from=0,to=1,length.out=n.cut+1), na.rm = TRUE))[-1])
  #print(qts)
  # end new code for RD
  df_bin <- df %>% mutate(cut = as.numeric(cut(xRes, qts ,include.lowest = TRUE))) %>%
    group_by(cut,group) %>% summarise( x = mean(xRes, na.rm =  TRUE), y = mean(yRes, na.rm = TRUE))
  return_list <- list("df_bin" = df_bin, "in_model" = lin_model, "cluster_se" = cluster_se)
  return(return_list)
}

#binscatter("anyDelin", "x", df %>% filter(abs(x) < bw_window))$df_bin %>% data.frame
