setwd("~/Desktop/OneDrive/MA 386")
require(openxlsx)
df <- read.xlsx("DATA_STACKED.XLSX",sheet = 1)

# df <- head(df,200)


library(ggplot2)

library(plyr)
library(dplyr)
library(ggbiplot)
df = df[complete.cases(df),]
df = df[,sapply(df, function(v) var(v, na.rm=TRUE)!=0)]   # remove constant column
df_pca <- prcomp(df,center = TRUE,scale. = TRUE)





select_features=c("Q15_26","Q15_28")
sample_ratio = 0.1

myggbiplot(df_pca)

myggbiplot(df_pca,select_features = c("Q15_26","Q15_28"),sample_ratio = 0.05)



myggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE, select_features=NULL,
                     sample_ratio = 0.1,...)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  
  
  # stop condition
  stopifnot(length(choices) == 2)
  
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else {
    stop('Expected a object of class prcomp only')
  }
  
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  
  # select only desired features
  if(typeof(select_features)!=typeof(NULL)){
    df.v <- df.v[select_features,]
  }
  
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  
  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }
  
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }
  
  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }
  
  
  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    if (length(rownames(v))>length(df.v$varname)){
      if (typeof(select_features)!=typeof(NULL)){
        df.v$varname = select_features
      }else{
        df.v$varname=rownames(v)
      }
      
    }else{
      df.v$varname <- rownames(v)
    }
  }
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # set sample ratio
  if(sample_ratio<1){
    dat = df.u[sample(nrow(df.u),round(sample_ratio*nrow(df.u))),]
  }else{
    dat = df.u
  }
  
  # Base plot
  g <- ggplot(data = dat, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'), 
                         size = 1/2, alpha = 1/3)
    }
    
    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = muted('red'))
  }
  
  # Label the variable axes
  if(var.axes) {
    g <- g + 
      geom_text(data = df.v, 
                aes(label = varname, x = xvar, y = yvar, 
                    angle = angle, hjust = hjust), 
                color = 'darkred', size = varname.size)
  }
  
  
  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    print("get run in if")
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      print("get run")
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    } else {
      print("get run here")
      g <- g + geom_point(alpha = alpha)      
    }
  }
  return(g)
}
