#Ping-pong unit hydrograph simulation
library(dplyr)
library(shape)
# n <- 12 #total number of students
# l <- 5 #length of watershed (must be <= n)
# w <- 5 #maximum width of watershed (must be >= n/l)
# options <- c("top", "bottom", "random") #whether the watershed is built weighted to the top or bottom, or if it is random

#Function to create "watershed"
create_WS <- function(n, l, w, option){
  
  ws_ras <- matrix(NA, nrow = l, ncol = w)
  mid_col <- case_when(w < 3 ~ 1,
                       TRUE ~ ceiling(w / 2))
  ws_ras[,mid_col] <- 1 #all of middle column is part of ws
  ws_ras[nrow(ws_ras), mid_col] <- 0 #color outlet differently
  
  if (option == "random"){
    # #Find available cell indices
    # indices <- which(is.na(ws_ras))
    # 
    # #find random subset (that fulfills rest of ws size)
    # rand_indices <- sample(indices, size = n - l, replace = FALSE)
    # ws_ras[rand_indices] <- 1
    
    #Get row order
    rows <- sample(1:nrow(ws_ras), size = nrow(ws_ras), replace = FALSE)
    #Get col order
    cols <- c(mid_col + rep(1:(floor(ncol(ws_ras) / 2)), each = 2) * rep(c(-1, 1), by = floor(ncol(ws_ras) / 2)))
    
  }else if (option == "bottom"){
    # #start filling from bottom of ws up
    # #find how many rows can be fully filled
    # n_cells <- rev(apply(ws_ras, 1, function(x){sum(is.na(x))}))
    # n_rows <- max(which(cumsum(n_cells) <= n - l))
    # 
    # #fill those rows fully
    # ws_ras[nrow(ws_ras):(nrow(ws_ras) - n_rows + 1), !(1:ncol(ws_ras) %in% mid_col)] <- 1
    # 
    # #Fill any additional cells that need it
    # while (sum(!is.na(ws_ras)) < n){
    #   #find row with free NA spots
    #   row <- min(which(apply(ws_ras, 1, function(x){sum(is.na(x))}) == 0)) - 1
    #   col <- min(which(is.na(ws_ras[row,])))
    #   ws_ras[row, col] <- 1
      
      #Get row order
      rows <- nrow(ws_ras):1
      #Get col order
      cols <- c(mid_col + rep(1:(floor(ncol(ws_ras) / 2)), each = 2) * rep(c(-1, 1), by = floor(ncol(ws_ras) / 2)))
      
  }else if (option == "top"){
    # #start filling from top of ws down
    # #find how many rows can be fully filled
    # n_cells <- apply(ws_ras, 1, function(x){sum(is.na(x))})
    # n_rows <- max(which(cumsum(n_cells) <= n - l))
    # 
    # #fill those rows fully
    # ws_ras[1:n_rows, !(1:ncol(ws_ras) %in% mid_col)] <- 1
    # 
    # #Fill any additional cells that need it
    # while (sum(!is.na(ws_ras)) < n){
    #   #find row with free NA spots
    #   row <- max(which(apply(ws_ras, 1, function(x){sum(is.na(x))}) == 0)) + 1
    #   col <- min(which(is.na(ws_ras[row,])))
    #   ws_ras[row, col] <- 1
    # }
    
    #Get row order
    rows <- 1:nrow(ws_ras)
    #Get col order
    cols <- c(mid_col + rep(1:(floor(ncol(ws_ras) / 2)), each = 2) * rep(c(-1, 1), by = floor(ncol(ws_ras) / 2)))
    
  }else if (option == "middle"){
    #start filling from middle row
    mid_row <- ceiling(nrow(ws_ras) / 2)
    #mid_col <- which(ws_ras[nrow(ws_ras),] == 0)
    
    #Get row order
    rows <- c(mid_row, mid_row + rep(1:(floor(nrow(ws_ras) / 2)), each = 2) * rep(c(-1, 1), by = floor(nrow(ws_ras) / 2)))
    #remove any impossible values
    drop_rows <- which(rows < 1 | rows > nrow(ws_ras))
    if (length(drop_rows) > 0){
      rows <- rows[-drop_rows]
    }
    
    #Get col order
    cols <- c(mid_col + rep(1:(floor(ncol(ws_ras) / 2)), each = 2) * rep(c(-1, 1), by = floor(ncol(ws_ras) / 2)))

  }
  
  if (option == "middle"){
    for (i in cols){
      for (j in rows){
        ws_ras[j, i] <- 1
        if (sum(!is.na(ws_ras)) == n){break}
      }
      if (sum(!is.na(ws_ras)) == n){break}
    }
    
  }else{
    for (i in rows){
      for (j in cols){
        ws_ras[i, j] <- 1
        if (sum(!is.na(ws_ras)) == n){break}
      }
      if (sum(!is.na(ws_ras)) == n){break}
    }
  }
  

  return(ws_ras)
}

plot_WS <- function(ws_ras, ws_num = NULL){
  par(mar = c(2, 0.5, 0.5, 0.5))
  plot(NA, xlim = c(0.5, ncol(ws_ras)+0.5), ylim = c(0.5, nrow(ws_ras)+0.5), xaxt = "n", yaxt = "n", )
  x <- rep(1:ncol(ws_ras), each = nrow(ws_ras))
  y <- rep(nrow(ws_ras):1, ncol(ws_ras))
  colors <- c("red3", "royalblue3")
  points(x, y, pch = 21, cex = 5, bg = colors[as.vector(ws_ras) + 1])
  legend("bottom", legend = c("Student", "Outlet", "Empty Seat"), pch = 21, pt.bg = c("royalblue3", "red3", "white"), pt.cex = 2.5,
         horiz = TRUE, bty = "n", cex = 2, inset = c(0, -0.125), xpd = NA)
  
  #add arrows showing flow directions
  x2 <- x[which(as.vector(ws_ras) == 1)]
  y2 <- y[which(as.vector(ws_ras) == 1)]
  arrow_points <- data.frame(x = x2, y = y2)
  
  mid_col <- which(ws_ras[nrow(ws_ras),] == 0)
  left <- filter(arrow_points, x < mid_col)
  right <- filter(arrow_points, x > mid_col)
  mid <- filter(arrow_points, x == mid_col)
  
  Arrows(x0 = left$x, y0 = left$y, x1 = left$x + 0.5, y1 = left$y, code = 2, arr.type = "triangle", lwd = 1.5)
  Arrows(x0 = right$x, y0 = right$y, x1 = right$x - 0.5, y1 = right$y, code = 2, arr.type = "triangle", lwd = 1.5)
  Arrows(x0 = mid$x, y0 = mid$y, x1 = mid$x, y1 = mid$y - 0.5, code = 2, arr.type = "triangle", lwd = 1.5)
  
  #Add number of "balls" in each seat
  if (!is.null(ws_num)){
    text(x, y, ws_num, col = "white")
  }
}

route_WS <- function(ws_ras, plot = TRUE){
  #calculate distance from each point to ws outlet
  mid_col <- which(ws_ras[nrow(ws_ras),] == 0)
  
  col_dist <- abs(1:ncol(ws_ras) - mid_col)
  row_dist <- abs(1:nrow(ws_ras) - nrow(ws_ras))
  dist <- expand.grid(col_dist, row_dist)
  dist_mat <- matrix(dist$Var1 + dist$Var2, nrow = nrow(ws_ras), byrow = TRUE) + 1
  
  #Replace any distances which are outside the "watershed" with NA
  dist_mat[which(is.na(ws_ras))] <- NA
  
  # flow_acc <- matrix(NA, nrow = nrow(dist_mat), ncol = ncol(dist_mat))
  # for (i in 1:nrow(flow_acc)){
  #   for (j in 1:ncol(flow_acc)){
  #     if (j < mid_col){
  #       flow_acc[i, j] <- sum(!is.na(dist_mat[i, 1:j]))
  #     }else if (j > mid_col){
  #       flow_acc[i, j] <- sum(!is.na(dist_mat[i, j:ncol(dist_mat)]))
  #     }else{
  #       flow_acc[i, j] <- sum(!is.na(dist_mat[1:i,]))
  #     }
  #   }
  # }

  #plot unit hydrograph
  if (plot){
    par(mar = c(4, 4.5, 2, 0.5))
    # barplot(table(dist_mat), cex.axis = 1.5, las = 1, ylab = "Q", xlab = "Time", cex.names = 1.5, cex.lab = 1.5, main = "Unit Hydrograph",
    #         cex.main = 2)
    # stripchart(as.numeric(dist_mat), method = "stack", at = 1, offset = 1.7, pch = 21, bg = "blue", cex = 3, ylim = c(1, 5), xlab = "Time",
    #            ylab = "Q", las = 1)
    dist_table <- table(dist_mat)
    xvals <- rep(1:length(dist_table), dist_table)
    yvals <- unlist(sapply(dist_table, function(x){1:x}))
    plot(xvals, yvals, pch = 21, bg = "royalblue3", cex = 3, ylim = c(0.5, max(yvals) + 0.5), las = 1, ylab = "Q", xlab = "Time",
         main = "Unit Hydrograph", cex.main = 2, cex.axis = 1.5, xaxt = "n", cex.lab = 1.5)
    axis(side = 1, at = 1:max(xvals), cex.axis = 1.5)
  }

  return(dist_mat)
}

flow_acc_fun <- function(mat, mid_col){
  
  flow_acc <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:nrow(flow_acc)){
    for (j in 1:ncol(flow_acc)){
      if (j < mid_col){
        flow_acc[i, j] <- sum(!is.na(mat[i, 1:j]))
      }else if (j > mid_col){
        flow_acc[i, j] <- sum(!is.na(mat[i, j:ncol(mat)]))
      }else{
        flow_acc[i, j] <- sum(!is.na(mat[1:i,]))
      }
    }
  }
  
  return(flow_acc)
}

get_ws_num <- function(mat, mid_col){
  ws_num <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
  
  for (i in 1:nrow(ws_num)){
    for (j in 1:ncol(ws_num)){
      if (j == 1 | j == ncol(ws_num)){
        ws_num[i, j] <- NA
      }else{
        if (j < mid_col){
          ws_num[i, j] <- mat[i, j - 1]
        }else if (j > mid_col){
          ws_num[i, j] <-mat[i, j + 1]
        }else{
          ws_num[i, j] <- sum(c(mat[i, j - 1], mat[i, j + 1], mat[i - 1, j]), na.rm = TRUE)
        }
      }
    }
  }
  
  ws_num[ws_num == 0] <- NA
  return(ws_num)
}

animate_UH <- function(dist_mat, ws_ras){
  
  mid_col <- which(ws_ras[nrow(ws_ras),] == 0)
  
  flow_acc <- list()
  ws_ras_new <- list()
  ws_num <- list()
  
  flow_acc[[1]] <- flow_acc_fun(dist_mat, mid_col)
  #ws_num[[1]] <- get_ws_num(ws_ras, mid_col)
  ws_ras_new[[1]] <- ws_ras
  ws_num[[1]] <- ws_ras
  ws_num[[1]][ws_num[[1]] == 0] <- 1
    
  #Plot initial ws and blank UH
  # par(mfrow = c(1, 2), mar = c(2, 2, 1, 0.5))
  # plot_WS(ws_ras, ws_ras[ws_ras == 0] <- 1)
  # plot(NA, ylim = c(0.5, max(table(dist_mat)) + 0.5), xlim = c(1, length(table(dist_mat))),
  #      las = 1, ylab = "Q", xlab = "Time",
  #      main = "Unit Hydrograph", cex.main = 2, cex.axis = 1.5, xaxt = "n", cex.lab = 1.5)
  
  for(i in 1:max(dist_mat, na.rm = T)){
    #Get new flow accumulation
    mod_flow_acc <- flow_acc[[i]]
    mod_flow_acc[mod_flow_acc == 0 | mod_flow_acc == 1] <- NA
    flow_acc[[i + 1]] <- flow_acc_fun(mod_flow_acc, mid_col)
    
    #update watershed raster and plot
    ws_ras_new[[i + 1]] <- flow_acc[[i + 1]]
    ws_ras_new[[i + 1]][ws_ras_new[[i + 1]] == 0] <- NA
    ws_ras_new[[i + 1]][ws_ras_new[[i + 1]] == max(ws_ras_new[[i + 1]], na.rm = TRUE)] <- 0
    ws_ras_new[[i + 1]][ws_ras_new[[i + 1]] > 0] <- 1
    
    ws_num[[i + 1]] <- get_ws_num(ws_num[[i]], mid_col)
    
    
    # plot_WS(ws_ras_new[[i]], ws_num[[i]])
    
    #Plot UH
    # dist_table <- table(dist_mat)[1:i]
    # xvals <- rep(1:length(dist_table), dist_table)
    # yvals <- unlist(sapply(dist_table, function(x){1:x}))
    # plot(xvals, yvals, pch = 21, bg = "royalblue3", cex = 3, ylim = c(0.5, max(table(dist_mat)) + 0.5), xlim = c(1, length(table(dist_mat))),
    #     las = 1, ylab = "Q", xlab = "Time",
    #      main = "Unit Hydrograph", cex.main = 2, cex.axis = 1.5, xaxt = "n", cex.lab = 1.5)
    # axis(side = 1, at = 1:max(xvals), cex.axis = 1.5)
    
  }
  
  return(list(ws_ras = ws_ras_new, ws_num = ws_num))
  
}

plot_animation <- function(ws_ras, ws_num, dist_mat, iteration){
  par(mfrow = c(1, 2), mar = c(2, 2, 1, 0.5))
  
  if (is.null(ws_ras)){
   #Blank plots
    # plot(NA, ylim = c(0,1), xlim = c(0,1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    # plot(NA, ylim = c(0,1), xlim = c(0,1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  }else{
    plot_WS(ws_ras, ws_num)
    
    par(mar = c(4, 4.5, 2, 0.5))
    if (iteration == 1){
      plot(NA, ylim = c(0.5, max(table(dist_mat)) + 0.5), xlim = c(1, length(table(dist_mat))),
                 las = 1, ylab = "Q", xlab = "Time",
                 main = "Unit Hydrograph", cex.main = 2, cex.axis = 1.5, xaxt = "n", cex.lab = 1.5)
           
    }else{
      dist_table <- table(dist_mat)[1:(iteration - 1)]
      xvals <- rep(1:length(dist_table), dist_table)
      yvals <- unlist(sapply(dist_table, function(x){1:x}))
      plot(xvals, yvals, pch = 21, bg = "royalblue3", cex = 3, ylim = c(0.5, max(table(dist_mat)) + 0.5), xlim = c(1, length(table(dist_mat))),
           las = 1, ylab = "Q", xlab = "Time",
           main = "Unit Hydrograph", cex.main = 2, cex.axis = 1.5, xaxt = "n", cex.lab = 1.5)
      axis(side = 1, at = 1:max(xvals), cex.axis = 1.5)
    }
  }
}

storm_hydrograph <- function(ws_ras, ppt){
  #get UH
  UH <- table(route_WS(ws_ras, plot = FALSE))
  
  #Create storm hydrograph
  Q <- matrix(0, nrow = length(ppt), ncol = length(ppt) + length(UH) - 1)
  for (i in 1:length(ppt)){
    Q[i, i:(i + length(UH) - 1)] <- UH * ppt[i]
  }
  
  #remove any rows with only zeros (to keep colors consistent)
  zero_rows <- which(apply(Q, 1, sum) == 0)
  if (length(zero_rows > 0)){
    Q <- Q[-zero_rows, ]
  }
  
  par(mar = c(4.5, 4.5, 2, 1))
  layout(mat = matrix(c(0, 2, 1, 2), nrow = 2, byrow = TRUE),  widths = c(0.25, 0.75))
  colors <- RColorBrewer::brewer.pal(n = max(nrow(Q), 3), "Dark2")
  colors2 <- rep("black", length(ppt))
  colors2[which(!(1:length(colors2) %in% zero_rows))] <- colors
  barplot(ppt, las = 1, xlab = "Time", ylab = "Excess P [in]", names.arg = 1:length(ppt), col = colors2, cex.names = 1.5,
          cex.lab = 1.5, cex.axis = 1.5, main = "Hyetograph", cex.main = 2)
  box()
  barplot(Q, las = 1, xlab = "Time", ylab = "Q", names.arg = 1:ncol(Q), col = colors, cex.names = 1.5, cex.lab = 1.5,
          cex.axis = 1.5, main = "Storm Hydrograph", cex.main = 2)
  box()

}

# ws_ras <- create_WS(15, 5, 5, "random")
# plot_WS(ws_ras)
# 
# route_WS(ws_ras)
# 
# ppt <- c(1, 0, 0, 0, 0.5)
# test <- rep(1:5, each = 2)
# beeswarm::beeswarm(test, spacing = 2, cex = 1.5, pch = 21, bg = as.vector(ws_ras) + 1)

test_plot <- function(){
  plot(runif(5), runif(5))
}