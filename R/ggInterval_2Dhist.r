#' @name ggInterval_2Dhist
#' @title visualize a 2-dimension histogram by symbolic data with ggplot
#' package.
#' @description Visualize the two continuous variable distribution
#' by dividing both the x axis and y axis into bins,and calculating
#' the frequency of observation interval in each bin.
#' @import tidyverse rlang ggplot2
#' @importFrom RSDA is.sym.interval
#' @importFrom dplyr between
#' @param data A ggESDA object. It can also be either RSDA object or
#' classical data frame,which will be automatically convert to ggESDA
#' data.
#' @param mapping Set of aesthetic mappings created by aes() or aes_().
#' If specified and inherit. aes = TRUE (the default),
#' it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' It is the same as the mapping of ggplot2.
#' @param x_bins x axis bins, which mean how many partials
#' x variable will be separate into.
#' @param y_bins y axis bins.It is the same as x_bins.
#' @param is_zero_remove whether remove data whose frequency is equal to zero
#' @param is_frequency_visible where add frequency text in each cells.
#' @return Return a ggplot2 object.
#' @usage ggInterval_2Dhist(data = NULL, mapping = aes(NULL),
#'  x_bins = 14, y_bins = 16, is_zero_remove = FALSE,
#'  is_frequency_visible=TRUE)
#' @examples
#' ggInterval_2Dhist(oils, aes(x = GRA, y = FRE),
#'   x_bins = 5, y_bins = 5)
#'
#' @export
ggInterval_2Dhist <- function(data = NULL, mapping = aes(NULL),
                              x_bins = 14, y_bins = 16, is_zero_remove = FALSE,
                              is_frequency_visible = TRUE){
  # test time consuming
  if(x_bins + y_bins > 200) {
    stop("ERROR : Bins are too large to calculate.Suggest two bins be smaller than 100.")
  }

  # data preparing
  argsNum <- length(mapping)
  args <- lapply(mapping[1:argsNum], FUN = rlang::get_expr)
  aes_x <- args$x; aes_y <- args$y

  # test data illegal
  iData <- test_data_type(data)$symbolic_data
  num_of_variables <- dim(data)[2]
  num_of_concepts <- dim(iData)[1]
  result <- NULL

  # test time consuming
  if(num_of_concepts * x_bins * y_bins >= 35000 & num_of_concepts * x_bins * y_bins < 50000){
    warning("The number of observations and bins are not suggested be too large.")
  }else if(num_of_concepts * x_bins * y_bins >= 50000){
    stop("The number of observations and bins are too large that will out of time limit.")
  }

  # start process
  with(data, {
    # get attrs
    attr1 <- which(unlist(lapply(data[, 1:num_of_variables], FUN = identical, x = eval(aes_x))))
    attr1 <- names(attr1)
    attr2 <- which(unlist(lapply(data[, 1:num_of_variables], FUN = identical, x = eval(aes_y))))
    attr2 <- names(attr2)

    # if cannot find attr
    if(length(attr1) == 0 || length(attr2) == 0){
      stop("ERROR : Missing attributes x or y in data frame.")
    }

    # test attribute illegal
    if(all((!is.numeric(data[[attr1]])), !RSDA::is.sym.interval(data[[attr1]]))
        ||  all((!is.numeric(data[[attr2]])), !RSDA::is.sym.interval(data[[attr2]]))){
      stop("ERROR : Variables in Scatter Plot can only be numeric.")
    }

    # prepare loop data
    r <- 8
    x_minimum <- min(iData[[attr1]]$min)
    x_maximum <- max(iData[[attr1]]$max)
    y_minimum <- min(iData[[attr2]]$min)
    y_maximum <- max(iData[[attr2]]$max)
    x_interval <- seq(x_minimum, x_maximum, (x_maximum - x_minimum) / x_bins)
    y_interval <- seq(y_minimum, y_maximum, (y_maximum - y_minimum) / y_bins)
    frequency_matrix <- matrix(0, nrow = x_bins, ncol = y_bins)

    # start loop to calculate frequency values in histogram matrix
    for(xi in 1:(length(x_interval) - 1)){
      for(yi in 1:(length(y_interval) - 1)){
        for(concept in 1:num_of_concepts){
          fx <- 0; fy <- 0
          # calculate x
          a <- iData[[attr1]][concept]$min
          b <- iData[[attr1]][concept]$max
          head_inside <- a %>% between(x_interval[xi], x_interval[xi + 1])
          tail_inside <- b %>% between(x_interval[xi], x_interval[xi + 1])
          contain <- x_interval[xi] %>% between(a, b)
          if(head_inside | tail_inside | contain){
            temp <- sort(c(x_interval[xi], x_interval[xi + 1], a, b))
            if(b - a == 0){
              fx <- fx + 1
            }else{
              fx <- fx + ((temp[3] - temp[2]) / (b - a))
            }
          }

          # calculate y
          a <- iData[[attr2]][concept]$min
          b <- iData[[attr2]][concept]$max
          head_inside <- a %>% between(y_interval[yi], y_interval[yi + 1])
          tail_inside <- b %>% between(y_interval[yi], y_interval[yi + 1])
          contain <- y_interval[yi] %>% between(a, b)
          if(head_inside | tail_inside | contain){
            temp <- sort(c(y_interval[yi], y_interval[yi + 1], a, b))
            if(b - a == 0){
              fy <- fy + 1
            }else{
              fy <- fy + ((temp[3] - temp[2]) / (b - a))
            }
          }
          frequency_matrix[xi,yi] <- frequency_matrix[xi, yi] + fx * fy
        }
      }
    }
    # end loop for calculate

    # build data frame to plot (combine margin and values)
    frequency_matrix <- c(as.matrix(frequency_matrix))
    if(all(round(frequency_matrix, 1) == 0)){
      warning("Visualize data by 10 times frequency.")
      frequency_matrix <- frequency_matrix * 10
    }
    frequency_matrix<-cbind(
        frequency_matrix,
        rep(x_interval[1:(length(x_interval) - 1)], length(y_interval) - 1),
        rep(x_interval[2:length(x_interval)], length(y_interval) - 1),
        rep(y_interval[1:(length(y_interval) - 1)], each = length(x_interval) - 1),
        rep(y_interval[2:length(y_interval)], each = length(x_interval) - 1)
    )
    colnames(frequency_matrix) <- c("freq", "x1", "x2", "y1", "y2")
    frequency_matrix <- as.data.frame(frequency_matrix)
    frequency_matrix[, "xmid"] <- (frequency_matrix$x1 + frequency_matrix$x2) / 2
    frequency_matrix[, "ymid"] <- (frequency_matrix$y1 + frequency_matrix$y2) / 2

    # escape sparse matrix
    if(is_zero_remove){
      frequency_matrix <- frequency_matrix[frequency_matrix$freq != 0, ]
    }
    midpoint <- (max(frequency_matrix$freq) + min(frequency_matrix$freq)) / 2

    # build Aesthetic
    origin_mapping <- mapping[-c(1, 2)] # Aesthetic without x,y
    converted_mapping <- list(data = frequency_matrix,
                              mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2,
                              fill = freq, alpha = 0.5))
    plot_mapping <- as.list(
      structure(
        as.expression(c(origin_mapping, converted_mapping)),
        class = "uneval"
        )
      )

    # plot
    base <- ggplot(frequency_matrix, aes(x1, y1)) +
      do.call(geom_rect, plot_mapping) +
      scale_fill_gradient2(low = "blue", mid="yellow",
                           high = "red", midpoint = midpoint,
                           limits = c(0, max(frequency_matrix$freq))) +
      labs(x = attr1, y = attr2, title = "2D hist.")+
      guides(alpha = FALSE)
    if(is_frequency_visible){
      base <- base + geom_text(aes(x = xmid, y = ymid, label = round(freq, 1)))
    }
    result[["plot"]] <- base

    # make table
    myTable <- matrix(0, nrow = x_bins, ncol = y_bins)
    for(r in 1:x_bins){
      for(c in 1:y_bins){
        myTable[r, c] <- round(frequency_matrix$freq[(c - 1) * x_bins + r], 3)
      }
    }
    col_names <- paste0("[", paste(unique(round(frequency_matrix$y1), 2), round(unique(frequency_matrix$y2), 2), sep = ":"), "]")
    row_names <- paste0("[", paste(unique(round(frequency_matrix$x1), 2), round(unique(frequency_matrix$x2), 2), sep = ":"), "]")
    s1 <- apply(myTable, 1, sum)
    s2 <- apply(myTable, 2, sum)
    myTable <- cbind(myTable, s1, round(s1 / num_of_concepts, 3))
    myTable <- rbind(myTable, c(s2, num_of_concepts, " "),
                     c(round(s2 / num_of_concepts, 3), " ", 1))
    rownames(myTable) <- c(row_names, paste0("Frequency of ", attr1), paste0("Margin of ", attr1))
    colnames(myTable) <- c(col_names, paste0("Frequency of ", attr2), paste0("Margin of ", attr2))
    result[[paste0("Table (", attr1, ", ", attr2, ")")]] <- as.data.frame(myTable)
    return(result)
  })
}
