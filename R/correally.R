#' Validate a hexadecimal color representation.
#'
#' `validateHex` returns if a string vector contains color representations in hexadecimal format.
#'
#' This function validates a given hexadecimal color representation.
#' It can check the normal hexadecimal notation (e.g. #FFFFFF) or the shorthand notation (e.g. #FFF).
#'
#' @param string String vector.
#' @param shorthand A logical scalar. If \emph{TRUE}, the validation is performed against the shorthand notation and if \emph{FALSE},
#' validation is performed against the normal notation.
#' @param ignore.case A logical scalar. If \emph{FALSE}, the validation is case sensitive and if \emph{TRUE}, case is ignored during validation.
#'
#' @return Returns a logical vector of the same length as the \code{string} input.
#'
#' @examples
#' validateHex("#FFFFFF")
#' validateHex("#FFF")
#' validateHex("#FFF", shorthand = TRUE)
#'
#' validateHex("#ffffff")
#' validateHex("#ffffff", ignore.case = TRUE)
#'
#' validateHex(c("#FFFFFF","asdf"))
#'
validateHex <- function(string, shorthand = FALSE, ignore.case = FALSE){
  if(shorthand){
    grepl("^#([0-9A-F]{3}){1,2}$", string, ignore.case = ignore.case)
  }else{
    grepl("^#([0-9A-F]{6})$", string, ignore.case = ignore.case)
  }
}


#' Validate an R Base color representation.
#'
#' `validateBaseColor` returns if a string vector contains color representations contained in the R Base colors.
#'
#' This function validates a given R Base color representation.
#' It validates the input against the [grDevices::colors()] function.
#'
#' @param string String vector.
#'
#' @return Returns a logical vector of the same length as the \code{string} input.
#'
#' @examples
#' validateBaseColor("red")
#' validateBaseColor("RED")
#'
#' validateBaseColor(c("red", "blue"))
#'
validateBaseColor <- function(string){
  string %in% grDevices::colors()
}


#' Validate an RColorBrewer representation.
#'
#' `validateRColorBrewer` returns if a string vector contains color representations contained in the RColorBrewer package.
#'
#' This function validates a given RColorBrewer representation.
#' It validates the input against the [RColorBrewer::brewer.pal.info] method.
#'
#' @param string String vector.
#'
#' @return Returns a logical vector of the same length as the \code{string} input.
#'
#' @examples
#' validateRColorBrewer("RdBu")
#' validateRColorBrewer("RED")
#'
#' validateRColorBrewer(c("BrBG", "blue"))
#'
validateRColorBrewer <- function(string){
  string %in% rownames(RColorBrewer::brewer.pal.info)
}


#' Validate a viridis color representation.
#'
#' `validateViridis` returns if a string vector contains color representations contained in the viridis package.
#'
#' This function validates a given viridis representation. Valid inputs are \emph{viridis, magma, plasma, inferno} or \emph{cividis}.
#' They are part of the [viridis] package. More information can be found here: [viridisLite::viridis].
#'
#' @param string String vector.
#'
#' @return Returns a logical vector of the same length as the \code{string} input.
#'
#' @examples
#' validateViridis("viridis")
#' validateViridis("RED")
#'
#' validateViridis(c("magma", "plasma"))
#'
validateViridis <- function(string){
  string %in% c("viridis", "magma", "plasma", "inferno", "cividis")
}


#' Create correally correlation plot
#'
#' This function creates an interactive visualization of a correlation matrix. It is based on the R implementation of \href{http://rstudio.com}{plotly.js},
#' an (MIT licensed) web-based interactive charting library. It provides a clean implementation for plotting correlation matrices, easy use of different
#' color schemes (Base R colors, Hex colors, [RColorBrewer] palettes as well as support for [viridisLite]). It also provides coloring options for backgrounds, grids and fonts
#' matching different [shinythemes].
#'
#'
#'
#' @param data A matrix or data frame containing a correlation matrix. Must be numeric and symmetric.
#' @param mask A logical scalar. If \emph{TRUE}, removes the everything above and including the diagonal. Defaults to \emph{FALSE}.
#' @param color Either a vector of Base R colors (e.g. "red", "blue"), a vector of colors in hexadecimal #RRGGBB# format,
#' a colorbrewer2.org palette name (e.g. "YlOrRd", "BrBG") or a viridis palette name (e.g. "viridis", "magma", "inferno").
#' Defaults to "RdBu".
#' @param theme A character string of length 1 defining the name of a [shinythemes] template. Formatting will be applied to the background,
#' gridline color, colorbar framing and font color. Defaults to the "default" theme.
#' @param width Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @param keepRatio A logical scalar. If \emph{TRUE}, keeps a 1:1 ratio between x and y axis size. Defaults to \emph{TRUE}.
#' @param showGrid A logical scalar. If \emph{TRUE}, displays gridlines. Defaults to \emph{TRUE}.
#'
#' @return Returns a plotly object.
#'
#' @examples
#' data = cor(mtcars[,c(1,3:7)])
#'
#' correallyPlot(data)
#' correallyPlot(data, mask = TRUE)
#'
#' correallyPlot(data, mask = TRUE, color = "viridis", theme = "slate")
#'
#' correallyPlot(data, mask = TRUE, width = 700, height = 500, keepRatio = FALSE, showGrid = FALSE)
#'
correallyPlot = function(data,
                         mask = FALSE,
                         color = "RdBu",
                         theme = "default",
                         width = NULL,
                         height = NULL,
                         keepRatio = TRUE,
                         showGrid = TRUE){


  #check if data is a matrix or a data frame
  if(!is.matrix(data)){
    if(is.data.frame(data)){
      data = as.matrix(data)
    }else{
      stop("Invalid data input")
    }
  }

  #check if data is symmetric
  if(!isSymmetric(data)){
    stop("Data input is not symmetric")
  }

  #masking
  if(mask){
    data[upper.tri(data, diag = TRUE)] <- NA
    data <- data[-1, -ncol(data)]
  }

  #define x/y labels
  x_labels <- colnames(data)
  y_labels <- rownames(data)

  #set row/colnames to numeric to create value grid
  colnames(data) <- 1:ncol(data)
  rownames(data) <- nrow(data):1

  #gridlines grid
  if(length(x_labels) > 2){
    x_grid <- seq(1.5, length(x_labels)-0.5, 1)
    y_grid <- seq(1.5, length(y_labels)-0.5, 1)
  }else{
    x_grid <- c(-1, 1.5)
    y_grid <- c(-1, 1.5)
  }

  #x/y axis range
  xrange <- c(0.5, length(x_labels)+0.5)
  yrange <- c(0.5, length(y_labels)+0.5)

  #color scheme
  if(length(color) == 1){
    if(correally::validateViridis(color)){
      color = getFromNamespace(color, "viridisLite")(15)
    }else if(correally::validateHex(color) | correally::validateBaseColor(color) | correally::validateRColorBrewer(color)){
      color = color
    }else{
      stop("Invalid color")
    }
  }else if(length(color)>1){
    if(all(correally::validateHex(color)) | all(correally::validateBaseColor(color))){
      color = color
    }else{
      stop("Invalid color")
    }
  }

  #shiny themes
  if(theme == "default"){
    primaryColor <- "#212529"
    bgColor <- "rgba(0, 0, 0, 0.03)"
    gridColor <- "rgba(0, 0, 0, 0.125)"
  }else if(theme == "slate"){
    primaryColor <- "#c8c8c8"
    bgColor <- "#1c1e22"
    gridColor <- "#333"
  }else if(theme == "cerulean"){
    primaryColor <- "#555555"
    bgColor <- "#f5f5f5"
    gridColor <- "#ccc"
  }else if(theme == "darkly"){
    primaryColor <- "#ffffff"
    bgColor <- "#303030"
    gridColor <- "#999"
  }else if(theme == "cosmo"){
    primaryColor <- "#333333"
    bgColor <- "#f5f5f5"
    gridColor <- "#ccc"
  }else if(theme == "cyborg"){
    primaryColor <- "#888888"
    bgColor <- "#151515"
    gridColor <- "#999"
  }else{
    primaryColor <- "#212529"
    bgColor <- "rgba(0, 0, 0, 0.03)"
    gridColor <- "rgba(0, 0, 0, 0.125)"
  }


  #sizing & scaling
  if(is.null(width)){
    width = dev.size("px")[1]
  }
  if(is.null(height)){
    height = dev.size("px")[2]
  }
  scaling <- min(width, height) / ncol(data) / 2

  #transform data
  plotdata <- reshape2::melt(data)
  plotdata$size <- (abs(plotdata$value)) + 0.1
  plotdata <- plotdata[!is.na(plotdata$value),]
  plotdata$size[plotdata$size < 0.3] <- 0.3
  plotdata$size <- plotdata$size * scaling

  #define plot
  fig <- plotly::plot_ly(data = plotdata, width = width, height = height)
  #add main trace
  fig <- fig %>% plotly::add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                                   color = ~value,
                                   colors = color,
                                   marker = list(size = ~size, opacity = 1, line = list(color = primaryColor, width = 1)),
                                   symbol = I("square"),
                                   text = ~value,
                                   hovertemplate = "%{text:.2f} <extra></extra>",
                                   xaxis = "x1",
                                   yaxis = "y1")

  #define axes
  xAx1 <- list(showgrid = FALSE,
               showline = FALSE,
               zeroline =  FALSE,
               tickvals = colnames(data),
               ticktext = x_labels,
               title = "",
               range = xrange,
               rangemode = "tozero",
               tickfont = list(color = primaryColor))

  xAx2 <- list(showgrid = TRUE,
               gridcolor = gridColor,
               showline = FALSE,
               zeroline =  FALSE,
               overlaying = "x",
               showticklabels = FALSE,
               range = xrange,
               tickvals = x_grid)

  yAx1 <- list(autoaxis = FALSE,
               showgrid = FALSE,
               showline = FALSE,
               zeroline =  FALSE,
               tickvals = rownames(data),
               ticktext = y_labels,
               title = FALSE,
               rangemode = "tozero",
               range = yrange,
               tickfont = list(color = primaryColor))

  yAx2 <- list(showgrid = TRUE,
               gridcolor = gridColor,
               showline = FALSE,
               zeroline =  FALSE,
               overlaying = "y",
               showticklabels = FALSE,
               range = yrange,
               tickvals = y_grid)


  #keepRatio scaling
  if(keepRatio){
    xAx1[["scaleanchor"]] <- "y"
    xAx1[["saleratio"]] <- 1
    xAx1[["constrain"]] <- "domain"

    xAx2[["scaleanchor"]] <- "y2"
    xAx2[["saleratio"]] <- 1
    xAx2[["constrain"]] <- "domain"

    yAx1[["scaleanchor"]] <- "x"
    yAx1[["saleratio"]] <- 1
    yAx1[["constrain"]] <- "domain"

    yAx2[["scaleanchor"]] <- "x2"
    yAx2[["saleratio"]] <- 1
    yAx2[["constrain"]] <- "domain"
  }

  #axes and background layout
  fig <- fig %>% plotly::layout(xaxis = xAx1,
                                yaxis = yAx1,
                                plot_bgcolor = "rgba(0,0,0,0)",
                                paper_bgcolor = bgColor)

  #add gridlines
  if(showGrid){
    #add invisible trace for gridlines
    fig <- fig %>% plotly::add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                                     opacity = 0,
                                     showlegend = FALSE,
                                     xaxis = "x2",
                                     yaxis = "y2",
                                     hoverinfo = "none")

    #extend layout
    fig <- fig %>% plotly::layout(xaxis2 = xAx2,
                                  yaxis2 = yAx2)
  }

  #colorbar
  fig <- fig %>% plotly::colorbar(title = "", limits = c(-1,1), x = 1.1, y = 0.75,
                                  outlinecolor = primaryColor, tickcolor = primaryColor, tickfont = list(color = primaryColor))
  fig
}
