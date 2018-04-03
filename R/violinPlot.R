violinPlot =function (...,  grownage = 10, 
    add = FALSE, axes = TRUE, handlelog = NA, wd = NA, maxwidth = 0.8, names, 
    horizontal = FALSE, side = "no", 
    frame.plot = axes, border = NULL, innerborder = NA, at = NULL, 
    boxwex = 1, ylim = NULL, xlim = NULL, show.names = NA) 
{
  ###############
  ##Help-Functions
  ################
  cutmin = -Inf
  cutmax = Inf
  cut = 3
  `violinPlotpolyshapes` <-
    function (side, dens, at, wd2, combinedpolygons, displayn, n, 
              col, border, horizontal, mlog, mexp) 
    {
      if ((side == 4) && (!combinedpolygons)) 
        at <- rep(at, each = 2)
      pborder <- NA
      #print('this')
      for (i in 1:ifelse(combinedpolygons, displayn, n)) {
        if (combinedpolygons) {
          x1 <- rev(dens[["y", i * 2]] * wd2) + at[i]
          x2 <- dens[["y", i * 2 - 1]] * -wd2 + at[i]
          y1 <- rev(dens[["x", i * 2]])
          y2 <- dens[["x", i * 2 - 1]]
          #print('this123')  
        }
        else {
          if (side == 2 || ((side == 4) && (i%%2 == 1))) {
            x1 <- NULL
            y1 <- NULL
            #print('this2') 
          }
          else {
            x1 <- dens[["y", i]] * wd2 + at[i]
            y1 <- dens[["x", i]]
            #print('this3')
          }
          if (side == 3 || ((side == 4) && (i%%2 == 0))) {
            x2 <- NULL
            y2 <- NULL
            #print('this4')
          }
          else {
            x2 <- rev(dens[["y", i]]) * -wd2 + at[i]
            y2 <- rev(dens[["x", i]])
            #print('this5')
          }
        }
        if (length(x1) > 0) {
          x1 <- c(at[i], x1, at[i])
          y1 <- c(y1[1], y1, y1[length(y1)])
          #print('this6')
        }
        if (length(x2) > 0) {
          x2 <- c(at[i], x2, at[i])
          y2 <- c(y2[1], y2, y2[length(y2)])
         # print('this7')
        }
        if (combinedpolygons || (side == 1)) 
          pborder <- border[[i]]
        #print('test')
        
        #return(list(y1,y2,x1,x2))
        if (horizontal) {
          polygon(mexp(c(y1, y2)), c(x1, x2), col = col[[i]][1], 
                  border = pborder)
          if ((!combinedpolygons) && (side != 1)) {
            if (is.null(border[[i]])) 
              lines(mexp(c(y1, y2)), c(x1, x2))
            else if (!is.na(border[[i]])) 
              lines(mexp(c(y1, y2)), c(x1, x2), col = border[[i]])
          }
        }
        else {
          polygon(c(x1, x2), mexp(c(y1, y2)), col = col[[i]][1], 
                  border = pborder)
          if ((!combinedpolygons) && (side != 1)) {
            if (is.null(border[[i]])) 
              lines(c(x1, x2), mexp(c(y1, y2)))
            else if (!is.na(border[[i]])) 
              lines(c(x1, x2), mexp(c(y1, y2)), col = border[[i]])
          }
        }
      }
    }
  
  `getgroupsfromarguments` <-
    function (args = match.call(sys.function(sys.parent()),sys.call(sys.parent())),
              envir = parent.frame(2)) 
    {
      nextargpos <- function(name, pos) {
        if (any(pos==length(args)))
          return(pos)
        posnext <- match(name, base::names(args[max(pos + 
                                                      1, 3):length(args)])) + max(pos + 1, 3) - 1
        if (!is.na(posnext))
          pos <- posnext
        pos
      }
      if (is.null(base::names(args))) 
        vars <- 1:length(args)
      else vars <- c(1, which(base::names(args)[2:length(args)] %in% 
                                c("formula", "x", "data", "")) + 1)
      if (length(vars) < 2) 
        return(list())
      options <- which(base::names(args) %in% c("subset", "na.action", 
                                                "drop.unused.levels", "xlev"))
      args <- args[c(vars, options)]
      args[[1]] <- quote(model.frame)
      hashad <- rep(FALSE, length(vars))
      groups <- list()
      notnamed <- 0
      subsetno <- numeric()
      naactno <- numeric()
      dulno <- numeric()
      xlevno <- numeric()
      datano <- numeric()
      argsvals <- lapply(as.list(args[2:length(vars)]), eval, envir)
      islists <- lapply(argsvals, function(x){is.list(x)||is.null(x)})
      for (i in 2:length(vars)) {
        if (hashad[i]) 
          next
        x <- argsvals[[i - 1]]
        if (inherits(x, "formula")) {
          datanonext <- match(TRUE, islists[max(datano, i):length(islists)]) + 
            max(datano, i)
          if (!is.na(datanonext)) {
            hashad[datanonext] <- TRUE
            datano <- datanonext
          }
          subsetno <- nextargpos("subset", subsetno)
          naactno <- nextargpos("na.action", naactno)
          dulno <- nextargpos("drop.unused.levels", dulno)
          xlevno <- nextargpos("xlev", xlevno)
          attr(args, "names")[i] <- "formula"
          m <- args[c(1, i, datano, subsetno, naactno, dulno, xlevno)]
          mf <- eval(m, envir)
          response <- attr(attr(mf, "terms"), "response")
          groups <- c(groups, split(mf[[response]], mf[-response]))
        }
        else if (is.list(x)) {
          groups <- c(groups, x)
        }
        else {
          x <- list(x)
          notnamed <- notnamed + 1
          attr(x, "names") <- notnamed
          groups <- c(groups, x)
        }
      }
      groups
    }
  
  #############################
  `makecombinedname` <-
    function(string1,string2) {
      if (is.na(string2))
        return(string1)
      if (string1==string2)
        return(string1)
      s1 <- unlist(strsplit(string1,NULL))
      s2 <- unlist(strsplit(string2,NULL))
      fromleft <- 0
      gfromleft <- 0
      while ((fromleft<length(s1)) && (fromleft<length(s2)) && (s1[fromleft+1]==s2[fromleft+1])) {
        if (any(grep("[. \t]",s1[fromleft+1])))
          gfromleft<-fromleft
        fromleft<-fromleft+1
      }
      fromright <- 0
      gfromright <- 0
      while ((fromright<length(s1)) && (fromright<length(s2)) && (s1[length(s1)-fromright]==s2[length(s2)-fromright])) {
        if (any(grep("[. \t]",s1[length(s1)-fromright])))
          gfromright<-fromright
        fromright<-fromright+1
      }
      if (gfromleft>gfromright)
        result<-substr(string1,1,gfromleft)
      else if (gfromright==0)
        result<-paste(string1,string2,sep="+")
      else
        result<-substr(string1,nchar(string1)-gfromright+1,nchar(string1))
      result
    }
    #internal functions (later on, mlog and mexp will be defined)
    mdensityxy <- function(x) {
        if (length(x) > 0) {
            # from <- max(cutmin, (min(mlog(x)) - cut * bw))
            # to <- min(cutmax, max(mlog(x)) + cut * bw)
            # density(mlog(x), bw = bw, from = from,
            #     to = to)[c("x", "y")]
          dens=AdaptGauss::ParetoDensityEstimation(x)
          return(list(x=dens$kernels,
          y=dens$paretoDensity))
        }
        else list(x = numeric(), y = numeric())
    }
    log = ""
    #get and store function arguments
    args <- match.call()
    mcall <- as.list(args)

    #settings with multiple options
    # method <- pmatch(method, c("overplot", "stack", "jitter"))
    # if (is.na(method) || method == 0) 
    #     stop("invalid plotting method")
    # beanlines <- pmatch(beanlines, c("mean", "median", "quantiles"))
    # if (is.na(beanlines) || beanlines == 0) 
    #     stop("invalid beanlines")
    # overallline <- pmatch(overallline, c("mean", "median"))
    # if (is.na(overallline) || overallline == 0) 
    #     stop("invalid overallline")
    side <- pmatch(side, c("no", "first", "second", "both"))
    if (is.na(side) || side == 0) 
        stop("invalid side")

    #get the groups dataset, we will generate one bean(-side) per group
    #and set the name and position settings
    groups <- getgroupsfromarguments(args)
    groups <- lapply(groups, na.omit)
    n <- length(groups)
    displayn <- if (side == 4) 
        ceiling(n/2)
    else n
    if (n == 0) 
        stop("no data found to violinPlot")
    if (missing(names)) {
        if (is.null(base::names(groups))) 
            attr(groups, "names") = 1:displayn
        names <- base::names(groups)
    }
    else {
        attr(groups, "names") <- names
        if (is.na(show.names)) 
            show.names <- TRUE
    }
    if (is.null(at)) {
        at <- 1:displayn
    }
    if ((side == 4) && (length(names) > length(at))) {
        for (i in 1:length(at)) {
            names[i] <- makecombinedname(names[i * 2 - 1], names[i * 
                2])
        }
        length(names) <- length(at)
    }

    #color settings
    combinedpolygons <- ((side == 4) && (length(border) < 2) && 
        (n%%2 == 0))

        col <- par("fg")
   
	if (!is.null(border))
        border <- rep(border, length.out = n)

    if (is.na(handlelog)) 
        if (add && ((horizontal & par()$xlog) || (!horizontal & 
            par()$ylog))) 
            handlelog <- TRUE
        else if (!add && (log != "")) 
            handlelog <- TRUE
        else handlelog <- FALSE
    if (handlelog) {
        mlog <- base::log
        mexp <- base::exp
    }
    else {
        mlog <- function(x) {
            x
        }
        mexp <- mlog
    }

    #generate the necessary data for the density shapes from the group data
    # if (!is.numeric(bw)) {
    #     bw <- mean(sapply(groups, function(x) {
    #         ifelse(length(x) > 1, density(mlog(x), kernel = kernel,
    #             bw = bw)$bw, NA)
    #     }), na.rm = TRUE)
    #     if (is.nan(bw))
    # bw <- 0.5
    # }
   
    #dens <- sapply(1:ncol(groups$`1`),FUN = function(i,groups) return(mdensityxy(groups$`1`[,i])),groups)
        
    dens <- sapply(groups, mdensityxy)
    #return(dens)
    for (i in 1:n) dens[["y", i]] <- dens[["y", i]] * min(1, 
        length(groups[[i]])/grownage)
    if (is.na(wd)) 
        wd <- maxwidth/max(unlist(dens["y", ]))
    wd2 <- wd * boxwex/2

    #plot windows and axes
    axespars <- lapply(mcall[base::names(mcall) %in% c("xaxt", 
        "yaxt", "las", "cex.axis", "col.axis", "format", "tick", 
        "xaxp", "yaxp")], eval, parent.frame())
    if (!add) {
        if (!is.numeric(xlim)) {
            if (side == 2) 
                xlim <- c(0, displayn)
            else if (side == 3) 
                xlim <- c(1, displayn + 1)
            else xlim <- c(0.5, displayn + 0.5)
        }
        if (!is.numeric(ylim)) 
            ylim <- range(groups, mexp(unlist(dens["x", ])))
        plot.new()
        windowpars <- lapply(mcall[base::names(mcall) %in% c("yaxs", 
            "xaxs")], eval)
        if (horizontal) {
            names(windowpars)[names(windowpars) %in% c("xaxs", 
                "yaxs")] <- rev(names(windowpars)[names(windowpars) %in% 
                c("xaxs", "yaxs")])
            if (log == "y") 
                log <- "x"
            do.call("plot.window", c(list(xlim = ylim, ylim = xlim, 
                log = log), windowpars))
        }
        else {
            do.call("plot.window", c(list(xlim = xlim, ylim = ylim, 
                log = log), windowpars))
        }
        if (frame.plot) 
            box()
        if (axes) 
            do.call("axis", c(list(side = 2 - horizontal), axespars))
    }
    if (axes) {
        if (is.na(show.names)) 
            show.names <- (n > 1)
        if (show.names) 
            do.call("axis", c(list(1 + horizontal, at = at, labels = names), 
                axespars))
    }
 #print('test12')
#return(list(dens,at,wd2))
    xxx=violinPlotpolyshapes(side, dens, at, wd2, combinedpolygons, 
            displayn, n, col, border, horizontal, mlog, mexp)
    #polygon(c(x1, x2)
#return(list(dens,at,wd,xxx))
    #finally, prints labels
    titlepars <- lapply(mcall[base::names(mcall) %in% c("main", 
        "sub", "xlab", "ylab", "cex.main", "col.main", "cex.lab", 
        "col.lab", "cex.sub", "col.sub")], eval, parent.frame())
    do.call("title", titlepars)

    #return generated data that can be used for subsequent calls
    invisible(list(wd = wd, names = names))
}