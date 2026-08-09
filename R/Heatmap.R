 Heatmap=function(DataOrDistances,Cls,method='euclidean',LowLim=0,HiLim,LineWidth=0.5,Clabel="Cluster No.",
                  YAxis = c("auto", "none"),
                  PlotIt = TRUE){
  # Heatmap(DataOrDistances,Cls,method) 
  # Heatmap: Distances of DataOrDistances sorted by Cls
  # INPUT
  # DataOrDistances                    [1:n,1:d] data cases in rows, variables in columns oder [1:n,1:n] distances
  # OPTIONAL
  # Cls                   numeric vector, [1:n,1]  classified data 
  # Distanz               see DistanceMatrix(...,method):
  #
  # LowLim,HiLim  limits for the color axis as in PlotPixMatrix
  # author: MT 08/2016, edited 28.01.2018
  #2.Editor: MT 06/18
  #3.Editor: 07/2020 because of reviews in GMD journal
  #4. editor; MT 08:2026
   
   YAxis = match.arg(YAxis)
   
   # ------------------------------------------------------------
   # Eingaben prüfen
   # ------------------------------------------------------------
   
   if (!is.matrix(DataOrDistances)) {
     message("DataOrDistances ist keine Matrix. as.matrix() wird verwendet.")
     DataOrDistances = as.matrix(DataOrDistances)
   }
   
   if (!is.numeric(DataOrDistances)) {
     warning(
       "DataOrDistances ist nicht numerisch. ",
       "storage.mode(DataOrDistances) = 'double' wird verwendet."
     )
     storage.mode(DataOrDistances) = "double"
   }
   
   n = nrow(DataOrDistances)
   
   if (missing(Cls)) {
     Cls = rep.int(1L, n)
   }
   
   if (length(Cls) != n) {
     stop("length(Cls) muss gleich nrow(DataOrDistances) sein.")
   }
   
   # Nach Cluster sortieren
   ord = order(Cls, decreasing = FALSE, na.last = TRUE)
   
   # check.attributes = FALSE verhindert, dass unterschiedliche
   # Zeilen- und Spaltennamen eine symmetrische Matrix fälschlich
   # als nicht symmetrisch klassifizieren.
   isDistanceMatrix =
     ncol(DataOrDistances) == n &&
     isTRUE(isSymmetric(DataOrDistances, check.attributes = FALSE))
   
   # ------------------------------------------------------------
   # Distanzmatrix erzeugen beziehungsweise sortieren
   # ------------------------------------------------------------
   
   if (isDistanceMatrix) {
     DataDists = DataOrDistances[ord, ord, drop = FALSE]
     
   } else {
     message("Datenmatrix erkannt; paarweise Distanzen werden berechnet.")
     
     # Nur einmal sortieren
     SortedData = DataOrDistances[ord, , drop = FALSE]
     
     if (requireNamespace("parallelDist", quietly = TRUE)) {
       DataDists = as.matrix(parallelDist::parDist(SortedData, method = method))
       
     } else {
       message("Paket 'parallelDist' ist nicht installiert; ",
               "stats::dist() wird verwendet.")
       
       DataDists = as.matrix(stats::dist(SortedData, method = method, diag = TRUE))
     }
   }
   
   # ------------------------------------------------------------
   # Farbgrenzen prüfen
   # ------------------------------------------------------------
   
   if (missing(HiLim)) {
     HiLim = max(DataDists, na.rm = TRUE)
   }
   
   isScalarNumber = function(x) {
     is.numeric(x) &&
       length(x) == 1L &&
       is.finite(x)
   }
   
   if (!isScalarNumber(HiLim)) {
     stop("'HiLim' muss eine einzelne endliche numerische Zahl sein.")
   }
   
   if (!isScalarNumber(LowLim)) {
     stop("'LowLim' muss eine einzelne endliche numerische Zahl sein.")
   }
   
   if (HiLim <= LowLim) {
     warning("HiLim muss größer als LowLim sein; ",
             "HiLim wird auf LowLim + 0.1 gesetzt.")
     HiLim = LowLim + 0.1
   }
   
   # ------------------------------------------------------------
   # Clustergrenzen bestimmen
   # ------------------------------------------------------------
   
   SortedCls = Cls[ord]
   ClusterLevels = unique(SortedCls)
   
   # Schneller als eine Schleife mit sum(Cls == ...)
   ClusterCounts = tabulate(match(SortedCls, ClusterLevels), nbins = length(ClusterLevels))
   
   if (length(ClusterCounts) > 1L) {
     Separators = head(cumsum(ClusterCounts) + 0.5, -1L)
   } else {
     Separators = numeric(0)
   }
   
   # ------------------------------------------------------------
   # Achsenverhalten
   # ------------------------------------------------------------
   
   if (YAxis == "none") {
     YNamesArgument = NULL
   } else {
     # Verhindert, dass vorhandene rownames vollständig angezeigt
     # werden; ggplot2 wählt stattdessen wenige automatische Ticks.
     YNamesArgument = seq_len(n)
   }
   
   # ------------------------------------------------------------
   # Plot
   # ------------------------------------------------------------
   
   plt = DataVisualizations::Pixelmatrix(
     DataDists,
     XNames = NULL,
     YNames = YNamesArgument,
     LowLim = LowLim,
     HiLim = HiLim,
     main = ""
   ) +
     ggplot2::labs(x = NULL, y = NULL) +
     ggplot2::theme(aspect.ratio = 1)
   
   if (length(Separators) > 0L) {
     # Nur zwei Layer, unabhängig von der Clusterzahl
     plt = plt +
       ggplot2::geom_hline(yintercept = Separators,
                           colour = "black",
                           linewidth = LineWidth) +
       ggplot2::geom_vline(xintercept = Separators,
                           colour = "black",
                           linewidth = LineWidth)
   }
   
   if (isTRUE(PlotIt)) {
     print(plt)
   }
   
   return(invisible(plt))
 }