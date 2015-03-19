# original code submitted by Adrien Remund,
# modified by Tim Riffe

LE <- compiler::cmpfun(function(mx, ax, Widths){
    if (is.null(dim(mx))){
      mx <- as.matrix(mx)
    }
    if (is.null(dim(ax))){
      ax <- as.matrix(ax)
    }
    if (is.null(dim(Widths))){
      Widths <- as.matrix(Widths)
    }
    N             <- nrow(mx)
    
    qx            <- (Widths * mx) / (1 + (Widths - ax) * mx)
    qx[N, ]       <- 1
    qx[qx > 1]    <- 1
    px            <- 1 - qx
    lx            <- apply(px, 2,function(.px,.N){
        c(1,cumprod(.px)[-.N])
      }, .N = N)
    dx            <- lx * qx
    dx[N, ]       <- 1 - colSums(dx[-N, , drop = FALSE])
    Lx            <- rbind(Widths[1:(N - 1), , drop = FALSE] * lx[2:N, , drop = FALSE] + ax[1:(N - 1), , drop = FALSE] * dx[1:(N - 1), , drop = FALSE], 
      lx[N, , drop = FALSE] * ax[N, , drop = FALSE]
    )
    
    Lx[is.infinite(Lx)] <- 1
    Lx[is.na(Lx)] <- 0
    Tx            <- apply(Lx, 2,function(.Lx){
        rev(cumsum(rev(.Lx)))
      })
    ex            <- Tx / lx
    return(ex[1, ])
  })

