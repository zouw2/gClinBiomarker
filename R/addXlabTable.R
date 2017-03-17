addXlabTable <- function (mat, merge = TRUE, bmargin = 0.025, Label = list(), 
          Text = list()) 
{
  stopifnot(is.logical(merge))
  stopifnot(is.matrix(mat))
  stopifnot(is.numeric(bmargin))
  stopifnot(0 <= bmargin && bmargin <= 1)
  old.par <- par(xpd = TRUE)
  usr <- par("usr")
  fig <- par("fig")
  plt <- par("plt")
  dxp <- plt[2] - plt[1]
  dyp <- plt[4] - plt[3]
  dxu <- usr[2] - usr[1]
  dyu <- usr[4] - usr[3]
  fuc <- numeric(4)
  fuc[1] <- usr[2] - dxu * plt[2]/dxp
  fuc[2] <- fuc[1] + (usr[2] - fuc[1])/plt[2]
  fuc[3] <- usr[4] - dyu * plt[4]/dyp
  fuc[4] <- fuc[3] + (usr[4] - fuc[3])/plt[4]
  l <- usr[1]
  r <- usr[2]
  u <- usr[3]
  b <- fuc[3] + bmargin * diff(fuc[3:4])
  w <- abs(diff(c(l, r)))
  h <- abs(diff(c(u, b)))
  rh <- h/nrow(mat)
  cw <- w/ncol(mat)
  tmp.Text <- list()
  tmp.Text[names(Text)] <- Text
  Text <- tmp.Text
  tmp.l <- NA
  for (i in 1:(nrow(mat) + 1)) {
    lines(c(l, r), rep(u - (i - 1) * rh, 2))
    for (j in 1:(ncol(mat) + 1)) {
      if (i == nrow(mat) + 1) 
        next
      if (j %in% c(1, ncol(mat) + 1)) {
        lines(rep(l + (j - 1) * cw, 2), c(u - (i - 1) * 
                                            rh, u - i * rh))
      }
      else {
        if (mat[i, j - 1] != mat[i, j] || !merge) {
          swit <- TRUE
          lines(rep(l + (j - 1) * cw, 2), c(u - (i - 
                                                   1) * rh, u - i * rh))
        }
      }
      if (j < ncol(mat) + 1) {
        if (j == 1) 
          tmp.l <- l
        if (merge) {
          if (j < ncol(mat)) {
            if (mat[i, j] != mat[i, j + 1]) {
              tmp.r <- l + j * cw
              Text$x <- mean(c(tmp.l, tmp.r))
              Text$y <- mean(c(u - (i - 1) * rh, u - 
                                 i * rh))
              Text$labels <- mat[i, j]
              Text$adj <- c(0.5, 0.5)
              do.call("text", Text)
              tmp.l <- tmp.r
            }
          }
          else {
            tmp.r <- l + j * cw
            Text$x <- mean(c(tmp.l, tmp.r))
            Text$y <- mean(c(u - (i - 1) * rh, u - i * 
                               rh))
            Text$labels <- mat[i, j]
            Text$adj <- c(0.5, 0.5)
            do.call("text", Text)
          }
        }
        else {
          Text$x <- l + (j - 1) * cw + cw/2
          Text$y <- mean(c(u - (i - 1) * rh, u - i * 
                             rh))
          Text$labels <- mat[i, j]
          Text$adj <- c(0.5, 0.5)
          do.call("text", Text)
        }
      }
    }
  }
  tmp.Label <- list(x = l - 0.01 * (fuc[2] - fuc[1]), y = seq(from = b + 
                                                                rh/2, by = rh, length.out = nrow(mat)), labels = rownames(mat), 
                    adj = c(1, 0.5))
  tmp.Label[names(Label)] <- Label
  Label <- tmp.Label
  do.call("text", Label)
  par(old.par)
}