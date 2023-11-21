marradistree <-
function(m, s, xlab=NULL,ylab=NULL,textv=FALSE,lwd=3,glab="") {
  n <- length(m)
  if (!n) 
        stop("invalid first argument")  

  distanza_minima <- 2 * max(s)
  width <- n + distanza_minima * (n)
  height <- max(m) + 2 * max(s)
            if (is.null(ylab)) 
                ylab <- c("")
            if (is.null(xlab)) 
                xlab <- c("Group")  
                
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  plot(1, type = "n", xaxt="n", xlab = xlab, ylab = ylab, xlim = c(-max(s), width), ylim = c(0, height))

  for (i in 1:n) {
    # tronco
    lines(c(i + distanza_minima * (i-1), i + distanza_minima * (i-1)), c(0, m[i]), col = "brown",lwd=lwd)
    
    # base
    base_length <- max(s)/2
    segments(x0 = i + distanza_minima * (i-1) - base_length/2, x1 = i + distanza_minima * (i-1) + base_length/2, y0 = 0, y1 = 0, col = "brown",lwd=lwd)
    
    # Chioma
    theta <- seq(0, 2 * pi, length.out = 100)
    x <- i + s[i] * cos(theta)
    y <- m[i] + s[i] * sin(theta)
    
    # Trova punto finale del segmento inclinato a -45 gradi
    x_end <- i + s[i] * cos(-pi/4)
    y_end <- m[i] + s[i] * sin(-pi/4)
    
    # Disegna il segmento inclinato
    lines(c(i + distanza_minima * (i-1), x_end + distanza_minima * (i-1)), c(m[i], y_end), col = "brown", lwd=lwd)
    
    # Disegna la circonferenza 
    lines(x + distanza_minima * (i-1), y, col = "darkgreen",lwd=lwd)
    
    # aggiunge media e varianza numeriche
    if(textv==TRUE){
    text(i + distanza_minima * (i-1) + mean(s)/3, max(m) / 3, labels = round(m[i], 2) )  
    text(i + distanza_minima * (i-1) + s[i]/2, m[i], labels = round(s[i], 2) ) 
    }
    # aggiunge etichette dei gruppi in alto
                if (is.null(glab)) 
                glab <- seq_len(n)
     text(i + distanza_minima * (i-1), max(m) + max(s), labels = glab[i], pos = 3)
    }

}
