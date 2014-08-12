
RED = 1
BLUE = 2
EMPTY = 3


numNeighborsOfColor = function(city, row, col, color) {
    num = 0
    if (row > 1 && col > 1 && city[row-1,col-1] == color) {
        num = num + 1
    }
    if (col > 1 && city[row,col-1] == color) {
        num = num + 1
    }
    if (row < height && col > 1 && city[row+1,col-1] == color) {
        num = num + 1
    }
    if (row < height && city[row+1,col] == color) {
        num = num + 1
    }
    if (row < height && col < width && city[row+1,col+1] == color) {
        num = num + 1
    }
    if (col < height && city[row,col+1] == color) {
        num = num + 1
    }
    if (row > 1 && col < height && city[row-1,col+1] == color) {
        num = num + 1
    }
    return(num)
}

numRedNeighbors = function(city, row, col) {
    return(numNeighborsOfColor(city, row, col, RED))
}

numBlueNeighbors = function(city, row, col) {
    return(numNeighborsOfColor(city, row, col, BLUE))
}

computeRatio = function(mycolor, city, row, col) {
    nred = numRedNeighbors(city, row, col)
    nblue = numBlueNeighbors(city, row, col)
    if (nred + nblue == 0) {
        return(1)
    }
    if (mycolor == RED) {
        ratio = nred/(nred+nblue)
    } else {
        ratio = nblue/(nred+nblue)
    }
    return(ratio)
}

belowThresh = function(mycolor, city, row, col, thresh) {

    ratio = computeRatio(mycolor, city, row, col)
    if (ratio < thresh) {
        return(T)
    } else {
        return(F)
    }
}

pointsForGrid = function(grid,val) {
    xcoords = vector()
    ycoords = vector()
    for (row in 1:nrow(grid)) {
        for (col in 1:ncol(grid)) {
            if (grid[row,col] == val) {
                xcoords[length(xcoords)+1] = col
                ycoords[length(ycoords)+1] = nrow(grid) - row + 1
            }
        }
    }
    return(list(xcoords,ycoords))
}

findRandomEmpty = function(city) {
    empties = which(city == EMPTY, arr.ind=T)
    return(empties[sample.int(nrow(empties),1),])
}

computeAvgRatio = function(city) {
    ratioSum = 0
    numNonEmpty = 0
    for (row in 1:nrow(city)) {
        for (col in 1:ncol(city)) {
            if (city[row,col] != EMPTY) {
                numNonEmpty = numNonEmpty + 1
                ratioSum = ratioSum +
                    computeRatio(city[row,col],city,row,col)
            }
        }
    }
    return (ratioSum / numNonEmpty)
}


# Main simulation.
run.sim <- function(probRed=.3,probBlue=.3,thresh=.6,width=50,height=50,
    numGen=40, progress) {

    height <<- height
    width <<- width 

    starter = runif(width * height)
    config = matrix(ifelse(starter < probRed, RED, 
        ifelse(starter < probRed + probBlue, BLUE,
        EMPTY)),nrow=height)
    num.reds <- length(which(config == RED))
    num.blues <- length(which(config == RED))
    num.total <- num.reds + num.blues

    city = array(dim=c(width,height,numGen))
    city[,,1] = config

    plots <- list()
    num.swaps <- numeric(length=numGen)
    for (gen in 2:numGen) {
        setProgress(paste0("Simulating generation ", gen, "..."),val=gen)
        reds = pointsForGrid(city[,,gen-1],RED)
        blues = pointsForGrid(city[,,gen-1],BLUE)
        the.data <- rbind(
                data.frame(x=reds[[1]],y=reds[[2]],col="red"),
                data.frame(x=blues[[1]],y=blues[[2]],col="blue"))
        plots[[gen-1]] <- ggplot(the.data, aes(x=x,y=y,color=col,shape=col)) + 
            geom_point(show_guide=FALSE, size=4.0) +
            ggtitle(paste("Generation",gen-1)) +
            scale_color_manual(values=c("#FF0000","#0000FF")) +
            scale_shape_manual(values=c(19,15)) +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  legend.position="none",
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        
        city[,,gen] = city[,,gen-1]

        for (row in seq(height)) {
            for (col in seq(width)) {
                curval = city[row,col,gen-1]
                if(curval != EMPTY  &&  
                        belowThresh(curval, city[,,gen-1],row,col,thresh)) {
                    city[row,col,gen] = EMPTY
                    randEmpty = findRandomEmpty(city[,,gen])
                    city[randEmpty[1],randEmpty[2],gen] = curval
                    num.swaps[[gen]] <- num.swaps[[gen]] + 1
                }
            }
        }
    }
    list(plots=plots, num.swaps=num.swaps, perc.swaps=num.swaps/num.total)
}

