
Summary<-function (x, na.rm = TRUE)
{
	options(digits=2)
    NonNA <- function(x) { sum(!is.na(x)) }
	uni <- function(x) { length(unique(x)) }
	Miss  <- function(x) { abs(length(x)-NonNA(x)) }
	Quant1<-function(x,probs=.25,na.rm=TRUE)
	{
	xx<-quantile(x,probs,na.rm=na.rm)
	names(xx)<-NULL
	xx
	}
	Quant2<-function(x,probs=.75,na.rm=TRUE)
	{
	xy<-quantile(x,probs,na.rm=na.rm)
	names(xy)<-NULL
	xy
	}
	if (!na.rm) x <- na.omit(x)
    if (is.null(dim(x)[2])) {
    	stats 		 <- matrix(rep(NA, 12), ncol = 12)
		rownames(stats) <- colnames(x)
		stats[1, 1]  <- length(x)
		stats[1, 2]  <- uni(x)
		stats[1, 3]  <- Miss(x)
		stats[1, 4]  <- NonNA(x)
        stats[1, 8]  <- mean(x, na.rm = na.rm)
		stats[1, 11] <- quantile(x, 0.98)
        stats[1, 9]  <- median(x, na.rm = na.rm)
        stats[1, 5]  <- min(x, na.rm = na.rm)
        stats[1, 12] <- max(x, na.rm = na.rm)
		stats[1, 6]  <- quantile(x, 0.02)
        stats[1, 7]  <- Quant1(x)
		stats[1, 10] <- Quant2(x)
	   }   else  {
        stats = matrix(rep(NA, ncol(x) * 12), ncol = 12)
        rownames(stats) <- colnames(x)
		stats[, 1]   <- apply(x, 2, length)
		stats[, 2]   <- apply(x, 2, uni)
		stats[, 3]   <- apply(x, 2, Miss)
		stats[, 4]   <- apply(x, 2, NonNA)
		stats[, 6]  <- apply(mtcars,2,quantile, probs =0.02)
        stats[, 8]   <- apply(x, 2, mean, na.rm = na.rm)
        stats[, 12]  <- apply(x, 2, max, na.rm = na.rm)
        stats[, 9]   <- apply(x, 2, median, na.rm = na.rm)
        stats[, 5]   <- apply(x, 2, min, na.rm = na.rm)
 		stats[, 11]  <- apply(mtcars,2,quantile, probs =0.98)
        stats[, 7]   <- apply(x, 2, Quant1)
		stats[, 10]   <- apply(x, 2, Quant2)
	}
	colnames(stats)<-c("n","Unique","Missing","NonNA","Min.","P2%","Q1","Mean","Median","Q3","P98%","Max.")
	stats
}
