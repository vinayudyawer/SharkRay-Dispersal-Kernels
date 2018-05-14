### Create dispersal kernel plots for SharkRayMPA project

displot<-function(data, cn, var="dis", xlab="Maximum dispersal distance (km)", ylab="Relative frequency", 
                  breaks=seq(0,log(200000+1), l=30), mar=c(4,4,1,1), bar.col=NA, bar.border=1,
                  xlim=log(c(0,5000)+1), ylim=NULL, lcol=2, dist="gamma", verbose=TRUE,
                  quants=c(0,0.25,0.5,0.75,1), plotit=FALSE, bars=TRUE, lab=TRUE, ...)
  {
  dat<-data.frame(filter(data, common_name%in%cn))
  if(nrow(dat)>3){
    a<-log(dat[dat[,var]>0,var]+1)
    # a<-log(dat$var+1)
    xfit<-seq(0,max(a),length=1000)
    par(mar=mar)
    h<-hist(a, plot=F, breaks=breaks, ...)
    if(dist=="normal"){
      # normal distribution
      yfn<-dnorm(xfit,mean=mean(a),sd=sd(a));
      yfitn<-yfn
      if(is.null(ylim)){yy<-range(0,max(yfitn[is.finite(yfitn)], na.rm=T),max(h$density, na.rm=T))}else{yy=ylim}
      if(plotit){
        if(bars==TRUE){hist(a, breaks=breaks, freq=F, xaxt="n", las=1, main="", ylab=ylab, xlab=xlab, col=bar.col, border=bar.border, xlim=xlim, ylim=yy, ...)}
        else{hist(a, breaks=breaks, freq=F, xaxt="n", las=1, main="", ylab=ylab, xlab=xlab, col=NA, border=NA, xlim=xlim, ylim=yy, ...)}
        axis(1, at= log(c(0.01, seq(0.1,1,l=10), seq(1,10,l=10),seq(10,100,l=10), seq(100,1000,l=10), seq(1000,10000,l=10))+1), labels=F, tcl=-0.3)
        axis(1, at= log(c(0.1,1,10,100,1000,10000)+1), labels=c(0.1,1,10,100,1000,10000))
        lines(xfit, yfitn, col=lcol, lwd=2)}
      out<-data.frame(common_name=cn, fit="normal", mean = mean(dat[dat[,var]>0,var]), sd=sd(dat[dat[,var]>0,var]), shape=NA, scale=NA, q=t(stats::quantile(dat[dat[,var]>0,var], probs=quants)))
    }
    if(dist=="gamma"){
      # gamma distribution
      yfg<-dgamma(xfit,shape=mean(a)^2/var(a),scale=var(a)/mean(a)); 
      yfitg<-yfg
      if(is.null(ylim)){yy<-range(0,max(yfitg[is.finite(yfitg)], na.rm=T),max(h$density, na.rm=T))}else{yy=ylim}
      if(plotit){
        if(bars==TRUE){hist(a, breaks=breaks, freq=F, xaxt="n", las=1, main="", ylab=ylab, xlab=xlab, col=bar.col, border=bar.border, xlim=xlim, ylim=yy, ...)}
        else{hist(a, breaks=breaks, freq=F, xaxt="n", las=1, main="", ylab=ylab, xlab=xlab, col=NA, border=NA, xlim=xlim, ylim=yy, ...)}
        axis(1, at= log(c(0.01, seq(0.1,1,l=10), seq(1,10,l=10),seq(10,100,l=10), seq(100,1000,l=10), seq(1000,10000,l=10))+1), labels=F, tcl=-0.3)
        axis(1, at= log(c(0.1,1,10,100,1000,10000)+1), labels=c(0.1,1,10,100,1000,10000))
        lines(xfit, yfitg, col=lcol, lwd=2)
      }
      out<-data.frame(common_name=cn, fit="gamma", mean = mean(dat[dat[,var]>0,var]), sd=sd(dat[dat[,var]>0,var]), shape=mean(a)^2/var(a), scale=var(a)/mean(a), q=t(stats::quantile(dat[dat[,var]>0,var], probs=quants)))
    }
    if(dist=="kernel"){
      # kernel distribution
      yfk<-density(a, n=1000, adjust=2)$y
      yfitk<-yfk
      if(is.null(ylim)){yy<-range(0,max(yfitk[is.finite(yfitk)], na.rm=T),max(h$density, na.rm=T))}else{yy=ylim}
      if(plotit){
        if(bars==TRUE){hist(a, breaks=breaks, freq=F, xaxt="n", las=1, main="", ylab=ylab, xlab=xlab, col=bar.col, border=bar.border, xlim=xlim, ylim=yy, ...)}
        else{hist(a, breaks=breaks, freq=F, xaxt="n", las=1, main="", ylab=ylab, xlab=xlab, col=NA, border=NA, xlim=xlim, ylim=yy, ...)}
        axis(1, at= log(c(0.01, seq(0.1,1,l=10), seq(1,10,l=10),seq(10,100,l=10), seq(100,1000,l=10), seq(1000,10000,l=10))+1), labels=F, tcl=-0.3)
        axis(1, at= log(c(0.1,1,10,100,1000,10000)+1), labels=c(0.1,1,10,100,1000,10000))
        lines(xfit, yfitk, col=lcol, lwd=2)
      }
      out<-data.frame(common_name=cn, fit="kernel", mean = mean(dat[dat[,var]>0,var]), sd=sd(dat[dat[,var]>0,var]), shape=NA, scale=NA, q=t(stats::quantile(dat[dat[,var]>0,var], probs=quants)))
    }
    if(plotit){
      box(bty="l")
      if(lab){legend('topright', bty="n", pch=NA, col=NA, legend=cn, text.font=4, cex=1.2)}
      }
    }
  else {
    # message("Not enough data")
    out<-data.frame(matrix(ncol=(6+length(quants)), nrow=1))
    names(out)<-c("common_name", "fit","mean","sd","shape","scale",paste0("q.",quants*100,"."))
    out$common_name<-cn
    out$fit<-dist
  }
  if(verbose==TRUE){return(out)}
}
