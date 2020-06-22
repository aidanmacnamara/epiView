primaryDataDistributionRank=function(w,var="min0",decreasing = T){

  out=w[c("gene",var)]
  colnames(out)[2]="value"
  out[order(out$value,decreasing = decreasing),]
}

# primaryDataDistributionRank(w)[1:2,]
