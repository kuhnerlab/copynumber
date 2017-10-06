####################################################################
## Author: Gro Nilsen, Knut Liest?l and Ole Christian Lingj?rde.
## Maintainer: Gro Nilsen <gronilse@ifi.uio.no>
## License: Artistic 2.0
## Part of the copynumber package
## Reference: Nilsen and Liest?l et al. (2012), BMC Genomics
####################################################################

#Get mad SD-estimate

##Input:
### x: vector of observations for which mad Sd is to be calculated
### k: window size to be used in median filtering

##Output:
### SD: mad sd estimate

##Required by:
### multipcf
### pcf
### aspcf


##Requires:
### medianFilter


getMad <- function(x,k=25){
  
  #Remove observations that are equal to zero; are likely to be imputed, should not contribute to sd:
  x <- x[x!=0]
  
  #Calculate runMedian  
  runMedian <- medianFilter(x,k)
  
  dif <- x-runMedian
  SD <- mad(dif, na.rm = TRUE)
 
	return(SD)
}
