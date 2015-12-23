################################################################################
# FUNCTION - sgems.variog ######################################################
################################################################################
sgems.variog <- function(x,y,pairs,azimuth,dip,omnidir)
{
begin <- '<variogram>'
end <- '</variogram>'
if(omnidir) {title <- paste('<title>variogram - Omni-directional</title>')}
else {title <- paste('<title>variogram - azth=', azimuth, ', dip=', dip, '</title>')}
direction <- paste('<direction>',sin(azimuth),cos(azimuth),-sin(dip),'</direction>')
ax <- paste('<x>', stringfromrow(x),'</x>')
ay <- paste('<y>', stringfromrow(y),'</y>')
apairs <- paste('<pairs>', stringfromrow(pairs),'</pairs>')
return(c(begin, title, direction, ax, ay, apairs, end))
}