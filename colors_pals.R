require(pals)

pal.bands(coolwarm, parula, ocean.haline, cubicl, kovesi.rainbow, ocean.phase, brewer.paired(12), stepped,
          main="Colormap suggestions")

labs=c('alphabet','alphabet2', 'glasbey','kelly','pal36', 'stepped', 'tol', 'watlington')
op=par(mar=c(0,5,3,1))
pal.bands(alphabet(), alphabet2(), glasbey(), kelly(),
          pal36(), stepped(), tol(), watlington(), labels=labs, show.names=FALSE)

pal.bands(coolwarm,cubehelix,gnuplot,jet,parula,tol.rainbow)

pal.bands(cubicyf,cubicl,isol,linearl,linearlhot,
          main="Niccoli")

par(op)
pal.bands(coolwarm, viridis, parula, n=200)

pal.bands(coolwarm, n=200)

# smooth palettes usually easy to compress
p1 <- coolwarm(255)
cool2 <- pal.compress(coolwarm)
p2 <- colorRampPalette(cool2)(255)
pal.bands(p1, p2, cool2,
          labels=c('original','compressed', 'basis'), main="coolwarm")

op <- par(mfrow=c(3,1), mar=c(1,1,2,1))
pal.sineramp(jet, main="jet")
pal.sineramp(tol.rainbow, main="tol.rainbow")
pal.sineramp(kovesi.rainbow, main="kovesi.rainbow")

require(ggplot2)
require(pals)
require(reshape2)

ggplot(melt(volcano), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradientn(colours=coolwarm(100), guide = "colourbar")
