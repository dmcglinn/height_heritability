
dat = read.csv('height_211.csv')
dat = subset(dat, subset = paste(dat$semester, dat$year) != 'fall 2015')

## drop males because only 2 outliers exist
dat = dat[dat$Sex == 'F', ]

mod1 = lm(offspring.height ~ mid.parent, data=dat)
mod2 = update(mod1, . ~ . + Sex)

pdf('./height_midparent.pdf')
par(mfrow=c(1,1))
hist(dat$offspring.height[dat$semester=='fall' & dat$year ==2015],
     xlab='Student Heights (in)', main='')
ylims = range(dat$offspring.height)
xlims = range(dat$mid.parent)
plot(offspring.height ~ mid.parent, data=dat, subset=semester=='fall' & year==2015,
     pch=19, xlim=xlims, ylim=ylims)
abline(lm(offspring.height ~ mid.parent, data=dat, subset=semester=='fall' & year==2015))
###
plot(offspring.height ~ mid.parent, data=dat, pch=19, xlim=xlims, ylim=ylims)
abline(lm(offspring.height ~ mid.parent, data=dat))
###
plot(offspring.height ~ mid.parent, data=dat, type='n', xlim=xlims, ylim=ylims)
points(offspring.height ~ mid.parent, data=dat, subset=Sex=='M', col='dodgerblue',
       pch=19)
points(offspring.height ~ mid.parent, data=dat, subset=Sex=='F', col='pink',
       pch=19)
abline(lm(offspring.height ~ mid.parent, data=dat, subset=Sex=='M'), 
       col='dodgerblue')
abline(lm(offspring.height ~ mid.parent, data=dat, subset=Sex=='F'), 
       col='pink')
legend('topleft', c('Males', 'Females'), col=c('dodgerblue','pink'), pch=19,bty='n')
dev.off()

mods = vector('list', 3)
h2 = se = rep(NA, 3)

mods[[1]] = lm(offspring.height ~ mid.parent, data=dat)
mods[[2]] = lm(offspring.height ~ mid.parent, data=dat, subset=Sex=='F')
mods[[3]] = lm(offspring.height ~ mid.parent, data=dat, subset=Sex=='M')

for(i in 1:3) {
    h2[i] = coef(mods[[i]])[2]
    se[i] = summary(mods[[i]])$coef[2,2]
}

xmin = min(h2 - 1.96 * se)
xmax = max(h2 + 1.96 * se)

pdf('./hereitablity_estimate.pdf')
cols= c('black','pink','dodgerblue')
par(mfrow=c(1,1))
plot(h2, 3:1, xlim=c(xmin, xmax), frame.plot=F, axes=F,
     ylab='Sex', xlab='Heritability (h^2)', pch=19,
     col=cols)
arrows(h2 - 1.96 * se, 3:1,
       h2 + 1.96 * se, code=3, 3:1, angle=90, length=.1 , 
       col=cols)
abline(v=0, lty=2)
axis(side=2, at = 3:1, labels = c('Both', 'Female', 'Male'))
axis(side=1, at=seq(-.4, 1.4, .4))
dev.off()
