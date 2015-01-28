
dat1 = read.csv('./height_211_2014.csv')
dat1$year = 2014
dat2 = read.csv('./height_211_2015.csv')
dat2$year = 2015
head(dat1)
head(dat2)
dat = rbind(dat1, dat2)

## drop males because only 2 outliers exist
dat = dat[dat$Sex == 'F', ]

mods = vector('list', 3)
h2 = se = rep(NA, 3)

mods[[1]] = lm(offspring.height ~ mom.height, data=dat)
mods[[2]] = lm(offspring.height ~ dad.height, data=dat)
mods[[3]] = lm(offspring.height ~ mid.parent, data=dat)

for(i in 1:3) {
    h2[i] = coef(mods[[i]])[2]
    se[i] = summary(mods[[i]])$coef[2,2]
}

par(mfrow=c(1,3))
plot(offspring.height ~ mom.height, data=dat)
abline(mods[[1]])
plot(offspring.height ~ dad.height, data=dat)
abline(mods[[2]])
plot(offspring.height ~ mid.parent, data=dat)
abline(mods[[3]])

ymin = min(h2 - 1.96 * se)
ymax = max(h2 + 1.96 * se)

par(mfrow=c(1,1))
plot(1:3, h2, ylim=c(ymin, ymax), frame.plot=F, axes=F,
     xlab='', ylab='Heritability (h^2)')
arrows(1:3, h2 - 1.96 * se,
       1:3, h2 + 1.96 * se, code=3, angle=90, length=.1 )
abline(h=0, lty=2)
axis(side=1, at = 1:3, labels = c('mom', 'dad', 'avg'))
axis(side=2)

pdf('./class_heights.pdf')
par(mfrow=c(1,1))
hist(dat$offspring.height, main='', xlab='Offspring Height (ft)')
plot(offspring.height ~ mid.parent, data=dat, frame.plot=F)
abline(mods[[3]])
plot(1, h2[3], ylim=c(ymin, ymax), frame.plot=F, axes=F,
     xlab='', ylab='Heritability (h^2)')
arrows(1, h2[3] - 1.96 * se[3],
       1, h2[3] + 1.96 * se[3], code=3, angle=90, length=.1 )
abline(h=0, lty=2)
axis(side=2)
dev.off()
