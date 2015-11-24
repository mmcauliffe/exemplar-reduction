t <- read.delim('test_output_sr.txt')
iphod <- read.delim('C:\\Users\\michael\\Documents\\Data\\Iphod\\IPhOD2_Words.txt')
iphod <- iphod[,names(iphod) %in% c('Word','unsDENS','SFreq')]

t$FollSpeakRate <- as.numeric(as.character(t$FollSpeakRate))
t$PrevSpeakRate <- as.numeric(as.character(t$PrevSpeakRate))
t$DurDiff <- as.numeric(as.character(t$DurDiff))
t$PrevCondProb <- as.numeric(as.character(t$PrevCondProb))
t$FollCondProb <- as.numeric(as.character(t$FollCondProb))
t$Duration <- as.numeric(as.character(t$Duration))

t <- subset(t,Dist != 0 & DurDiff != 0 & PrevSpeakRate != 0 & FollSpeakRate != 0 & PrevCondProb != 1 & FollCondProb != 1)

cor.test(t$Dist,t$DurDiff)

t$PrevSpeakRate <- log(t$PrevSpeakRate)
t$PrevSpeakRate <- t$PrevSpeakRate - mean(t$PrevSpeakRate)

t$FollSpeakRate <- log(t$FollSpeakRate)
t$FollSpeakRate <- t$FollSpeakRate - mean(t$FollSpeakRate)

t$PrevCondProb <- log(t$PrevCondProb)
t$PrevCondProb <- t$PrevCondProb - mean(t$PrevCondProb)

t$FollCondProb <- log(t$FollCondProb)
t$FollCondProb <- t$FollCondProb - mean(t$FollCondProb)

t$Duration <- log(t$Duration)
t$Duration <- t$Duration - mean(t$Duration)

testt <- sparseby(t, list(group = t[,c("Speaker", "Word")]),function(subset) within(subset,{ HyperHyp <- abs(Dist - mean(Dist))}))
testt[testt$DurDiff < 0,]$HyperHyp <- -1 *testt[testt$DurDiff < 0,]$HyperHyp

t$HyperHyp <- abs(t$Dist - mean(t$Dist))
t[t$DurDiff < 0,]$HyperHyp <- -1 *t[t$DurDiff < 0,]$HyperHyp

wordcounts <- ddply(t2,~Word, nrow)
wordcounts[wordcounts$V1 < 5,]$Word
t <- subset(t, !Word %in% wordcounts[wordcounts$V1 < 10,]$Word)

t$Word <- factor(t$Word)

t2 <- merge(testt,iphod)

t2$NeighDen <- t2$unsDENS - mean(t2$unsDENS)
t2$Freq <- log(t2$SFreq)
t2$Freq <- t2$Freq - mean(t2$Freq)

t2$OrthoLen <- t2$OrthoLen - mean(t2$OrthoLen)

t.lmer <- lmer(HyperHyp~ FollSpeakRate + PrevSpeakRate + (1+FollSpeakRate + PrevSpeakRate |Word)+ (1+FollSpeakRate + PrevSpeakRate|Speaker),data=t)

t.lmer <- lmer(HyperHyp~ PrevCondProb + FollCondProb  + (1+PrevCondProb + FollCondProb |Word)+ (1+PrevCondProb + FollCondProb|Speaker),data=t2)

t2 <- subset(t2,Speaker!='s06')
t2 <- subset(t2, !Word %in% wordcounts[wordcounts$V1 < 10,]$Word)

hh.lmer <- lmer(HyperHyp~ Freq + NeighDen  + Gender + Age + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb  + (1|Word)  + (0 + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb |Word)+ (1|Speaker)+ (0+ Freq + NeighDen +FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb|Speaker),data=t2, control=lmerControl(optCtrl=list(maxfun=1000000) ))
summary(hh.lmer)

dd.lmer <- lmer(DurDiff~ Freq + NeighDen  + Gender + Age + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb  + (1|Word)  + (0 + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb |Word)+ (1|Speaker)+ (0+ Freq + NeighDen +FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb|Speaker),data=t2, control=lmerControl(optCtrl=list(maxfun=1000000) ))
summary(dd.lmer)

dur.lmer <- lmer(Duration~ Freq + NeighDen  + Gender + Age + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb  + (1|Word)  + (0+ FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb |Word)+ (1|Speaker)+ (0 + Freq + NeighDen  +FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb|Speaker),data=t2, control=lmerControl(optCtrl=list(maxfun=1000000)))
summary(dur.lmer)

hh.lmer.trim <- lmer(HyperHyp~ Freq + NeighDen + Gender + Age + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb  + (1|Word)  + (0+ FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb |Word)+ (1|Speaker)+ (0+ Freq + NeighDen +FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb|Speaker),data=t2, subset = abs(scale(resid(hh.lmer))) < 2.5, control=lmerControl(optCtrl=list(maxfun=1000000) ))
summary(hh.lmer.trim)

dd.lmer.trim <- lmer(DurDiff~ Freq + NeighDen + Gender + Age + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb  + (1|Word)  + (0 + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb |Word)+ (1|Speaker)+ (0+ Freq + NeighDen +FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb|Speaker),data=t2, subset = abs(scale(resid(dd.lmer))) < 2.5, control=lmerControl(optCtrl=list(maxfun=1000000) ))
summary(dd.lmer.trim)

dur.lmer.trim <- lmer(Duration~ Freq + NeighDen  + Gender + Age + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb  + (1|Word)  + (0 + FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb |Word)+ (1|Speaker)+ (0 + Freq + NeighDen  +FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb|Speaker),data=t2, subset = abs(scale(resid(dur.lmer))) < 2.5, control=lmerControl(optCtrl=list(maxfun=1000000)))
summary(dur.lmer.trim)


ddply(t,~Word, summarise, sd(PrevCondProb), sd(FollCondProb))

t.sub <- subset(t,abs(scale(resid(dur.lmer.trim))) < 2.5)
ggplot(t.sub,aes(x=HyperHyp,y=FollSpeakRate)) + geom_point() +geom_smooth()

t.lmer.trim <-lmer(HyperHyp~ FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb + (1|Word)  + (1+FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb |Word)+ (1|Speaker)+ (0+FollSpeakRate + PrevSpeakRate + PrevCondProb + FollCondProb|Speaker),data=t.sub, control=lmerControl(optCtrl=list(maxfun=20000) ))


ddply(t2,~Speaker, nrow)
wordcounts <- ddply(t2,~Word, nrow)

