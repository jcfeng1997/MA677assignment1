library(ggplot2)

## Experiments

alpha_0.6_m <- function(m){
  1-pbinom(m-1,100,0.6)
}

##ggplot()+
##stat_function(fun = alpha_0.6_m)+
##xlim(1,100)

for (i in 1:100) {
  m1 <- 1
  if(alpha_0.6_m(m1)>0.05){
    m1 <- i+1
  }
  if(alpha_0.6_m(m1)<0.05){
    print(m1)
    break
  }
}

alpha_0.8_m <- function(m){
  1-pbinom(m-1,100,0.8)
}

##ggplot()+
##stat_function(fun = alpha_0.8_m)+
##xlim(1,100)

for (i in 1:100) {
  m2 <- 1
  if(alpha_0.8_m(m2)>0.95){
    m2 <- i+1
  }
  if(alpha_0.8_m(m2)<0.95){
    print(m2-1)
    break
  }
}


# Replicate

# define function for m=69 
alpha_p_69 <- function(p){
  1-pbinom(68,100,p)
}

# define function for m=73 
alpha_p_73 <- function(p){
  1-pbinom(72,100,p)
}

# plot
ggplot()+
  stat_function(fun = alpha_p_69)+
  xlim(.4,1)+
  stat_function(fun = alpha_p_73)+
  xlim(.4,1)+
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  geom_segment(aes(x=.6,xend=.8,y=.95,yend=.95),colour="#990000", linetype="dashed")+
  geom_segment(aes(x=.6,xend=.8,y=.05,yend=.05),colour="#990000", linetype="dashed")+
  geom_segment(aes(x=.6,xend=.6,y=.05,yend=.95),colour="#990000", linetype="dashed")+
  geom_segment(aes(x=.8,xend=.8,y=.05,yend=.95),colour="#990000", linetype="dashed")+
  xlab("p")+
  ylab(" ")