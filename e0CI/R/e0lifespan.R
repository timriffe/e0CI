mxi <- c(0.00339193594631364, 0.000218176772567409, 0.000112225116810933, 
  6.28291177575766e-05, 0.000101363542293164, 6.51192928664136e-05, 
  0.000149171546489114, 9.1324237373929e-05, 2.37282051423152e-05, 
  0.000147879337023085, 9.65951840540929e-05, 0.000117234660071609, 
  0.000148808909648065, 0.000163764625306839, 0.000149412048660226, 
  0.000213094661860419, 0.00023027681399857, 0.000282336781997834, 
  0.00050188288290611, 0.000562886740592384, 0.000574408115385048, 
  0.000514548145914474, 0.000575794758336449, 0.000480863313117501, 
  0.000463134800382548, 0.000502754181984964, 0.000645385305694193, 
  0.0003667942771129, 0.000603066930943783, 0.00045207728108274, 
  0.000478283688664601, 0.00042576207621009, 0.000617385146939247, 
  0.000508241397905761, 0.000644703331378454, 0.000737128700261654, 
  0.000781916438939588, 0.000780640652915767, 0.000967075438995114, 
  0.000920303288718444, 0.000902575264339258, 0.00109044964305154, 
  0.00101740127547675, 0.0015271255555041, 0.00169574111630681, 
  0.00175169148842781, 0.0018026473940166, 0.00216983959187882, 
  0.00239814419332988, 0.00269181794667615, 0.00267041310251076, 
  0.00319234932239479, 0.00329322987795461, 0.00354615423575612, 
  0.00420398515967041, 0.00425132697285316, 0.00521858341634454, 
  0.00539687553611323, 0.00621780287015636, 0.00612863243489794, 
  0.00726211576445593, 0.00791084227232883, 0.00906584547738209, 
  0.00891900418960592, 0.0107880933694964, 0.0116056543187965, 
  0.0128801771661057, 0.0134168872110118, 0.0148532976158312, 0.0173448638578334, 
  0.0187429106408253, 0.0208917282602883, 0.0234542109586237, 0.0257972488018167, 
  0.0287920169190418, 0.0314573432001538, 0.0364895860299795, 0.0399768034760908, 
  0.043965565614878, 0.0507213174341672, 0.0596296552080825, 0.0637515213108531, 
  0.0732254121078608, 0.0818383560964431, 0.0948940158613105, 0.106824456366161, 
  0.120436207306585, 0.134541051681535, 0.150389179836178, 0.164475493940974, 
  0.188367367423705, 0.207633882777195, 0.234706937851654, 0.255445646786733, 
  0.281561247690424, 0.312059485943387, 0.341773555257173, 0.372838620781916, 
  0.405047636966564, 0.438155042120152, 0.471883011885644, 0.505930054282744, 
  0.539981448344002, 0.573720790589121, 0.606841759923003, 0.639059173580347, 
  0.670118493512966, 0.699803137778454, 0.727939217682343, 0.754397610310851, 
  0.779093540465437, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
e0basic <- sum(exp(-cumsum(mxi)))
library(compiler)
library(parallel)

# generate a single lifespan according to some
# super-simple assumptions.
lifespan <- function(mxi){
    # assumptions:
    # 1) mx is valid as the hazard
    # 2) hazard is constant within single-age intervals
    
    # for each single age, we randomly generate a lifespan
    # all you need in order to survive each age for the 
    # lifespan to be greater than 1
    times  <- rexp(n=rep(1,length(mxi)),rate=mxi) 
    # at which ages 'would' the person die? (logical vector)
    timesl <- times < 1
    # logicals are also interpreted as 0,1 in R,
    # and I want to throw out the 0s, so we replace with NAs
    timesl[!timesl] <- NA
    # give me the first age (index) at which the person would die:
    int <- which.min(timesl) 
    # minus one to make it completed age, this is a lifespan
    int - 1 #+ times[int]
  }

gete0exp <- function(.mxi,.PopSize=1e6){
  # generate a lifeline for each member of the population.
  # imagine that this is a cohort. In this case a simulated
  # synthetic cohort of ages at death. Then the mean of these
  # lifespans is the same as e0, no?
  mean(replicate(.PopSize,lifespan(.mxi)))
}
#gete0exp(mxi,1e6)
# how many times do we want to simulate?
n <- 1000
# we apply over the vector of 1:n, which itself isn't used numerically...
# assume a population of a 1e6 people for now. You can change the number
# of cores to suit your machine. On my machine with 6 cores it takes like 
# 10 seconds.
e0vec <- replicate(n, gete0exp(mxi,.PopSize=1e4))
# throwing errors today that I can't find...
#e0vec <- mclapply(1:n,gete0exp,.mxi=mxi,.PopSize=1e4,mc.cores=6)
#e0vec <- lapply(1:n,gete0exp,.mxi=mxi,.PopSize=1e4)
# with enough of these e0 estimate we can see 
fivenum(e0vec)
var(e0vec)
plot(density(e0vec))
abline(v=mean(e0vec)) 
abline(v=quantile(e0vec,c(.025,.975)),col="red") 
diff(quantile(e0vec,c(.025,.975))) # .53044

# similar finding
set.seed(1)
e0vec1 <- replicate(n, gete0exp(mxi,.PopSize=1e4)) # uses many more randomly generated numbers than the following
set.seed(1)
e0vec2 <- replicate(n, e0dx(dx, 1e4))

# asymptotically the same
diff(quantile(e0vec1,c(.025,.975)))
diff(quantile(e0vec2,c(.025,.975)))