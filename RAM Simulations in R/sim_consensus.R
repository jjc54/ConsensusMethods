library(tidyverse)

### SETUP
# 9 experts
# 20 questions
# 9 Likert scale #appropriateness (0: inappropriate to 9:appropriate)

getmedianram = function(){

# function to generate prob for an expert 
expert = function(seed){
  set.seed(seed)
stubbornness = sample(0:1,size = 1) # expert sure 1 or not sure 0 (stubbornness level)
#if (stubbornness){
    appro.direction = sample(0:1,size = 1) # 1 - answer 1/2 or 0 - 8/9
      if (appro.direction){
          prob = c(0.33,0.33,0.34,0,0,0,0,0,0)
        }else{
        prob = c(0,0,0,0,0,0,0.34,0.33,0.33)
        }
#}
# else{
#     prob = c(0,0,0.0,0.33,0.34,0.33,0,0,0) 
# }
return(prob)
}


# generate randomly 9 experts using expert() function
exp.ind = sample(1:99999,size = 9)
probs = purrr::map(exp.ind , expert)

# data
data.sim = purrr::map(probs , rmultinom,n=20,size=1)  # generate probs
data.sim2 = lapply(data.sim,function(x) x*(1:9)) # generate answers 1 to 9
data.sim3 = do.call(rbind, lapply(data.sim2, as.data.frame))
colnames(data.sim3) = paste0("Q",1:20)
data.sim3 = data.sim3 |> 
  mutate(expertID = paste0("EXP", gl(k = 9,9,labels = 1:9)))

# Zero to NA so we can remove them later to keep only the answer 
dd = data.sim3 |> 
  group_by(expertID) |> 
  summarise(across(.cols=everything(),.fns=~sum(.)))

tdd = t(dd)
colnames(tdd) = paste0("rater",1:9)


library(irr)
#Fleiss, J.L., and others. 1971. “Measuring Nominal Scale Agreement Among Many Raters.” Psychological Bulletin 76 (5): 378–82.
#Joseph L. Fleiss, Myunghee Cho Paik, Bruce Levin. 2003. Statistical Methods for Rates and Proportions. 3rd ed. John Wiley; Sons, Inc.




# compute mode and median
dd |> 
  select(-expertID) |> 
  summarise(across(
    .cols = everything(),
    .fns =  list(median = ~median(.,na = TRUE)))) -> medians

medians$kappa = kappam.fleiss(tdd)$value

return(medians)
}

as.tibble(t(replicate(10000,getmedianram(),simplify = T))) ->sim.result

