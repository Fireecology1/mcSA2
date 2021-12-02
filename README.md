## mcSA2 - Version-controlled (git/Github) mcSA project for ConPyro System

Stand-adjusted (Wotton-Beverly 2007) moisture content model, for adjusting the FFMC estimate 
of fine fuel moisture content using stand, density, season, and DMC variables. 
Currently used directly in Conifer Pyrometrics System (Perrakis et al. 2020); can also be used
as stand-alone fine dead fuel moisture calculator. 

Requires FFMC, DMC, stand (1:5; deciduous, Douglas-fir, mixedwood, pine, spruce), 
density (1:3; light, mod, dense), season (1, 1.5, 2, 3; spring, spr-sum transition, sum, fall)

season==1.5 represents spring/summer transition (uses half of spring, half of summer estimate)

ConPyro version includes a statement recoding density at high FFMC levels, to avoid nonsensical  
behaviour with the pure statistical models: 
Above FFMC 92.9, dense stands recoded to moderate
Above FFMC 96.1, light stands recoded to moderate
Above FFMC 95.15, spring or sp/sum transition seasons recoded to summer

D.Perrakis, December 2021; daniel.perrakis@nrcan-rncan.gc.ca
