#source function
source("./stabil_function.r")

###############################MAMMALS##############################################################
#portal
portalRA=read.table("./Mammals/Portal/rodents_controls.txt",header=T)
portalE=read.table("./Mammals/Portal/energy_controls.txt",header=T)
portalrod_out=stabil(portalRA,c("Dipodomys.merriami","Dipodomys.ordii","Dipodomys.spectabilis","Chaetodipus.baileyi","Peromyscus.eremicus","Perognathus.flavus","Peromyscus.maniculatus","Chaetodipus.penicillatus","Reithrodomtomys.montanus"),"Portal Rodents")

#sevilleta
pgrassRA=read.table("./Mammals/sevilleta/5pgrass.txt",header=T)
pgrassE=read.table("./Mammals/sevilleta/5pgrass_E.txt",header=T)
pgrass_out=stabil(pgrassE,names(pgrassE)[-(1:3)],"Sevilleta 5-Points Grass Rodents")

plarreaRA=read.table("./Mammals/sevilleta/5plarrea.txt",header=T)
plarreaE=read.table("./Mammals/sevilleta/5plarrea_E.txt",header=T)
plarrea_out=stabil(plarreaE,names(plarreaE)[-(1:3)],"Sevilleta 5-Points Larrea Rodents")

rslarreaRA=read.table("./Mammals/sevilleta/rslarrea.txt",header=T)
rslarreaE=read.table("./Mammals/sevilleta/rslarrea_E.txt",header=T)
rslarrea_out=stabil(rslarreaE,names(rslarreaE)[-(1:3)],"Sevilleta Rio Salado Larrea Rodents")

#Konza
konza=read.table("./Mammals/Konza_Abund.txt",header=T)
konza_out=stabil(konza,names(konza)[-(1:3)],"Konza Rodents")

#Curlew Valley
curlew=read.table("./Mammals/curlew.txt",header=T)
curlew_out=stabil(curlew,names(curlew)[-(1:3)],"Curlew Rodents")

ineel=read.table("./Mammals/INEEL.txt",header=T)
ineel_out=stabil(ineel,names(ineel)[-(1:3)],"INEEL Rodents")

#Curlew Valley excluding PMs
curlewPM=read.table("./Mammals/curlewPM.txt",header=T)
curlewPM_out=stabil(curlewPM,names(curlewPM)[-(1:3)],"Curlew Rodents (no manics)")

#Jornada
jgrassRA=read.table("./Mammals/Jornadagrass.txt",header=T)
jgrassE=read.table("./Mammals/Jornadagrass_E.txt",header=T)
jgrass_out=stabil(jgrassE,names(jgrassE)[-(1:3)],"Jornada Grassland Rodents")

jshrubRA=read.table("./Mammals/Jornadashrub.txt",header=T)
jshrubE=read.table("./Mammals/Jornadashrub_E.txt",header=T)
jshrub_out=stabil(jshrubE,names(jshrubE)[-(1:3)],"Jornada Shrubland Rodents")

#Powdermill
pdmA=read.table("./Mammals/Powdermill/PDMabund.txt",header=T)
pdmE=read.table("./Mammals/Powdermill/PDME.txt",header=T)
pdm_out=stabil(pdmA,names(pdmA)[-(1:3)],"Powdermill NR Rodents")

#Powdermill - squirrels
pdm_sqA=read.table("./Mammals/Powdermill/pdm_sqabund.txt",header=T)
pdm_sqE=read.table("./Mammals/Powdermill/pdm_sqenergy.txt",header=T)
pdmsq_out=stabil(pdm_sqA,names(pdm_sqA)[-(1:3)],"Powdermill NR Squirrels")

#Powdermill - granivores
pdm_rodA=read.table("./Mammals/Powdermill/pdm_rodabund.txt",header=T)
pdm_rodE=read.table("./Mammals/Powdermill/pdm_rodenergy.txt",header=T)
pdmrod_out=stabil(pdm_rodE,names(pdm_rodA)[-(1:3)],"Powdermill NR Granivores")

#Powdermill - herbivores
pdm_herbA=read.table("./Mammals/Powdermill/pdm_herbabund.txt",header=T)
pdm_herbE=read.table("./Mammals/Powdermill/pdm_herbenergy.txt",header=T)
pdmherb_out=stabil(pdm_herbE,names(pdm_herbA)[-(1:3)],"Powdermill NR Herbivores")

#Shortgrass Steppe
shortgrass=read.table("./Mammals/shortgrass.txt",header=T)
shortgrassE=read.table("./Mammals/shortgrassE.txt",header=T)
shortgrass_out=stabil(shortgrassE,names(shortgrassE)[-(1:3)],"Shortgrass LTER Rodents")

#Ontario
ontariorod=read.table("./Mammals/Ontario_rod.txt",header=T)
ontariorod_out=stabil(ontariorod,names(ontariorod)[-(1:3)],"Ontario Rodents")

#Mont St. Hilaire
hilaire=read.table("./Mammals/hilaire.txt",header=T)
hilaire_out=stabil(hilaire,names(hilaire)[-(1:3)],"Mont St. Hilaire Rodents")

#KarooNP ungulates
karoo_ung=read.table("./Mammals/karoo_ungulates.txt",header=T)
karooung_out=stabil(karoo_ung,names(karoo_ung)[-(1:3)],"Karoo NP Ungulates")

#KarooNP carnivores
karoo_carn=read.table("./Mammals/karoo_carnivores.txt",header=T)
karoocarn_out=stabil(karoo_carn,names(karoo_carn)[-(1:3)],"Karoo NP Carnivores")

#KarooNP w/ ostrich
karoo_ost=read.table("./Mammals/karoo_ost.txt",header=T)
karooost_out=stabil(karoo_ost,names(karoo_ost)[-(1:3)],"Karoo NP Herbivores")

#KrugerNP
kruger=read.table("./Mammals/KrugerNP.txt",header=T)
kruger_biomass=read.table("./Mammals/KrugerNP_biomass.txt",header=T)
kruger_out=stabil(kruger,names(kruger)[-(1:3)],"Kruger NP")

#Golden Gate NP
golden=read.table("./Mammals/goldengate.txt",header=T)
golden_biomass=read.table("./Mammals/goldengate_biomass.txt",header=T)
golden_out=stabil(golden,names(golden)[-(1:3)],"Golden Gate Highlands NP")

#Canadian mesocarnivores
alberta=read.table("./Mammals/canada_carnivores/alberta.txt",header=T)
alberta_out=stabil(alberta,names(alberta)[-(1:3)],"Alberta Carnivores")

BC=read.table("./Mammals/canada_carnivores/BC.txt",header=T)
BC_out=stabil(BC,names(BC)[-(1:3)],"BC Carnivores")

manitoba=read.table("./Mammals/canada_carnivores/manitoba.txt",header=T)
manitoba_out=stabil(manitoba,names(manitoba)[-(1:3)],"Manitoba Carnivores")

newbrunswick=read.table("./Mammals/canada_carnivores/newbrunswick.txt",header=T)
newbrunswick_out=stabil(newbrunswick,names(newbrunswick)[-(1:3)],"New Brunswick Carnivores")

northwest=read.table("./Mammals/canada_carnivores/northwest.txt",header=T)
northwest_out=stabil(northwest,names(northwest)[-(1:3)],"Northwest Territories Carnivores")

ontario=read.table("./Mammals/canada_carnivores/ontario.txt",header=T)
ontario_out=stabil(ontario,names(ontario)[-(1:3)],"Ontario Carnivores")

quebec=read.table("./Mammals/canada_carnivores/quebec.txt",header=T)
quebec_out=stabil(quebec,names(quebec)[-(1:3)],"Quebec Carnivores")

saskatch=read.table("./Mammals/canada_carnivores/saskatch.txt",header=T)
saskatch_out=stabil(saskatch,names(saskatch)[-(1:3)],"Saskatchewan Carnivores")

yukon=read.table("./Mammals/canada_carnivores/yukon.txt",header=T)
yukon_out=stabil(yukon,names(yukon)[-(1:3)],"Yukon Carnivores")

#################################BIRDS##############################################################

#Hubbard Brook
hubbard=read.table("./Birds/hubbard.txt",header=T)
hubbard_out=stabil(hubbard,names(hubbard)[-(1:3)],"Hubbard Brook")

#White Mountain NF
whitemt=read.table("./Birds/whitemountain.txt",header=T)
whitemt_out=stabil(whitemt,names(whitemt)[-(1:3)],"White Mountain NF Birds")

#Redvers
redvers=read.csv("./Birds/redvers.txt",header=T)
redvers_out=stabil(redvers,names(redvers)[-(1:3)],"Redvers Waterfowl")

#Skokholm
skokholm=read.table("./Birds/skokholm.txt",header=T)
skokholm_out=stabil(skokholm,names(skokholm)[-(1:3)],"Skokholm Birds")

skokholm_water=read.table("./Birds/skokholm_water.txt",header=T)
skokholm_water_out=stabil(skokholm_water,names(skokholm_water)[-(1:3)],"Skokholm Waterfowl")

skokholm_carrion=read.table("./Birds/skokholm_carrion.txt",header=T)
skokholm_carrion_out=stabil(skokholm_carrion,names(skokholm_carrion)[-(1:3)],"Skokholm Carrion Birds")

#Konza LTER
konza_waterfowl=read.table("./Birds/konza_waterfowl.txt",header=T)
konza_waterfowl_out=stabil(konza_waterfowl,names(konza_waterfowl)[-(1:3)],"Konza Waterfowl")

konza_raptors=read.table("./Birds/konza_raptors.txt",header=T)
konza_raptors_out=stabil(konza_raptors,names(konza_raptors)[-(1:3)],"Konza Raptors")

konza_birds=read.table("./Birds/konza_birds.txt",header=T)
konza_birds_out=stabil(konza_birds,names(konza_birds)[-(1:3)],"Konza Birds")

konza_game=read.table("./Birds/konza_gamebirds.txt",header=T)
konza_game_out=stabil(konza_game,names(konza_game)[-(1:3)],"Konza Game Birds")

konza_songbirds=read.table("./Birds/konza_songbirds.txt",header=T)
konza_song_out=stabil(konza_songbirds,names(konza_songbirds)[-(1:3)],"Konza Songbirds")

#Texas Migratory Birds
texas=read.table("./Birds/texas.txt",header=T)
texas_out=stabil(texas,names(texas)[-(1:3)],"Texas Migratory Birds")

#Eastern Wood
easternwood=read.table("./Birds/easternwood.txt",header=T)
easternwood_out=stabil(easternwood,names(easternwood)[-(1:3)],"Eastern Wood Breeding Birds")

#Pawnee Birds
pawnee_birds=read.table("./Birds/pawnee_birds.txt",header=T)
pawnee_birds_out=stabil(pawnee_birds,names(pawnee_birds)[-(1:3)],"Pawnee Breeding Birds")

#Pawnee Raptors
pawnee_raptors=read.table("./Birds/pawnee_raptors.txt",header=T)
pawnee_raptors_out=stabil(pawnee_raptors,names(pawnee_raptors)[-(1:3)],"Pawnee Raptors")

#Mountain Bird Watch
mountainbird=read.csv("./Birds/mountainbird.csv",header=T)
mountainbird_out=stabil(mountainbird,names(mountainbird)[-(1:3)],"Mountain Bird Watch")

#Mountain Bird Watch - Maine
mountainbird_maine=read.table("./Birds/mountainbird_maine.txt",header=T)
mountainbird_maine_out=stabil(mountainbird_maine,names(mountainbird_maine)[-(1:3)],"Maine Mountain Passerines")

#Mountain Bird Watch - New York
mountainbirdny=read.table("./Birds/mountainbird_ny.txt",header=T)
mountainbirdny_out=stabil(mountainbirdny,names(mountainbirdny)[-(1:3)],"New York Mountain Passerines")

#Mountain Bird Watch - Green Mountains
mountainbirdvt=read.table("./Birds/mountainbird_vt.txt",header=T)
mountainbirdvt_out=stabil(mountainbirdvt,names(mountainbirdvt)[-(1:3)],"Green Mountains Passerines")

#Luquillo Birds
elverde=read.table("./Birds/elverde.txt",header=T)
elverde_out=stabil(elverde,names(elverde)[-(1:3)],"Luquillo Birds")

#################################REPTILES############################################################
#Luquillo
luquillo=read.table("./Herps/luquillo.txt",header=T)
luquillo_out=stabil(luquillo,names(luquillo)[-(1:3)],"Luquillo Towers")

#Bold Park
boldpark=read.table("./Herps/boldpark.txt",header=T)
boldpark_out=stabil(boldpark,names(boldpark)[-(1:3)],"Bold Park Herps")

boldparksnakes=read.table("./Herps/boldparksnakes.txt",header=T)
boldparksnakes_out=stabil(boldparksnakes,names(boldparksnakes)[-(1:3)],"Bold Park Snakes")

boldparklizards=read.table("./Herps/boldparklizards.txt",header=T)
boldparklizards_out=stabil(boldparklizards,names(boldparklizards)[-(1:3)],"Bold Park Lizards")

#Cowley County
cowleycounty=read.table("./Herps/cowleycounty.txt",header=T)
cowleycounty_out=stabil(cowleycounty,names(cowleycounty)[-(1:3)],"Cowley County Herps")

cowleysnakes=read.table("./Herps/cowleysnakes.txt",header=T)
cowleysnakes_out=stabil(cowleysnakes,names(cowleysnakes)[-(1:3)],"Cowley County Snakes")

cowleylizards=read.table("./Herps/cowleylizards.txt",header=T)
cowleylizards_out=stabil(cowleylizards,names(cowleylizards)[-(1:3)],"Cowley County Lizards")

cowleyturtles=read.table("./Herps/cowleyturtles.txt",header=T)
cowleyturtles_out=stabil(cowleyturtles,names(cowleyturtles)[-(1:3)],"Cowley County Turtles")

#Ora Banda
orabanda=read.table("./Herps/orabanda.txt",header=T)
orabanda_out=stabil(orabanda,names(orabanda)[-(1:3)],"Ora Banda Herps")

orabandasnakes=read.table("./Herps/orabandasnakes.txt",header=T)
orabandasnakes_out=stabil(orabandasnakes,names(orabandasnakes)[-(1:3)],"Ora Banda Snakes")

orabandalizards=read.table("./Herps/orabandalizards.txt",header=T)
orabandalizards_out=stabil(orabandalizards,names(orabandalizards)[-(1:3)],"Ora Banda Lizards")

#################################INVERTEBRATES#######################################################


#################################PLANTS##############################################################

#Desert Laboratory Plants
desertlab=read.table("./Plants/desertlab.txt",header=T)
desertlab_out=stabil(desertlab,names(desertlab)[-(1:3)],"Desert Laboratory Annuals (open habitats)")

desertlabshrub=read.table("./Plants/desertlab_shrub.txt",header=T)
desertlabshrub_out=stabil(desertlabshrub,names(desertlabshrub)[-(1:3)],"Desert Laboratory Annuals (shrub habitats)")

#Portal Summer annuals
portalsummer=read.csv("./Plants/portalsummer.csv",header=T)
portalsummer_out=stabil(portalsummer,names(portalsummer)[-(1:3)],"Portal Summer annuals")

#Portal Winter annuals
portalwinter=read.csv("./Plants/portalwinter.csv",header=T)
portalwinter_out=stabil(portalwinter,names(portalwinter)[-(1:3)],"Portal Winter annuals")
