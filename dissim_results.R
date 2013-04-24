##########################Write Dissimilarity Results to file######################################################

dissim_results=function(filename, name) { 
  
  source("./dissimilarity.R")
  
  #Read file
  dataset=read.table(filename,header=T)
  
  #Run analysis
  out=dissim(dataset,name)
  
  #Save results  
  write.table(out, "dissimilarity.csv", append=T, sep=",", col.names = F, row.names=FALSE)
  
}

#List of files
files=c("./Birds/hubbard.txt","./Birds/whitemountain.txt","./Birds/redvers.txt",
        "./Birds/skokholm.txt","./Birds/konza_waterfowl.txt","./Birds/konza_songbirds.txt",
        "./Birds/texas.txt","./Birds/easternwood.txt","./Birds/pawnee_birds.txt",
        "./Birds/pawnee_raptors.txt","./Birds/mountainbird_maine.txt","./Birds/mountainbird_ny.txt",
        "./Birds/mountainbird_vt.txt","./Birds/elverde.txt","./Fish/hinkleyflatfish.txt" ,"./Fish/hinkleygadoidfish.txt" ,"./Fish/northseademersal.txt",
        "./Fish/northseaflatfish.txt", "./Fish/northseagadoid.txt", "./Fish/northseapelagic.txt",
        "./Herps/luquillo.txt", "./Herps/boldparksnakes.txt", "./Herps/boldparklizards.txt", 
        "./Herps/cowleysnakes.txt", "./Herps/cowleylizards.txt", "./Herps/orabandasnakes.txt",
        "./Herps/orabandalizards.txt", "./Herps/fitchsnakes.txt", "./Herps/coweetasalamanders.txt",
        "./Herps/esgeorgeturtles.txt", "./Herps/rainbowfrogs.txt","./Invertebrates/cacoastmolluscs.txt",
        "./Invertebrates/hubbardleps.txt",'./Invertebrates/jornadapitC.txt',
        './Invertebrates/jornadapitG.txt','./Invertebrates/jornadapitM.txt','./Invertebrates/jornadapitT.txt','./Invertebrates/luquillosnails.txt',
        './Invertebrates/oneidazooplankton.txt','./Invertebrates/pacificarthropodsCAY.txt','./Invertebrates/pacificechinoderms.txt',
        './Invertebrates/pacificmolluscsBOA.txt','./Invertebrates/pacificmolluscsCAY.txt','./Invertebrates/pacificmolluscsGPT.txt',
        './Invertebrates/pacificmolluscsHAZ.txt','./Invertebrates/pacificmolluscsMCR.txt','./Invertebrates/pacificmolluscsOCC.txt',
        './Invertebrates/pacificmolluscsPSN.txt','./Invertebrates/pacificmolluscsSAD.txt','./Invertebrates/pacificmolluscsSHB.txt',
        './Invertebrates/ukbutterfliesA.txt','./Invertebrates/ukbutterfliesC.txt','./Invertebrates/ukbutterfliesF.txt',
        './Invertebrates/ukbutterfliesG.txt','./Invertebrates/ukbutterfliesM.txt','./Invertebrates/ukbutterfliesW.txt',
        "./Mammals/Portal/rodents_controls.txt","./Mammals/sevilleta/5pgrass.txt",
        "./Mammals/sevilleta/5plarrea.txt","./Mammals/sevilleta/rslarrea.txt",
        "./Mammals/Konza_Abund.txt","./Mammals/curlew.txt","./Mammals/INEEL.txt",
        "./Mammals/Jornadagrass.txt","./Mammals/Jornadashrub.txt","./Mammals/Powdermill/pdm_sqabund.txt",
        "./Mammals/Powdermill/pdm_rodabund.txt","./Mammals/shortgrass.txt","./Mammals/Ontario_rod.txt",
        "./Mammals/hilaire.txt","./Mammals/karoo_ungulates.txt","./Mammals/KrugerNP.txt",
        "./Mammals/goldengate.txt",
        './Plants/desertlab.txt', './Plants/desertlabshrub.txt', './Plants/Jornada_CS.txt',
        './Plants/Jornada_CW.txt', './Plants/Jornada_GS.txt', './Plants/Jornada_GW.txt',
        './Plants/Jornada_MS.txt', './Plants/Jornada_MW.txt', './Plants/Jornada_PS.txt',
        './Plants/Jornada_PW.txt', './Plants/Jornada_TS.txt', './Plants/Jornada_TW.txt',      
        './Plants/kansasannuals.txt', './Plants/kansasperennials.txt', './Plants/portalsummer.txt', 
        './Plants/portalwinter.txt', './Plants/steppe_counts.txt')
names=c("Hubbard","WhiteMountain","Redvers","Skokholm","KonzaWaterfowl","KonzaSongbirds",
        "Texas","EasternWood","Pawnee","PawneeRaptors","Maine","NewYork","GreenMountains",
        "Luquillo","HinkleyFlatfish" ,"HinkleyGadoidfish" ,"NorthseaDemersal",
        "NorthseaFlatfish", "NorthseaGadoid", "NorthseaPelagic","Luquillo", "BoldParkSnakes", "BoldParkLizards", "CowleyCountySnakes", 
        "CowleyCountyLizards", "OraBandaSnakes", "OraBandaLizards", "FitchSnakes", 
        "CoweetaSalamanders", "ESGeorgeTurtles", "RainbowBayFrogs","CACoastlineMolluscs","HubbardBrookLeps",'JornadaPitfallsCreosote',
        'JornadaPitfallsGrassland','JornadaPitfallsMesquite','JornadaPitfallsTarbush','LuquilloSnails',
        'OneidaLakeZooplankton','PacificCoastArthropodsCAY','PacificCoastEchinoderms',
        'PacificCoastMolluscsBOA','PacificCoastMolluscsCAY','PacificCoastMolluscsGPT',
        'PacificCoastMolluscsHAZ','PacificCoastMolluscsMCR','PacificCoastMolluscsOCC',
        'PacificCoastMolluscsPSN','PacificCoastMolluscsSAD','PacificCoastMolluscsSHB',
        'UKButterfliesAgricultural','UKButterfliesCoastal','UKButterfliesForest',
        'UKButterfliesGrassland','UKButterfliesMixed','UKButterfliesWetland',
        "Portal","Sev5pgrass","Sev5plarrea","Sevrslarrea","Konza","Curlew","INEEL","JornadaGrass",
        "JornadaShrub","Powdermillsq","Powdermillrod","Shortgrass","Ontario","Hilaire","KarooNP",
        "KrugerNP","GoldengateNP",
        'DesertLabOpen', 'DesertLabShrub', 'JornadaCS', 'JornadaCW',     
        'JornadaGS', 'JornadaGW', 'JornadaMS', 'JornadaMW',      
        'JornadaPS', 'JornadaPW', 'JornadaTS', 'JornadaTW',      
        'KansasAnnuals', 'KansasPerennials', 'PortalSummer',
        'PortalWinter', 'Steppe')

for(i in 1:length(files)) {
  
  dissim_results(files[i],names[i])
  
}

