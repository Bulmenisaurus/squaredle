"use strict";
(() => {
  // src/puzzleData.ts
  var puzzle = {
    sideLength: 31,
    letters: `oeonrsmctunrmrpunegyxolupetntmi
    unenommeeetomuirucciaamericiumt
    iiuiuuuhcolugliroodarmstadtiuml
    nmmuiiinheilpromethiumhbsddnmna
    oturomdenlfmupagsoaieytterbiumb
    rihityaiehmmiralxovcaliforniumo
    oompbdrktuiuudieeetstndfdumtblc
    bseaborgiummoinulkoieiruyueaies
    iumlueinuuulioremdcppenimorbrro
    urslbnonmnieniooiutihuriiicinmi
    eofaecroentgeniumuitnepnunuidtn
    dipdrrmdiscmeumcnruspihmmmrpnaa
    dmiiyabuhnomuicinrepocoauuyinmh
    rizulymeitneriumipovarsenictoun
    ilumlumzicueomnutcpiitpiuncpbin
    eetoisstrontiumiuroaslhsabilrsz
    inmruoaloyuelhuveutcpnoayuraaen
    nroumnshsmnarmiobtatilrrlditccm
    ohmuicnarftbuwnrihsiieupildilth
    tanundmyttriumaeaesnlastatinean
    psmoiaauhaimvumlnniiicnoouuunmd
    yshurmunimulairftiuuieteanmmmlr
    rieiiiouanmagnesiummgtraogiylou
    kuulnblravovrfgcmmeouhucnsuusne
    empdeirhhtrlsauaoerrouitfttcmio
    iuiltnteocduohdsndhdnlkoseaurfe
    uuihenirolhcfpioyeiciiettnrlatm
    muinaruuoemuilehnussrubidiumuht
    muillaggmgeivnuimfdegmuibretimu
    mumtuiimlueeuousahbismuthpebuup
    itrmlciruirnemycadmiumeaeeiimum`.replaceAll(" ", "").replaceAll("\n", "").toUpperCase(),
    words: "abdomen,abdomens,able,abnormal,abnormally,aces,ache,acid,acne,acre,actinium,actor,acts,actual,acute,acuter,adage,adages,adder,adders,addict,adds,admire,admirer,admires,admit,adobe,adore,adoring,adorn,aerate,aerial,aero,again,agar,ageing,ages,aging,ahoy,aide,aids,ails,aims,airbus,airs,airy,akin,alan,alarm,alarms,alas,alba,album,alec,aleph,alert,alerts,ales,algae,alias,aliases,alien,aliens,alines,alit,allay,allays,alliums,allot,allusion,allusions,ally,alma,alms,alps,also,altar,altars,alter,alto,alts,alum,aluminium,aluminum,alumni,amen,amend,americium,amid,amino,amir,ammo,ammonia,amount,ampere,amulet,amulets,amuse,amused,amuses,anemia,anemic,anger,angers,angst,animal,anime,anise,anises,anna,annal,annals,anneal,anon,another,ante,anthill,anti,antic,antics,antimony,antler,ants,anus,anyone,aorta,aortas,apes,appoint,appoints,approve,approver,approves,apps,apron,aprons,arables,arbiter,arcane,arced,arch,archer,arcs,area,areas,arena,arenas,argon,argument,arguments,aria,arias,arid,ariel,arise,armor,armories,armors,armory,armour,arms,army,aroma,aromas,arose,around,array,arrest,arse,arsenic,article,articles,arts,ascent,ashen,ashes,ashier,ashiest,ashore,asinine,aspen,assume,astatine,asthma,atom,atoms,atonal,atop,atrocities,attain,attains,attend,attest,attic,attics,attire,audio,audit,audits,augur,aunt,auntie,aura,aurora,auroras,authentic,author,auto,autos,autumn,autumns,avian,baas,babe,babes,babies,baby,badder,baddest,bade,bail,bails,bait,bale,bales,ball,ballad,balled,ballot,balls,balm,balms,bane,barb,barber,bard,bards,bare,bared,bares,barn,barred,barrel,barrels,barren,basal,base,based,bash,basin,basis,bates,bath,baths,bats,bays,bead,beadily,beady,beam,bear,beard,bearer,beat,beds,bedside,bedsides,beep,beeper,beer,beet,begs,beguile,being,bell,belle,bells,belt,bend,benign,bent,berate,beret,berg,berkelium,berry,beryllium,beside,besides,best,bests,beta,beth,bets,better,bible,bibles,bidder,bidders,bides,bids,bike,bile,bind,bins,bionic,biopic,biotic,birch,birches,bird,birdie,birth,bisect,bismuth,bite,biter,bites,bits,bitten,bitter,blab,blade,blam,blame,blames,blare,blared,blares,bleary,bled,blind,blip,blithe,blither,blob,bloc,blue,blueberry,bluer,blur,body,bohrium,boil,boing,bomb,bomber,bombers,bone,boom,booms,bore,boring,born,borne,boron,bounce,bounced,bouncer,bounty,bovine,brace,braces,braid,brain,bran,bras,brat,bratty,bray,breach,bread,bred,brent,bribe,bribes,brim,brine,brines,brit,brits,bromine,broth,broths,bruins,brunette,brunt,brute,bubba,bugs,build,built,bulb,bull,bulls,bummer,bump,bunnies,buns,bunt,bureau,burgess,burial,burlier,burp,bush,busiest,busy,butt,butte,butter,butts,buys,cacti,cadmium,caesium,cage,cagers,cages,cahoots,cain,calcium,calif,californium,calm,calmer,came,cameo,cameos,camo,cams,candid,candies,candle,candy,cane,caned,canes,canine,canoe,canoed,canoer,canon,cans,cant,canteen,canton,canyon,cape,caper,capes,capita,caps,captain,captains,caramel,carat,carb,carbon,carbs,card,care,cares,caret,carets,carl,carp,carps,cars,cart,carton,cartons,cartoon,cartoonist,cartoonists,carts,casa,case,cases,cash,cashes,cashier,casino,casinos,cast,caste,castle,casts,cater,catholic,catholics,catnip,cats,cattail,cattails,cause,caution,cave,cavern,caves,cease,cedar,celibate,cell,cells,cent,center,centre,cents,century,cesium,chad,chalice,chalices,champ,channel,chant,chaos,char,charlie,chase,chat,cheek,cheer,chef,chemist,chess,chide,chides,chin,chins,chip,chipper,chips,chlorine,chomp,chore,chorine,chose,chosen,chroma,chrome,chromium,chum,chums,church,cider,cigar,cilantro,cinema,cipher,citation,cite,cites,cities,citric,city,clad,clam,clammy,clan,clans,clasp,clause,clean,cleaner,cleans,cleric,client,clients,clinic,clip,clog,clot,cloth,clothe,club,coarse,coast,coasts,coat,coats,coattail,coauthor,cobalt,coca,coco,code,codes,cognate,cogs,cohost,coil,coin,coined,coins,coke,cold,cole,colon,color,colour,colt,column,coma,comas,comb,combat,come,comedic,comedies,comedy,comes,comet,comets,comma,command,commandeer,commander,commas,commend,comment,commit,commits,communal,commune,communes,communist,communists,commute,commuter,comp,cone,cones,confetti,conga,conic,conn,conned,cons,consent,constant,constants,contact,contacts,contend,content,contents,contest,cooed,cool,cools,cope,coper,copes,copier,copies,copper,cops,cord,cordon,cordoning,core,cores,corn,cornea,corneas,corned,corner,cornering,corps,corpse,corral,cory,cost,costa,costal,costar,costly,costs,cosy,cots,cougar,could,count,counter,courier,court,couth,cove,cover,coverup,coverups,coves,crab,craft,cram,crap,crate,crave,craves,cream,create,cred,cretin,crib,crimson,crises,crisis,crisp,crispier,crispiest,crop,crops,crouton,croutons,crud,cruel,cruise,cruised,cruises,crumb,crush,cueing,cuing,cull,cult,cumin,curate,curates,curd,curdle,cure,cures,curium,curl,curls,curt,curtail,curtails,curtain,curtains,cushion,cushioned,cushions,cute,cuter,cutest,cutie,cutler,cuts,cutter,dabs,dads,dairy,daisy,dale,dame,dammit,damn,damned,damns,dams,dandy,dare,darer,dares,dark,darken,darkened,darn,dart,darts,dash,data,date,dater,dates,daytime,daytimes,dead,deaf,deal,deals,dear,deary,debate,debates,debit,debt,debtor,debtors,debts,deco,decor,decry,deem,deer,defile,define,defines,definite,deform,deformities,deft,deftly,deli,delirium,dell,delt,demise,demo,demon,demons,demos,denier,denies,denim,dens,dent,dents,deny,derby,dermal,dermis,describe,destines,deter,detour,dharma,dial,dials,diaries,diary,dice,dices,dicier,died,dies,diet,dieter,diets,differ,differs,digress,digs,dilate,dilates,dilemma,dilemmas,dill,dills,dilute,dime,dimple,dimples,dine,diner,dines,dinner,dino,dinos,diode,diorama,dioramas,dips,dire,direct,dirk,dirt,disc,disco,discs,disguise,disguises,dish,diss,dissed,disses,ditto,dittos,dock,docs,doer,doers,does,doing,dole,dome,domes,dominion,domino,done,donor,dons,doom,dooms,door,dope,dopes,dork,dorm,dorms,dorsal,dosage,dosages,dose,dots,double,doubled,doubles,dough,drab,drag,drags,dragster,drain,drained,drake,drat,dread,dreads,dried,drill,drills,drip,drone,droned,drones,drool,drought,drug,druid,drum,drummer,drums,dryly,dubnium,duck,duct,duel,dues,duet,duff,dull,duly,dumb,dummy,dump,dune,dunes,duos,durable,dyes,dynamic,dynamics,dysprosium,each,eager,eagle,earl,earlier,earn,earns,earring,ears,earth,earthy,ease,eased,east,easter,eastern,eaten,eater,eats,echelon,echo,edit,editing,eels,eerie,egad,eight,einstein,einsteinium,either,elastic,elate,elates,elevate,elevates,elevator,elicit,elide,elite,elites,elitist,elms,else,elude,email,emails,embalm,eminent,emit,emits,emitter,emmy,emoticon,emotion,empty,emulate,emulates,emulsion,emulsions,emus,enact,enacts,enamor,enamors,encase,enchant,encore,encores,encounter,endemic,endless,endure,enemies,enemy,engine,engines,enormous,enormously,enroll,ensue,ensuing,ensure,entail,enter,entice,entices,entire,entitle,entitled,entrants,envoy,envoys,eons,epic,epics,episode,episodes,epoch,eras,erase,erased,eraser,erasure,erbium,erect,erratum,erred,errs,erudite,erudite,erupt,erupts,escape,escaper,escapes,escort,escorts,estate,esteem,etch,eternal,ethanol,ether,ethic,ethical,ethics,ethos,euphoria,euphorias,euro,europium,euros,evict,evicts,evil,evils,exotic,fable,fabled,fables,fade,fads,fall,falls,fame,famine,famines,farce,farced,fare,fared,farm,farms,fart,fashion,fashioned,fashions,fate,fear,feat,feats,feds,feign,fell,felon,felt,fend,feral,ferment,ferments,fermium,fern,ferns,fest,fester,fetal,fetus,fifth,fifths,fifty,file,filet,fileting,fill,fillet,film,filth,filthy,final,finale,find,fine,fines,finest,finite,fins,firm,firms,fiscal,fist,fitness,fits,fitted,fittest,five,flair,flame,flaming,flan,flap,flare,flared,flaw,flea,fleas,fled,flee,flip,fluid,fluoride,fluorine,flute,focus,foes,foil,fold,forbid,ford,fore,forest,forests,form,formal,format,former,forms,forum,forums,foster,foul,fouls,four,fours,fourth,frail,frame,francium,francs,frat,fraud,frauds,fray,free,frees,freest,fresco,frescos,friend,friendly,fries,fringe,fringes,fume,furnace,further,fuse,fused,fuses,fusion,fuss,fussed,fusses,gadolinium,gain,gala,gale,gall,gallium,gals,game,gamed,gamer,games,gamma,gamut,gamuts,gander,gang,gangs,garage,garages,garlic,garner,garners,gash,gaunt,gems,gene,genera,genie,genre,gent,gents,geologic,germ,german,germane,germanium,germs,gets,ghoul,giga,gill,gimme,ginger,gingers,girl,gist,gists,glad,glam,glamour,glee,glen,glue,gluer,glug,glum,glut,gluten,gnat,goal,goalie,goals,goat,goer,gold,golem,gone,good,goodies,goon,gore,gored,gores,gourd,grad,grade,grads,grail,grain,gram,grammar,grams,granite,grant,granted,gray,grid,grim,grime,grimes,grin,grind,grinned,grit,groin,gruel,grunt,grunted,guano,guiles,guilt,guinea,guise,guised,guises,gulf,gull,gullet,gulp,gums,gunmen,gunned,gunner,guru,gust,gusts,guts,gutter,guttural,guys,hafnium,hags,hail,hair,hale,half,halftime,hall,halo,halos,halt,halts,hamper,hams,hand,handprint,hare,hares,harm,harms,harp,harps,harry,hart,hash,hashes,haste,hastier,hastiest,hatch,hatchet,hate,hater,hates,hath,hats,haul,have,head,heal,healer,hear,heard,heat,heed,heel,heir,heirs,held,helium,hell,helm,help,helpful,helpless,hems,hence,hens,herb,herd,here,herein,hermit,hermits,hero,hesitate,hesitation,hide,hider,hides,hike,hill,hilt,hind,hint,hints,hippest,hippie,hippies,hippo,hippos,hips,hire,hiss,hissed,hisses,hits,hive,hoes,hold,hole,holes,holier,holiest,holmium,hologram,holograms,holt,home,homes,honda,hone,honed,hones,honey,hoot,hoots,hope,hormonal,hormone,horn,hornet,horns,horrid,horridly,horse,hose,host,hosts,hotel,hound,hour,hubris,hubs,hues,huge,hugs,hula,hulk,human,humanity,humanoid,humans,humming,hummus,humor,humored,humors,hump,hums,humus,hunch,hunched,hunches,hundred,hunt,hunts,hurdle,hurl,hurls,hurt,hush,huts,hydro,hydrogen,hyena,hyenas,hymn,hymns,ibis,ibises,ices,icier,iciest,icon,icons,idea,ideal,ideals,ideas,idiom,idioms,idiot,idle,idles,idly,idol,iffy,ignite,ignore,illusion,illusions,image,images,imagine,imagines,imaging,imam,imminent,immune,immutable,impale,impaled,impales,implies,implore,implores,inane,inbred,inch,inched,inches,incise,incised,incises,incite,income,incomer,incur,indent,indices,indict,indies,indium,indoor,inept,infer,infernal,infers,infest,inflame,infringe,infringes,infuse,infused,infuses,infusion,ingest,ingrain,ingrained,inherit,inherits,inhuman,initial,initially,initiate,initiates,initiation,inland,inlay,inlet,inlets,inmate,innate,inner,innermost,inns,insect,inset,insets,inside,insist,instil,insure,insurer,intact,intend,intent,inter,interact,interacts,intercom,interest,interests,interim,intern,internal,into,intone,intro,intros,intrusion,intuit,iodine,ionic,ions,iota,irate,ired,ires,iridium,iris,irises,iron,ironic,irons,irony,irritable,irritant,issue,issued,italic,item,items,iterate,iterates,keel,keels,keen,keener,kemp,keto,kiln,kilo,kind,kindred,king,kiss,kite,kiting,kits,kris,krypton,labor,laboring,lace,lacerate,laces,lacey,laden,lady,lags,laid,lain,lair,lairs,lama,lamb,lame,lameness,lament,laments,lamer,lamest,lamp,lance,lancer,land,landform,lane,lanes,lang,lantern,lanthanum,laps,lapse,lard,lars,laser,lash,lasso,lassos,last,lasts,latch,late,latent,later,latest,latrine,latte,latter,laura,laurel,lawn,lawrencium,lays,lead,leader,leaders,leads,leaf,leafs,lean,leanest,leans,leap,leapt,lear,learn,learnt,lease,leased,leaser,least,leech,leeched,leek,legit,lemma,lemmas,lemon,lemur,lemurs,lend,lent,lentil,leone,lept,less,lest,lethal,lethargic,lets,letter,liar,liars,libel,liberate,lice,license,lids,lied,lies,lieu,life,lift,lifted,lifts,like,lilac,lily,lima,lime,limes,limit,limo,limp,line,lineage,lineages,linear,lined,lineman,linen,liner,lines,ling,lint,lion,lioness,lionesses,lions,lipid,lips,lisp,list,lists,lite,liter,literal,lithium,litre,litter,litters,live,liver,lives,load,loads,loaf,loafer,loafers,loafs,lobe,loch,logic,logs,loin,lone,loom,loon,loony,loot,lord,lore,lotion,lots,loud,louie,louis,lube,lubed,lubes,lull,lulu,lumber,lumbers,lumen,lump,luna,lunar,lunch,luncheon,luncheon,lunches,lunchroom,lurch,lure,lures,lush,lust,lute,lutetium,lying,lyre,mace,maces,madam,made,madman,maestro,maestros,mage,mages,magnesium,mags,maid,maiden,mail,mailman,mails,maim,main,make,making,male,males,malice,mall,malls,malt,malts,mama,mana,mandarin,mane,manes,manga,manganese,manhattan,mania,manic,manor,mans,mansion,manta,mantle,manual,many,maps,mara,marc,mare,mares,margin,marginal,maria,marine,marital,mark,market,marketing,marking,marmot,marmots,marred,married,marries,marry,mars,marsh,marshal,marshals,mart,mash,mashes,mason,mass,masseur,mast,master,masts,match,mate,mater,math,mats,matt,maul,mauled,mauls,maxi,maya,maybe,mayo,mayor,mead,meal,meals,mealtime,mean,meanest,meanly,means,meant,meantime,meat,meatier,meats,mecca,medic,medics,medium,mediums,meet,meets,mega,meitnerium,meld,melee,melt,melts,melty,meme,memento,mementos,memo,memoir,memos,menace,menaces,mend,mention,mentor,menu,mercury,mere,merit,merits,merlin,mesa,mesh,meta,metal,meter,method,metre,metric,metro,mice,micro,mics,midair,midi,might,migrant,mild,mile,miles,mill,millennium,millenniums,milo,mime,mimed,mimic,mina,mince,mind,mindless,mine,mined,miner,mineral,miners,mines,mini,minibar,minima,minimum,mining,minion,minions,minor,minoring,minors,mint,mints,minus,minuses,minutia,mirage,mirages,mire,mires,mirror,miss,mist,mister,mistime,mists,mite,mitt,mittens,mnemonic,moan,moaned,moans,moat,moats,mobile,mobiles,mobs,mocha,mochas,modal,modals,mode,modem,modems,modes,modest,modi,mogul,moist,moisten,mold,mole,moles,molly,molt,molten,molts,molybdenum,moment,momentous,momentously,moments,momentum,moms,money,mono,monsieur,monte,month,months,monument,monuments,mooch,mood,mooing,moon,moor,moos,moose,moot,moral,morale,morals,morbid,more,morel,morels,morn,morning,morocco,moron,moronic,morose,morph,morphine,morphs,morse,morsel,mortal,mortals,mortar,moss,most,mote,motel,moth,mother,moths,motion,motor,mould,mound,mount,mourn,mouth,move,mover,movie,much,muck,mucous,mucus,muffin,mugs,mule,mules,mull,mulls,mumble,mumbles,mummies,mummy,mums,munch,munched,mural,murals,murmur,muscular,muse,mused,musers,muses,music,must,mutable,mutant,mutants,mutate,mutates,mute,mutes,muting,mutinies,mutter,mutton,mutual,mutually,myriad,myth,myths,nags,nail,naive,naiveties,name,named,naming,nana,naps,nasal,nastier,nation,natural,nature,nautical,near,neat,neater,nebula,nebulae,nebulas,nectar,nectars,need,needy,neigh,neither,neodymium,neon,neptunium,nerd,nerdy,ness,nest,nestle,nests,nether,nets,neuron,neuter,nice,nicer,niche,niches,nick,nickel,nickels,nickle,nifty,nine,nines,ninth,ninths,niobium,nitpick,nitro,nitrogen,nobelium,node,nodes,nods,noel,noir,noise,noisy,nomad,nomadic,nomads,nominee,noms,nonce,none,noon,noose,nordic,norm,normal,normally,norms,north,northern,nose,nosy,notch,note,notion,noun,nouns,nova,nude,nudes,nudity,nuke,null,nulls,numb,number,numbers,numerable,numeric,nuns,oafs,oars,oasis,oath,oaths,oats,oboe,oboes,obtain,obtains,occur,ocelot,offer,offers,often,ohms,oils,oily,olive,olives,omega,omelet,omelets,omen,omened,omens,omit,omits,omnivore,once,ones,onion,online,only,onto,onus,onuses,oomph,opacities,opal,opals,open,openly,opens,opine,opium,oppose,optic,optimum,opus,oral,orate,orator,orbit,orbits,orca,orcs,ordain,ordained,order,ores,orient,orients,origin,ornament,ornaments,osmium,other,otter,ouch,ought,ounce,ours,outcast,outcasts,outcry,outdid,outer,outermost,outlet,outlier,outline,outran,outrun,outruns,outs,oval,oven,over,overcoat,overrun,overt,oxygen,pact,pacts,page,pages,paid,pail,pain,pair,pairs,pale,paled,paler,pales,palladium,palled,palm,palms,pals,panorama,panoramas,pans,para,parcel,parcels,parch,pare,pared,parer,paris,parlor,parry,parse,part,parts,pass,past,pasta,pastor,path,paths,pats,patter,pave,paves,pear,peas,peat,peep,peer,pees,pelvic,penal,penalty,pens,pepper,perform,performer,pest,pestle,pesto,pests,petal,peter,petit,petite,pets,phase,phonic,phonics,phonier,phonies,phoniest,phosphoresce,phosphorescent,phosphorus,pick,picket,pickets,pickle,picnic,pics,picture,pictures,pier,piers,pies,pile,piled,piles,pileup,pileups,pilgrim,pilgrimage,pilgrimages,pilgrims,pill,pillar,pills,pimp,pine,pines,pinhole,pinholes,pins,pint,pints,pioneer,pipe,piper,pipes,pirate,pirates,pistachio,pistachios,pita,pitas,pities,pits,pity,place,places,placid,plaid,plain,platinum,play,plays,plea,pleas,plop,plug,plum,plume,plural,plurals,plus,plutonium,poach,poacher,pocket,pockets,podium,poet,poetic,poets,point,points,pointy,poke,polo,polonium,pops,porch,pore,pores,port,ports,pose,poser,posh,posit,post,postal,posture,potassium,pots,potter,pouch,pour,poured,pours,pouts,praseodymium,pray,prays,preach,preen,premium,prep,preps,prevail,prevails,price,pricy,prima,primal,prime,primer,primes,print,printer,prints,prior,prize,prizes,procure,procures,prom,promethium,promo,promote,promoter,promotes,proms,prop,proper,propose,props,pros,prose,prosper,protactinium,prove,prover,proves,prune,pruner,prunes,puke,pull,pullup,pulp,pulpit,puma,pumas,pump,punier,puns,punt,punts,pupa,pupae,puppet,puppets,puppies,pure,puree,purees,purer,purest,purist,purity,purr,purse,puts,rabid,rabies,race,racer,racers,races,radiate,radiates,radiator,radii,radio,radioing,radios,radium,radix,radon,raft,rage,rages,raging,rags,raid,raids,rail,rails,rain,rained,rake,raking,rally,ramen,rams,rancid,rand,rang,ransom,rant,ranted,rants,rapes,rapid,rapper,rare,rarer,rarest,rash,raspiest,rasps,raster,ratchet,rate,rater,rates,rather,rats,rave,raves,rays,reach,react,reactant,reactants,reacts,read,reader,readers,readily,reads,ready,real,realise,realist,realistic,realm,realms,reals,realtor,ream,reap,rear,reason,rebar,rebate,rebel,rebels,reborn,rebut,rebuts,recant,recipe,recipes,recital,recline,recommend,recon,recons,record,redness,redo,redone,redos,reds,reed,reenact,reenacts,rees,refashion,refashioned,refashions,refine,refines,reform,reformat,refs,regain,regal,regard,regime,reglue,reign,rein,reined,reins,reinstate,relax,reliance,reliant,relic,relics,relish,remarries,remedies,remedy,renal,rent,rental,rented,renter,rents,repair,repairs,repeat,repeats,repel,repent,repents,report,reports,reps,reroute,rescind,reside,resides,residue,residues,resin,resort,resorts,rest,restrict,rests,resume,retail,retailer,retailers,retain,retains,retention,reticle,reticles,retina,retinas,retire,retires,retried,retro,return,returns,reunion,reunite,reunites,revile,revisit,rhenium,rhino,rhodium,ribosome,ribs,rice,rich,richer,riches,ride,rider,ridley,rife,rift,rigor,rile,riled,riles,rims,rind,ring,rings,rinse,riot,rioter,ripe,ripen,ripens,riper,ripest,rips,rise,risen,rises,rite,rites,ritual,ritualistic,ritually,ritz,rival,river,rivet,rivets,roach,roam,roams,roar,roars,roast,roasts,robber,robe,robed,robin,robs,rode,rodent,rodents,roentgen,roger,rogue,roil,roils,role,roles,roll,rolls,roman,romans,romeo,romp,room,rooms,roomy,root,roots,rope,ropes,rose,rosy,rotate,rotation,rotational,rote,rotor,rots,rouge,rough,roulette,roulettes,round,route,router,rover,rubidium,rubs,ruby,rudd,rugs,ruin,ruined,ruins,rule,ruler,rules,rumination,rumor,rumored,rumour,rumple,rumpled,rumples,rune,runes,runic,runner,runnier,runny,runs,runt,runts,rupees,rural,ruse,rush,rust,rustic,ruth,ruthenium,rutherfordium,ruts,sacs,sadder,safe,safer,safes,safest,saga,sagas,sage,sages,sags,said,sail,sailor,sale,saline,saloon,salsa,salsas,salt,saltier,salts,salty,samarium,same,sameness,samurai,sanction,sand,sandier,sandy,sane,saner,sanity,sans,santo,sash,sass,sassily,satin,satire,satiric,satyr,sauce,saul,sauna,saunas,sausage,sausages,saute,scalar,scald,scale,scaly,scam,scammed,scammer,scan,scandium,scans,scant,scar,scare,scares,scarier,scatter,scene,scent,schema,schemas,scheme,schemer,sclerosis,scone,scones,scope,score,scot,scour,scout,scouts,scrap,scrape,scraper,scrapes,scrapper,scraps,scribe,scrub,scrum,scum,seabed,seabeds,seaborgium,seal,seals,seam,seams,seance,sear,seas,seat,seats,second,sect,sedate,sedates,seen,seer,selenium,sell,seller,sells,semi,send,sender,senile,senior,sent,sentinel,sepia,sept,septic,serf,serial,serrate,serum,serums,sets,settle,shale,shall,shalt,sham,shaman,share,shares,sharia,sharp,sharply,shatter,shea,shear,shes,shield,shin,shine,shined,shines,ship,ships,shire,shoal,shoo,shop,shore,shores,short,shorts,shot,shots,should,sick,sickle,sicko,side,sides,sienna,sign,silica,silicon,silk,sill,silly,silo,silos,silver,simmer,simmered,simulate,simulates,sing,sinister,sinned,sinner,sins,sinus,sinuses,siphon,siphons,sips,sire,siren,sires,sister,site,sits,situp,size,slab,slag,slain,slam,slams,slap,slash,sled,sleek,sleet,slept,slid,slim,slit,slits,sliver,slob,slop,slope,slosh,slot,sloth,slots,slouch,slum,slumber,slums,slur,slyly,small,smaller,smash,smear,smell,smelled,smells,smelt,smelts,smith,smog,smooth,smoothie,smug,snag,snags,snail,snap,snaps,snare,snarl,sneer,snide,snip,snipe,sniper,snipes,snippet,snippets,snore,snores,snot,soap,soar,soars,societal,sociopath,sociopaths,sock,socket,soda,sodas,sodium,sofa,soft,soften,softer,soil,soils,sold,sole,soles,solo,some,sonar,song,sonic,sonnet,sonny,sons,soon,sooner,soot,sore,sores,sorest,sorrily,sorry,sort,sorts,soul,soulmate,sour,soured,spam,spams,span,spans,spar,spars,sparse,spas,spasm,spat,spent,sperm,spiel,spies,spill,spilt,spin,spine,spines,spiral,spit,spits,splash,splay,splays,split,splits,spore,sport,sports,sportscaster,spray,sprays,spry,spur,stab,stabs,stain,stains,stair,stairs,stale,stall,stand,star,starch,stare,stares,stars,stash,stat,state,states,static,statics,station,stats,statue,stature,status,statute,stay,steadier,steal,steel,steep,steer,stein,stem,stems,step,stern,stick,stickler,stickmen,stiff,stifle,stiles,still,stilts,stint,stipend,stir,stirs,stoat,stoats,stock,stoic,stoke,stoma,stone,stoner,stood,stop,store,stored,storm,story,stout,stove,strait,strap,straps,strict,strip,stripe,stripes,strive,strives,strontium,strove,strum,strums,stub,stummed,stupid,stupidly,stupor,stupors,stutter,subs,subsea,such,sued,suede,suer,sues,sugar,sugars,suing,suit,sulfur,summary,sumo,sums,sunniest,sunny,suns,super,supper,support,supportive,supports,sure,surer,surf,surly,surreal,surrealist,surrealistic,sync,syndrome,synod,syrup,syrups,table,tables,tabs,tacit,taco,tacos,tact,tactic,tactile,tacts,tail,tailor,tails,talc,tale,talent,tall,tally,tame,tamer,tammy,tang,tango,tangos,tans,tape,taper,tapes,tapioca,tapiocas,taps,tardy,tare,tares,tarp,tarps,tars,tart,taste,tasted,taster,tastier,tastiest,tate,taunt,tavern,teal,team,tear,teas,tease,teased,teaser,teat,tech,technetium,tedium,teeing,teem,teems,teen,teepee,tell,tellurium,tenant,tend,tenet,tennis,tenor,tens,tense,tenser,tent,tenth,tenths,tents,tenure,tepee,tepid,terbium,term,terminal,termite,terms,terra,terrain,tesla,test,tester,tests,thallium,than,that,thatcher,thee,their,them,then,there,therein,thesis,thick,thicket,thickets,thin,thine,thinned,thinner,thinnest,thins,thirty,this,thorium,thorn,thorns,those,thou,threat,threaten,threatens,three,throb,throne,thru,thrum,thud,thug,thumb,thump,thyme,tiara,tibia,tick,ticket,tickets,tickle,tics,tidbit,tidbits,tide,tidier,tidies,tidiest,tidily,tidy,tied,tier,tiers,ties,tiger,tile,tiles,tiling,till,tilt,tilts,time,timed,timeout,timer,times,timid,tine,tines,ting,tinier,tiniest,tins,tint,tinted,tints,tipper,tips,tiptoe,tirade,tirades,tire,tires,titan,titanic,titanium,titans,title,titled,toast,toasts,toed,toenail,toes,tofu,toil,toilet,toilets,toils,told,toll,tome,tomes,tonal,tone,toner,tong,tongs,tonic,tons,tool,toot,tooth,toots,topic,torch,tore,torment,torn,toro,torus,tory,total,totally,tote,totes,tour,toured,tourism,tout,touts,toys,trace,traces,tract,tracts,trade,tradeoff,trader,traders,trades,trail,trails,train,trait,tram,trans,transfer,translation,translator,trap,trapper,traps,trash,trauma,tray,tread,treads,treat,triad,trial,tribal,tribe,tribune,tribunes,tricep,tried,trill,trim,trims,trio,trip,trivia,trivial,troll,trope,tropes,tropic,tropics,trout,trove,troves,troy,truancy,truant,truants,true,trues,truism,truisms,truly,truss,truth,truths,tryout,tsar,tuba,tube,tuber,tubers,tubes,tuck,tugs,tulip,tulips,tumble,tumbles,tummy,tumor,tumour,tumult,tuna,tune,tuner,tunes,tungsten,tunic,tunnel,turbine,turbines,turf,turn,turnip,turnips,turns,turnstile,tutor,tyrannic,uglier,ulcer,ulcers,umami,umber,umbrella,umlaut,umlauts,umpire,unbind,uncouth,uncut,undead,undid,undo,undoing,unheard,unhelpful,union,unions,unironic,unison,unisons,unit,unitard,unite,unites,unities,units,unity,unlike,unlit,unmarried,unreal,unrest,unrests,unripe,unroll,unroots,unseal,unsure,untie,untied,unties,until,untitled,unto,upend,upper,uproar,uproot,uproots,upscale,uranium,urban,urge,urged,urges,urinal,urinate,urinates,urine,urns,usable,usage,usages,used,user,users,uses,using,utopia,utopias,utter,vacant,vacantly,vacate,vacates,vaginal,vain,vanadium,vapor,vapors,vats,vein,venue,vest,veto,vets,vibrant,vice,vile,vine,vinegar,vinegars,vineries,vines,viral,visa,visit,visits,vista,vita,vital,voes,vote,voter,votes,wane,ware,warn,wine,wineries,wire,wren,xenon,yahoo,yams,yard,yarn,yarns,yeast,yeses,yield,yonder,yore,your,youth,youths,ytterbium,ytterbium,yttrium,yuan,zeal,zebra,zillion,zillions,zinc,zirconium,zits".split(",").map((w) => w.toUpperCase())
  };

  // src/monomitter.ts
  function monomitter(emitLatestOnSubscribe = false) {
    const callbacks = /* @__PURE__ */ new Set();
    let valueBeenSet = false;
    let latestValue = void 0;
    function publish(value) {
      valueBeenSet = true;
      latestValue = value;
      callbacks.forEach((callback) => callback(value));
    }
    function subscribe(callback) {
      callbacks.add(callback);
      if (emitLatestOnSubscribe && valueBeenSet) {
        callback(latestValue);
      }
      return {
        unsubscribe() {
          callbacks.delete(callback);
        }
      };
    }
    function clear() {
      callbacks.clear();
    }
    return {
      publish,
      subscribe,
      clear
    };
  }

  // src/puzzleLogic.ts
  var toCoordinate = (index, sideLength) => {
    return { x: index % sideLength, y: Math.floor(index / sideLength) };
  };
  var toIdx = (coordinate, sideLength) => {
    return coordinate.y * sideLength + coordinate.x;
  };
  var adjacent = (c, sideLength) => {
    return [
      { x: c.x + 1, y: c.y },
      // →
      { x: c.x, y: c.y - 1 },
      // ↑
      { x: c.x - 1, y: c.y },
      // ←
      { x: c.x, y: c.y + 1 },
      // ↓
      { x: c.x + 1, y: c.y + 1 },
      // ↘
      { x: c.x + 1, y: c.y - 1 },
      // ↗
      { x: c.x - 1, y: c.y - 1 },
      // ↖
      { x: c.x - 1, y: c.y + 1 }
      // ↙
    ].filter(({ x, y }) => 0 <= x && x < sideLength && 0 <= y && y < sideLength);
  };
  var findWord = (gridSize, grid, word, visited) => {
    if (word === "") {
      return visited;
    }
    if (visited.length === 0) {
      for (let i = 0; i < grid.length; i++) {
        if (grid[i] === word[0]) {
          const wordPath = findWord(gridSize, grid, word.slice(1), [i]);
          if (wordPath !== null) {
            return wordPath;
          }
        }
      }
      return null;
    } else {
      const lastLetterIdx = visited[visited.length - 1];
      for (const neighbor of adjacent(toCoordinate(lastLetterIdx, gridSize), gridSize)) {
        const neighborIdx = toIdx(neighbor, gridSize);
        if (visited.includes(neighborIdx)) {
          continue;
        }
        if (grid[neighborIdx] !== word[0]) {
          continue;
        }
        const wordPath = findWord(gridSize, grid, word.slice(1), [...visited, neighborIdx]);
        if (wordPath !== null) {
          return wordPath;
        }
      }
      return null;
    }
  };
  var PuzzleLogic = class {
    constructor(puzzleInfo) {
      this.puzzle = puzzleInfo;
      this.letterBuffer = [];
      this.userFindWord = monomitter();
    }
    handleKey(key) {
      if (key === "Escape") {
        this.letterBuffer = [];
      }
      if (key === "Backspace") {
        this.letterBuffer.pop();
      }
      if (key === "Enter") {
        this.userFindWord.publish(this.letterBuffer.join(""));
        this.letterBuffer = [];
      }
      if ("ABCDEFGHIJKLMNOPQRSTUVWXYZ".includes(key.toUpperCase())) {
        this.letterBuffer.push(key.toUpperCase());
      }
    }
    undoKey(key) {
      const removedKey = this.letterBuffer.pop();
      console.assert(removedKey === key, "Popped key does not match undo");
    }
    getCurrentWordPath() {
      const letterIndices = findWord(
        this.puzzle.sideLength,
        this.puzzle.letters,
        this.letterBuffer.join(""),
        []
      );
      if (letterIndices === null) {
        return null;
      } else {
        return letterIndices.map((c) => toCoordinate(c, this.puzzle.sideLength));
      }
    }
    getTileValues(words) {
      const values = [];
      for (let i = 0; i < this.puzzle.sideLength ** 2; i++) {
        values.push({ containing: 0, starting: 0 });
      }
      for (const word of words.allWords) {
        if (words.unlockedWords.has(word)) {
          continue;
        }
        const wordPosition = findWord(this.puzzle.sideLength, this.puzzle.letters, word, []);
        if (wordPosition === null) {
          throw new Error(`Could not find word ${word} on the grid`);
        }
        values[wordPosition[0]].starting++;
        for (const letter of wordPosition) {
          values[letter].containing++;
        }
      }
      return values;
    }
  };

  // src/puzzleRenderer.ts
  var circleToPath = (x, y, r) => {
    return `M ${x} ${y}
m ${r}, 0
a ${r},${r} 0 1,1, ${-r * 2},0
a ${r},${r} 0 1,1 ${r * 2},0`;
  };
  var PuzzleRenderer = class {
    constructor(puzzleInfo, container, lineElement) {
      this.puzzle = puzzleInfo;
      this.puzzleContainer = container;
      this.line = lineElement;
      this.letterTiles = this.createLetterTiles();
    }
    createLetterTiles() {
      const tiles = [];
      for (const letter of this.puzzle.letters) {
        const letterContainer = document.createElement("div");
        letterContainer.classList.add("letter-container");
        const letterElement = document.createElement("div");
        letterElement.innerText = letter;
        const startNumElement = document.createElement("div");
        startNumElement.classList.add("start-num");
        startNumElement.innerText = "?";
        const includeNumElement = document.createElement("div");
        includeNumElement.classList.add("include-num");
        includeNumElement.innerText = "?";
        letterContainer.append(letterElement, startNumElement, includeNumElement);
        this.puzzleContainer.append(letterContainer);
        tiles.push(letterContainer);
      }
      return tiles;
    }
    renderWordLine(word) {
      const gapSize = 800 * (1 / 100);
      const nonGap = 800 - (this.puzzle.sideLength - 1) * gapSize;
      const tileSize = nonGap / this.puzzle.sideLength;
      const centerDist = tileSize + gapSize;
      const offset = tileSize / 2;
      const formattedPoints = word.map(({ x, y }) => ({ x: x * centerDist + offset, y: y * centerDist + offset })).map(({ x, y }, idx) => {
        if (idx === 0) {
          const circle = circleToPath(x, y, 10);
          return `${circle} M ${x.toFixed(1)} ${y.toFixed(1)}`;
        } else {
          return `L ${x.toFixed(1)} ${y.toFixed(1)}`;
        }
      }).join(" ");
      this.line.setAttribute("d", formattedPoints);
    }
    drawTileValues(tileValues) {
      const formatNum = (n) => {
        if (n === 0) {
          return "";
        }
        if (n >= 10) {
          return "+";
        }
        return `${n}`;
      };
      for (let i = 0; i < tileValues.length; i++) {
        const tile = this.letterTiles[i];
        const start = tile.querySelector(".start-num");
        const contains = tile.querySelector(".include-num");
        start.innerText = formatNum(tileValues[i].starting);
        contains.innerText = formatNum(tileValues[i].containing);
        if (tileValues[i].starting === 0 && tileValues[i].containing === 0) {
          tile.classList.add("eliminated");
        }
      }
    }
  };

  // src/wordManager.ts
  var startLettersRevealed = (length) => {
    let num = 1;
    if (length >= 6)
      num++;
    if (length >= 12)
      num++;
    return num;
  };
  var endLettersRevealed = (length) => {
    let num = 0;
    if (length >= 7)
      num++;
    if (length >= 8)
      num++;
    if (length >= 10)
      num++;
    return num;
  };
  var wordHint = (word) => {
    const start = startLettersRevealed(word.length);
    const end = endLettersRevealed(word.length);
    const numHidden = word.length - start - end;
    return word.slice(0, start) + "*".repeat(numHidden) + word.slice(word.length - end);
  };
  var WordManager = class {
    constructor(words, popupWordContainer) {
      this.allWords = new Set(words);
      this.unlockedWords = this.loadLocalStorage();
      this.popupWordContainer = popupWordContainer;
      this.wordElements = /* @__PURE__ */ new Map();
      this.renderPopup();
    }
    loadLocalStorage() {
      const saved = localStorage.getItem("words");
      if (saved !== null) {
        return new Set(saved.split(","));
      } else {
        return /* @__PURE__ */ new Set();
      }
    }
    saveToLocalStorage(words) {
      localStorage.setItem("words", Array.from(words).join(","));
    }
    trySubmitWords(word) {
      if (!this.allWords.has(word)) {
        return "invalidWord";
      }
      if (this.unlockedWords.has(word)) {
        return "alreadyGuessed";
      }
      this.unlockedWords.add(word);
      this.saveToLocalStorage(this.unlockedWords);
      const wordElement = this.wordElements.get(word);
      if (wordElement === void 0) {
        console.error(`Word ${word} has no corresponding HTML element`);
      } else {
        wordElement.innerText = word.toLowerCase();
      }
      return "success";
    }
    renderPopup() {
      const longestWordLength = Math.max(...Array.from(this.allWords).map((w) => w.length));
      for (let i = 4; i <= longestWordLength; i++) {
        const heading = document.createElement("h2");
        heading.innerText = `${i} letters`;
        const words = Array.from(this.allWords).filter((w) => w.length === i).sort();
        const wordGroupContainer = document.createElement("div");
        wordGroupContainer.classList.add("word-group");
        wordGroupContainer.style.setProperty("--word-length", `${i + 2}ch`);
        for (const word of words) {
          const wordContainer = document.createElement("span");
          this.wordElements.set(word, wordContainer);
          if (this.unlockedWords.has(word)) {
            wordContainer.innerText = word.toLowerCase();
          } else {
            wordContainer.innerText = wordHint(word.toLowerCase());
          }
          wordGroupContainer.append(wordContainer);
        }
        this.popupWordContainer.append(heading, wordGroupContainer);
      }
    }
  };

  // src/script.ts
  var main = () => {
    const puzzleContainer = document.getElementById("puzzle-container");
    const lineContainer = document.getElementById("line-highlight");
    const wordsFoundContainer = document.getElementById("words-found");
    const wordsTotalContainer = document.getElementById("words-left");
    const wordsPopup = document.getElementById("words-popup");
    const popupContent = document.getElementById("words-container");
    const openPopupButton = document.getElementById("word-opener");
    const closePopupButton = document.getElementById("close");
    let isPopupOpen = false;
    const openPopup = () => {
      wordsPopup.style.display = "block";
      isPopupOpen = true;
    };
    const closePopup = () => {
      wordsPopup.style.display = "none";
      isPopupOpen = false;
    };
    const puzzleRenderer = new PuzzleRenderer(puzzle, puzzleContainer, lineContainer);
    const puzzleLogic = new PuzzleLogic(puzzle);
    const wordManager = new WordManager(puzzle.words, popupContent);
    const tileValues = puzzleLogic.getTileValues(wordManager);
    puzzleRenderer.drawTileValues(tileValues);
    wordsFoundContainer.innerText = wordManager.unlockedWords.size.toString();
    wordsTotalContainer.innerText = wordManager.allWords.size.toString();
    window.addEventListener("keydown", (e) => {
      if (isPopupOpen) {
        if (e.key === "Escape") {
          closePopup();
        }
        return;
      }
      const key = "abcdefghijklmnopqrstuvwxyz".includes(e.key) ? e.key.toUpperCase() : e.key;
      puzzleLogic.handleKey(key);
      const currentPath = puzzleLogic.getCurrentWordPath();
      if (currentPath !== null) {
        puzzleRenderer.renderWordLine(currentPath);
      } else {
        puzzleLogic.undoKey(key);
      }
    });
    puzzleLogic.userFindWord.subscribe((word) => {
      const response = wordManager.trySubmitWords(word);
      if (response === "success") {
        const tileValues2 = puzzleLogic.getTileValues(wordManager);
        puzzleRenderer.drawTileValues(tileValues2);
        wordsFoundContainer.innerText = wordManager.unlockedWords.size.toString();
      }
    });
    openPopupButton.addEventListener("click", openPopup);
    closePopupButton.addEventListener("click", closePopup);
  };
  main();
})();
