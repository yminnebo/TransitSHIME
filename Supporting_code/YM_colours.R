# Author: Yorick Minnebo
# Date: 08/2021

# Colormagic
# Convenient place for all colors in TransitSHIME r project
# Colours are created with rgb.to, colorhunt.co, color-hex.com and medialab.github.io/iwanthue/
message(crayon::white("Colours are created with "), crayon::hyperlink(crayon::blue("rgb.to"), "https://rgb.to/"), crayon::white(", "),
        crayon::hyperlink(crayon::blue("colorhunt.co"), "https://colorhunt.co/"),crayon::white(", "),
        crayon::hyperlink(crayon::blue("color-hex.com"), "https://color-hex.com/"),crayon::white(" and "),
        crayon::hyperlink(crayon::blue("iwanthue"), "https://medialab.github.io/iwanthue//"))

# Colours
sixdonorsFSPD2 = c("#A9D08E","#8EA9DB","#FF9999","#C8BCD7","#FBD2B0","#B2DCE6", "#A9D08E","#8EA9DB","#FF9999",
                   "#C8BCD7","#FBD2B0","#B2DCE6","#A9D08E","#8EA9DB","#FF9999","#C8BCD7","#FBD2B0","#B2DCE6")
# sixdonors = c("#A9D08E","#8EA9DB","#FF9999","#C8BCD7","#FBD2B0","#B2DCE6")
sixdonors <- c("#974908", "#089648", "#480896", "#085696", "#960856", "#569608")
sixdonorsmapped = c("1" = "#974908", "2" = "#089648", "3" = "#480896",
                    "4" = "#085696", "5" = "#960856", "6" = "#569608")
# sixdonors = c("#6A8259","#596A89","#B26B6B","#8F81A1","#CA9973","#75A5B2")
morecolors = c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", 
               "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88",
               "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D","#8A7C64", "#599861")
fourcolours = c("#d18fa9","#d1b78f","#8fd1b7","#8fa9d1")

transitcolors = c("ST" = "#7ACD74", "MT" = "#757BCD", "LT" = "#CD747A")
transitcolors2 =c("STD" = "#7ACD74", "MTD" = "#757BCD", "LTD" = "#CD747A")
transitcolours = c("ST" = "#7ACD74", "MT" = "#757BCD", "LT" = "#CD747A")
inttransitcolours = c("Short transit donor" = "#7ACD74", "Medium transit donor" = "#757BCD", "Long transit donor" = "#CD747A")

desired = c("Acidaminococcus"="#DA5724",  "Akkermansia"="#94cfd5", "Alistipes"="#3F4921",
            "Bacteroides"="#DAA1AD", "Bifidobacterium"="#CBD588", "Bilophila"="#5F7FC7", "Blautia"="#673770", 
            "Cloacibacillus" = "#D7C1B1", "Dialister"= "#FAFD7C","Enterobacteriaceae"="#38333E", 
            "Clostridium_XlVa" = "#689030", "Eisenbergiella" = "#508578", "Lachnospiraceae"="#CD9BCD",
            "Megamonas" ="#D14285","Megasphaera"= "#6DDE88", "Mitsuokella" ="#652926", "Parabacteroides"="#fff8dc",
            "Phascolarctobacterium"="#5E738F", "Prevotella"= "#C84248", "Pseudomonas"="#D1A33D", "Roseburia"="#8A7C64", 
            "Ruminococcus" = "#AD6F3B", "Ruminococcaceae" = "#CC0C00", "Sutterella" = "#046582", "Veillonella" = "#F8698C", 
            "Other"="#bfbfbf")

OTUcolors = c("OTU1" = "#689030",	"OTU2" = "#D14285",	"OTU3" = "#38333E",	"OTU4" = "#5F7FC7",	"OTU5" = "#C0717C",	
              "OTU6" = "#C0717C",	"OTU7" = "#CBD588",	"OTU8" = "#94cfd5",	"OTU9" = "#D1A33D",	"OTU10" = "#C0717C",
              "OTU11" = "#652926",	"OTU12" = "#C0717C",	"OTU13" = "#C0717C",	"OTU14" = "#38333E",	"OTU15" = "#5E738F",	
              "OTU16" = "#C0717C",	"OTU17" = "#C0717C",	"OTU18" = "#CD9BCD",	"OTU19" = "#7FDCC0",	"OTU20" = "#AD6F3B",
              "OTU21" = "#94cfd5",	"OTU22" = "#673770",	"OTU23" = "#89C5DA",	"OTU24" = "#CD9BCD",	"OTU25" = "#D7C1B1",
              "OTU26" = "#e84a5f",	"OTU27" = "#8A7C64",	"OTU28" = "#C0717C",	"OTU29" = "#7FDCC0",	"OTU30" = "#7FDCC0",
              "OTU31" = "#673770",	"OTU32" = "#5E738F",	"OTU33" = "#79d70f",	"OTU34" = "#3F4921",	"OTU35" = "#D3D93E",	
              "OTU36" = "#3F4921",	"OTU37" = "#689030",	"OTU38" = "#ff5200",	"OTU41" = "#000000",	"OTU42" = "#CBD588",
              "OTU43" = "#5F7FC7",	"OTU44" = "#6DDE88",	"OTU45" = "#3F4921",	"OTU46" = "#689030",	"OTU47" = "#000000",
              "OTU48" = "#689030",	"OTU49" = "#C0717C",	"OTU50" = "#000000",	"OTU51" = "#000000",	"OTU52" = "#689030",
              "OTU53" = "#689030",	"OTU54" = "#8569D5",	"OTU55" = "#000000",	"OTU56" = "#000000",	"OTU57" = "#C0717C",
              "OTU58" = "#689030",	"OTU59" = "#000000",	"OTU60" = "#673770",	"OTU62" = "#000000",	"OTU63" = "#000000",	
              "OTU64" = "#C0717C",	"OTU65" = "#AD6F3B",	"OTU66" = "#673770",	"OTU67" = "#D14285",	"OTU68" = "#D1A33D",
              "OTU69" = "#000000",	"OTU71" = "#D3D93E",	"OTU72" = "#000000",	"OTU73" = "#f2ed6f",	"OTU74" = "#D3D93E",
              "OTU75" = "#CD9BCD",	"OTU76" = "#000000",	"OTU77" = "#AD6F3B",	"OTU78" = "#CD9BCD",	"OTU79" = "#689030",
              "OTU82" = "#AD6F3B",	"OTU83" = "#8A7C64",	"OTU84" = "#C0717C",	"OTU85" = "#673770",	"OTU86" = "#C0717C",
              "OTU88" = "#AD6F3B",	"OTU90" = "#CD9BCD",	"OTU91" = "#000000",	"OTU93" = "#000000",	"OTU95" = "#000000",
              "OTU99" = "#000000",	"OTU101" = "#3F4921",	"OTU102" = "#000000",	"OTU103" = "#000000",	"OTU104" = "#CD9BCD",	
              "OTU105" = "#673770",	"OTU108" = "#CD9BCD",	"OTU113" = "#000000",	"OTU114" = "#000000",	"OTU117" = "#000000",	
              "OTU132" = "#CD9BCD",	"OTU138" = "#000000",	"OTU145" = "#3F4921",	"OTU165" = "#3F4921",	"OTU181" = "#AD6F3B",	
              "Other" = "#bfbfbf")

Top15colors = c("Akkermansia"="#1a1259","Alistipes" = "#CD9BCD", "Bacteroides"="#3194cd", "Bifidobacterium" ="#233938", 
                "Bilophila"="#0d654d", "Blautia"="#149700", "Enterobacteriaceae"="#85cb4f", "Lachnoclostridium"="#63415a",
                "Lachnospiraceae"="#937777", "Megamonas"="#503365", "Mitsuokella"="#874444", "Other"="#bfbfbf", 
                "Parabacteroides"= "#874444", "Phascolarctobacterium"="#d86d36", "Pseudomonas" ="#883488", "Ruminococcus"="#fbaa22")

Top15levels = c("Acidaminococcus", "Akkermansia","Alistipes" , "Anaeroglobus", "Anaerostipes", "Bacteria", "Bacteroides", 
                "Bifidobacterium" , "Bilophila",  "Blautia", "Cloacibacillus","Clostridiales" ,"Clostridium_XlVa",  "Coprococcus", "Desulfovibrio", 
                "Dialister", "Dorea", "Eisenbergiella", "Enterobacteriaceae", "Erysipelotrichaceae", "Faecalibacterium", 
                "Fusicatenibacter", "Fusobacterium",  "Lachnoclostridium","Lachnospiraceae", "Megamonas", "Megasphaera" , "Mitsuokella", 
                "Parabacteroides", "Phascolarctobacterium", "Prevotella", "Pseudomonas", "Roseburia", "Ruminococcaceae",
                "Ruminococcaceae_unclassified", "Ruminococcus", "Ruminococcus2", "Sutterella", "Veillonella", "Other")




Top15levels = c("Achromobacter",	"Acidaminococcus",	
                "Actinobacteria",	
                "Akkermansia",	"Alistipes",	
                "Alphaproteobacteria",
                "Anaeroglobus",	"Anaerostipes",	"Anaerotruncus",	"Asaccharobacter",	
                "Bacillales",
                "Bacillus",	"Bacteria",	
                # "Bacteria_Firmicutes_Clostridia_Clostridiales_Lachnospiraceae_Ruminococcus",	
                # "Bacteria_Firmicutes_Clostridia_Clostridiales_Ruminococcaceae_Ruminococcus",	
                "Bacteroidales",
                "Bacteroides",	"Barnesiella",	"Bifidobacterium",	"Bilophila",	"Blautia",	"Burkholderia",	
                "Butyricicoccus",	"Butyricimonas",	"Cloacibacillus",	"Clostridiales",	
                # "Clostridiales_Incertae_Sedis_XIII_unclassified",	
                "Clostridium_IV",	"Clostridium_sensu_stricto",	"Clostridium_XI",	"Clostridium_XlVa",	"Clostridium_XlVb",	"Clostridium_XVIII",
                "Collinsella",	"Coprococcus",	"Coriobacteriaceae_unclassified",	"Desulfovibrio",	"Dialister",	"Dorea",	"Eggerthella",	
                "Eisenbergiella",	"Enterobacteriaceae",	"Enterococcus",	"Erysipelotrichaceae",	"Escherichia/Shigella",	
                "Eubacteriaceae",
                "Eubacterium",	"Faecalibacterium",	"Faecalicoccus",	
                "Firmicutes",	
                "Flavonifractor",	"Fusicatenibacter",	
                "Fusobacterium",
                "Gammaproteobacteria",
                "Holdemanella",	"Holdemania",	"Intestinibacter",	"Intestinimonas",	
                "Lachnospiraceae",	"Lactobacillus",	"Lactococcus",	"Lysinibacillus",	"Megamonas",	"Megasphaera",	"Mitsuokella",	"Murimonas",	
                "Odoribacter",	"Oscillibacter",	"Parabacteroides",	"Paraprevotella",	"Parasutterella",	"Pediococcus",	"Peptoniphilus",
                "Peptostreptococcaceae",	
                "Peptostreptococcus",	"Phascolarctobacterium",	"Prevotella",	"Pseudoflavonifractor",	
                "Pseudomonadaceae",
                "Pseudomonas",	"Rhizobium",	"Romboutsia",	"Roseburia",	"Ruminococcus", "Ruminococcaceae",	"Senegalimassilia",	
                "Slackia",	"Stenotrophomonas",	"Streptococcus",	"Subdoligranulum",	"Succiniclasticum",	"Sutterella",	"Terrisporobacter",	
                "Veillonella",	
                "Veillonellaceae",	
                "Victivallis")

# "Actinobacteria_unclassified",
# "Alphaproteobacteria_unclassified",
# "Bacillales_unclassified",
# "Bacteroidales_unclassified",
# "Eubacteriaceae_unclassified",
# "Gammaproteobacteria_unclassified",
# "Peptostreptococcaceae_unclassified",
# "Pseudomonadaceae_unclassified",
# "Veillonellaceae_unclassified",





Interaction.colours = c("1:Faecal sample"="#c2deaf", "2:Faecal sample"="#afc2e5", "3:Faecal sample"="#ffb7b7", "4:Faecal sample"="#d8d0e3",
                        "5:Faecal sample"="#fcdfc7", "6:Faecal sample"="#c9e6ed", "1:100%"="#A9D08E",  "2:100%"="#8EA9DB", "3:100%"="#FF9999",
                        "4:100%"="#C8BCD7", "5:100%"="#FBD2B0", "6:100%"="#B2DCE6", "1:33%"="#769163", "2:33%"="#637699", "3:33%"="#b26b6b", 
                        "4:33%"="#8c8396", "5:33%"="#af937b", "6:33%"="#7c9aa1", "1:20%"="#435338", "2:20%"="#384357", "3:20%"="#663d3d",
                        "4:20%"="#504b56", "5:20%"="#645446", "6:20%"="#47585c", "1:10%"="#10140e", "2:10%"="#0e1015", "3:10%"="#190f0f",
                        "4:10%"="#141215", "5:10%"="#191511", "6:10%"="#111617")

donorcolors <- c("P" = "#cdb174", "D" = "#a48d5c")
vesselcolors <- c("Proximal" = "#cdb174", "Distal" = "#a48d5c", "P" = "#cdb174", "D" = "#a48d5c")
sixdonors <- c("1" = "#FF410D", "2"="#6EE2FF",  "3"="#95CC5E", "4"="#D0DFE6", "5"="#F79D1E", "6"="#748AA6")
sixdonors <- c("1" = "#e84629", "2"="#5a41cf",  "3"="#6bdb43", "4"="#837c78", "5"="#d4e130", "6"="#ea3bb9")


sixdonors <- c("1" = "#4E9F3D", "2"="#3C4D9F",  "3"="#9F3C4D", "4"="#8F3C9F", "5"="#9F8F3C", "6"="#3C9F8F")
sixdonors <- c("1" = "#9C19E0", "2"="#E19B19",  "3"="#878987", "4"="#91e119", "5"="#19e1cd", "6"="#e11991")
sixdonors <- c("1" = "#9C19E0", "2"="#19e1cd",  "3" = "#878987", #"3"="#19E19B",
               # "4"="#5FE119", 
               "4"="#91e119",
               # "5"="#195FE1", 
               "5"="#E19B19", 
               # "6"="#E1195F"
               "6"="#e11991")
