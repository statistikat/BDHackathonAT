library(data.table)
setwd("/data")
csk <- fread("cedefop/ft_skill_profession_en.csv",header=FALSE)
setnames(csk,colnames(csk),c("GeneralId","PublicationCountry","Esco_Level_4","Esco_Level_3",
                             "Esco_Level_2","Esco_Level_1","Skill_Esco_Level_4","Skill_Esco_Level_3",
                             "Skill_Esco_Level_2","Skill_Esco_Level_1","Skill_Esco_Level_0"))
cskr <- fread("cedefop/ft_skill_profession_requirement_en.csv")


doc <- fread("cedefop/ft_document_en.csv",header=FALSE)
doc