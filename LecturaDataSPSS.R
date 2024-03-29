library(tidyverse)
library(datos)
library(gapminder)
library(Lahman)
library(nycflights13)
library(remotes)
library(foreign)


ReadSPSSData <- function() {
  ruta.de.archivo <- file.choose()
  dataset_GSS <- read.spss(ruta.de.archivo, to.data.frame = TRUE)
  tb_datos_GSS <- as_tibble(dataset_GSS)
  return(tb_datos_GSS)
}


variable.index <- c("YEAR:GSS YEAR FOR THIS RESPONDENT", "ID:RESPONDENT ID NUMBER", "WRKSTAT:LABOR FORCE STATUS", 
                    "HRS1:NUMBER OF HOURS WORKED LAST WEEK", "HRS2:NUMBER OF HOURS USUALLY WORK A WEEK", 
                    "MARITAL:MARITAL STATUS", "MARTYPE:MARITAL TYPE", 
                    "PRESTG105PLUS:R'S OCCUPATIONAL PRESTIGE SCORE USING THRESHOLD METHOD (2010)", 
                    "DIVORCE:EVER BEEN DIVORCED OR SEPARATED", "WIDOWED:EVER BEEN WIDOWED", 
                    "COIND10:PARTNER'S INDUSTRY CODE (2010)", "MAOCC10:MOTHERS CENSUS OCCUPATION CODE (2010)",
                    "SIBS:NUMBER OF BROTHERS AND SISTERS", "CHILDS:NUMBER OF CHILDREN", "AGE:AGE OF RESPONDENT", 
                    "AGEKDBRN:R'S AGE WHEN 1ST CHILD BORN", "EDUC:HIGHEST YEAR OF SCHOOL COMPLETED", 
                    "PAEDUC:HIGHEST YEAR SCHOOL COMPLETED, FATHER", "MAEDUC:HIGHEST YEAR SCHOOL COMPLETED, MOTHER", 
                    "SPEDUC:HIGHEST YEAR SCHOOL COMPLETED, SPOUSE", "COEDUC:HIGHEST YEAR SCHOOL COMPLETED, PARTNER", 
                    "DEGREE:R'S HIGHEST DEGREE", "PADEG:FATHER'S HIGHEST DEGREE", "MADEG:MOTHERS HIGHEST DEGREE", 
                    "SPDEG:SPOUSE'S HIGHEST DEGREE", "SEX:RESPONDENTS SEX","RACE:RACE OF RESPONDENT", 
                    "BORN:WAS R BORN IN THIS COUNTRY", "PARBORN:WERE R'S PARENTS BORN IN THIS COUNTRY", 
                    "GRANBORN:HOW MANY GRANDPARENTS BORN OUTSIDE U.S.", "HOMPOP:NUMBER OF PERSONS IN HOUSEHOLD", 
                    "EARNRS:HOW MANY IN FAMILY EARNED MONEY", "INCOME:TOTAL FAMILY INCOME", 
                    "RINCOME:RESPONDENTS INCOME", "INCOME16:TOTAL INCOME", "RINCOME16:RESPONDENTS INCOME", 
                    "PARTYID:POLITICAL PARTY AFFILIATION", "VOTE12:DID R VOTE IN 2012 ELECTION", 
                    "PRES12:VOTE FOR ...", "VOTE16:DID R VOTE IN 2016 ELECTION", "PRES16:VOTE FOR ...", 
                    "IF12WHO:WHO WOULD R HAVE VOTED FOR-2012 ELECTION", "IF16WHO:WHO WOULD R HAVE VOTED FOR-2016 ELECTION", 
                    "POLVIEWS:THINK OF SELF AS LIBERAL OR CONSERVATIVE",
                    "SPKRAC:ALLOW RACIST TO SPEAK", "COLRAC:ALLOW RACIST TO TEACH", "LIBRAC:ALLOW RACISTS BOOK IN LIBRARY", 
                    "SPKCOM:ALLOW COMMUNIST TO SPEAK", "COLCOM:SHOULD COMMUNIST TEACHER BE FIRED", 
                    "LIBCOM:ALLOW COMMUNISTS BOOK IN LIBRARY", "SPKMIL:ALLOW MILITARIST TO SPEAK", 
                    "COLMIL:ALLOW MILITARIST TO TEACH", "LIBMIL:ALLOW MILITARISTS BOOK IN LIBRARY", 
                    "SPKHOMO:ALLOW HOMOSEXUAL TO SPEAK", "COLHOMO:ALLOW HOMOSEXUAL TO TEACH", 
                    "LIBHOMO:ALLOW HOMOSEXUALS BOOK IN LIBRARY", "SPKMSLM:ALLOW MUSLIM CLERGYMEN PREACHING HATRED OF THE US", 
                    "COLMSLM:ALLOW ANTI-AMERICAN MUSLIM CLERGYMEN TEACHING IN COLLEGE", 
                    "LIBMSLM:ALLOW ANTI-AMERICAN MUSLIM CLERGYMEN'S BOOKS IN LIBRARY", "HAPPY:GENERAL HAPPINESS", 
                    "HAPMAR:HAPPINESS OF MARRIAGE", "HAPCOHAB:HAPPINESS OF RELT WITH PARTNER", "HEALTH:CONDITION OF HEALTH", 
                    "LIFE:IS LIFE EXCITING OR DULL", "FAIR:PEOPLE FAIR OR TRY TO TAKE ADVANTAGE", 
                    "TRUST:CAN PEOPLE BE TRUSTED", "SATFAM7:FAMILY LIFE", "SATJOB:JOB LIFE", 
                    "SATLIFE:LIFE SATISFACTION", 
                    "SATSOC:SOCIAL SATISFACTION", "FEHIRE:SHOULD HIRE AND PROMOTE WOMEN", 
                    "FEAR:AFRAID TO WALK AT NIGHT IN NEIGHBORHOOD", "FECHLD:MOTHER WORKING DOESN'T HURT CHILDREN", 
                    "FEJOBAFF:FOR OR AGAINST PREFERENTIAL HIRING OF WOMEN", 
                    "ABANY:ABORTION IF WOMAN WANTS FOR ANY REASON", "PREMARSX:SEX BEFORE MARRIAGE", 
                    "TEENSEX:SEX BEFORE MARRIAGE:TEENS 14-16", "XMARSEX:SEX WITH PERSON OTHER THAN SPOUSE", 
                    "HOMOSEX:HOMOSEXUAL SEX RELATIONS")




