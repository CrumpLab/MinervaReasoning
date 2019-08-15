# Drug name generator

library(tidyverse)

# 40 common drug names, with the beginings and endings split

drugs <- data.frame(stringsAsFactors=FALSE,
    Original = c("Acetaminophen", "Adderall", "Alprazolam", "Amitriptyline",
                 "Amlodipine", "Amoxicillin", "Ativan", "Atorvastatin",
                 "Azithromycin", "Ciprofloxacin", "Citalopram", "Clindamycin",
                 "Clonazepam", "Codeine", "Cyclobenzaprine", "Cymbalta", "Doxycycline",
                 "Gabapentin", "Hydrochlorothiazide", "Ibuprofen", "Lexapro",
                 "Lisinopril", "Loratadine", "Lorazepam", "Losartan", "Lyrica",
                 "Meloxicam", "Metformin", "Metoprolol", "Naproxen", "Omeprazole",
                 "Oxycodone", "Pantoprazole", "Prednisone", "Tramadol", "Trazodone",
                 "Viagra", "Wellbutrin", "Xanax", "Zoloft"),
       First = c("Aceta", "Adde", "Alpra", "Amitri", "Amlo", "Amoxi", "Ati",
                 "Atorva", "Azithro", "Cipro", "Citalo", "Clinda", "Clona",
                 "Co", "Cyclo", "Cym", "Doxy", "Gaba", "Hydro", "Ibu", "Lexa",
                 "Lisino", "Lorata", "Loraze", "Losa", "Lyri", "Melo", "Metfo",
                 "Meto", "Napro", "Ome", "Oxy", "Panto", "Predni", "Trama", "Trazo",
                 "Via", "Wellbu", "Xa", "Zo"),
        Last = c("minophen", "rall", "zolam", "ptyline", "dipine", "cillin",
                 "van", "statin", "mycin", "floxacin", "pram", "mycin",
                 "zepam", "deine", "benzaprine", "balta", "cycline", "pentin",
                 "thiazide", "profen", "pro", "pril", "dine", "pam", "tan", "ca", "xicam",
                 "rmin", "prolol", "xen", "prazole", "codone", "prazole",
                 "sone", "dol", "done", "gra", "trin", "nax", "loft")
)


# create new drug names by recombining first and last parts, take only new and unique combinations

new_first <- rep(drugs$First,40)
new_last <- rep(drugs$Last, each=40)
new_drugs <- paste(new_first,new_last, sep="")
new_drugs <- new_drugs[new_drugs %in% drugs$Original == FALSE]
new_drugs <- unique(new_drugs)

# sample enough unique drug names to cover the experiment
sample_drugs <- sample(new_drugs, 22*3)


# stimulus generator
# create dataframe to code and assign stimuli

general_design <- data.frame(stringsAsFactors=FALSE,
                       premise_one = c("A+", "A+", "A-", "A-", "AB+", "AB-", "AB+", "AB-", "A+",
                                       "A-", "AB+", "AB-", "C+", "C-", "C+",
                                       "C+", "C-", "C-", "AB+", "AB-", "AB+",
                                       "AB-"),
                       premise_two = c("AB+", "AB-", "AB+", "AB-", "A+", "A+", "A-", "A-", NA,
                                       NA, NA, NA, NA, NA, "AB+", "AB-", "AB+",
                                       "AB-", "C+", "C+", "C-", "C-"),
                     Question_type = c("experimental", "experimental", "experimental",
                                       "experimental", "experimental",
                                       "experimental", "experimental", "experimental",
                                       "control", "control", "control", "control",
                                       "control", "control", "control",
                                       "control", "control", "control", "control",
                                       "control", "control", "control")
                  ) %>%
  mutate(drug_one = str_replace(premise_one,"[+-]",""),
         drug_two = str_replace(premise_two,"[+-]",""),
         drug_A = sample_drugs[1:22],
         drug_B = sample_drugs[23:44],
         drug_C = sample_drugs[45:66],
         p1_outcome = as.factor(str_detect(premise_one,"[+]")),
         p2_outcome = as.factor(str_detect(premise_two,"[+]"))) %>%
  mutate(p1_wording = factor(p1_outcome,labels=c("not cured","cured")),
         p2_wording = factor(p2_outcome,labels=c("not cured","cured")),
         p1_sentence = NA,
         p2_sentence = NA,
         p1_question = list(NA),
         p2_question = list(NA))

# write the premise and question for each trial 

for(i in 1:dim(general_design)[1]){

  # write drug one premise  
  if (general_design$drug_one[i] == "A") {
    premise <- paste("People who took",
                    general_design$drug_A[i],"were",
                    general_design$p1_wording[i])
    question <- paste("How well does", general_design$drug_A[i],"work?")
  }
  
  if (general_design$drug_one[i] == "B") {
    premise <- paste("People who took",
                    general_design$drug_B[i],"were",
                    general_design$p1_wording[i])
    question <- paste("How well does", general_design$drug_B[i],"work?")
  }
  
  if (general_design$drug_one[i] == "AB") {
    premise <- paste("People who took",
                     general_design$drug_A[i],"and",
                     general_design$drug_B[i],"were",
                     general_design$p1_wording[i])
    question <- c(paste("How well does", general_design$drug_A[i],"work?"),
                  paste("How well does", general_design$drug_B[i],"work?"),
                  paste("How well does the combination of", 
                      general_design$drug_A[i],"and",
                      general_design$drug_B[i],"work?")
    )
  }
  
  if (general_design$drug_one[i] == "C") {
    premise <- paste("People who took",
               general_design$drug_C[i],"were",
               general_design$p1_wording[i])
    question <- paste("How well does", general_design$drug_C[i],"work?")
  }
  
  general_design$p1_sentence[i] <- premise
  general_design$p1_question[[i]] <- question
  
  # write drug two premise
  if (is.na(general_design$drug_two[i])) {
    premise <- NA
    question <- NA}
  else{
    if (general_design$drug_two[i] == "A") {
      premise <- paste("People who took",
                       general_design$drug_A[i],"were",
                       general_design$p2_wording[i])
      question <- paste("How well does", general_design$drug_A[i],"work?")
    }
    
    if (general_design$drug_two[i] == "B") {
      premise <- paste("People who took",
                       general_design$drug_B[i],"were",
                       general_design$p2_wording[i])
      question <- paste("How well does", general_design$drug_B[i],"work?")
      
    }
    
    if (general_design$drug_two[i] == "AB") {
      premise <- paste("People who took",
                       general_design$drug_A[i],"and",
                       general_design$drug_B[i],"were",
                       general_design$p2_wording[i])
      question <- c(paste("How well does", general_design$drug_A[i],"work?"),
                    paste("How well does", general_design$drug_B[i],"work?"),
                    paste("How well does the combination of", 
                          general_design$drug_A[i],"and",
                          general_design$drug_B[i],"work?")
      )
    }
    
    if (general_design$drug_two[i] == "C") {
      premise <- paste("People who took",
                       general_design$drug_C[i],"were",
                       general_design$p2_wording[i])
      question <- paste("How well does", general_design$drug_B[i],"work?")
    }
  }
  
  general_design$p2_sentence[i] <- premise
  general_design$p2_question[[i]] <- question
}

# create unique list of questions for each set of premises
general_design <- general_design %>%
  mutate(all_questions = list(NA),
         num_questions = 0,
         q1 = NA,
         q2 = NA,
         q3 = NA,
         q4 = NA)

for(i in 1:dim(general_design)[1]){
  all_questions <- c(general_design$p1_question[[i]],general_design$p2_question[[i]])
  all_questions <- all_questions[!is.na(all_questions)]
  all_questions <- unique(all_questions)
  general_design$all_questions[[i]] <- all_questions
  general_design$num_questions[i] <- length(all_questions)
  general_design$q1[i] <- all_questions[1]
  general_design$q2[i] <- all_questions[2]
  general_design$q3[i] <- all_questions[3]
  general_design$q4[i] <- all_questions[4]
}

general_design <- general_design %>%
  mutate(stimulus = paste(premise_one,premise_two,sep=","))

library(xprmntr)
stimulus_json <- stimulus_df_to_json(general_design,
                       stimulus= "stimulus",
                       data = c("premise_one","premise_two",
                                "p1_outcome","p2_outcome",
                                "p1_sentence","p2_sentence",
                                "q1","q2","q3","q4"))




