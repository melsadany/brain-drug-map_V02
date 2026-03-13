################################################################################
################################################################################
rm(list = ls()); gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),"/jmichaelson-wdata/msmuhammad/workbench/customized-functions/correct_path.R"))
source(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
showtext::showtext_auto()
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/projects/brain-drug-map_V02/")
setwd(project.dir)
################################################################################
################################################################################
################################################################################
################################################################################
ns.data <- pdsload(correct_path("/wdata/msmuhammad/data/neurosynth/use_this/features-data-by-xyz-in-MNI-only.rds.pxz"))
# ns.topics <- read_tsv("../../data/neurosynth/neurosynth-data/data-neurosynth_version-7_vocab-LDA400_vocabulary.txt")
# ns.topics <- read_tsv("../../data/neurosynth/neurosynth-data/data-neurosynth_version-7_vocab-LDA400_keys.tsv")
# ns.topics.js <- jsonlite::read_json("../../data/neurosynth/neurosynth-data/data-neurosynth_version-7_vocab-LDA400_metadata.json")
drug.maps <- pdsload("data/derivatives/voxel-wise-drug-spearman-voxel-labeled.rds.pxz")



all <- inner_join(drug.maps[,-c(4:6)] %>% rename(x=mni_x,y=mni_y,z=mni_z), ns.data)

################################################################################
################################################################################
################################################################################
registerDoMC(11)
all.corr <- foreach(i = 4:750, .combine = rbind) %dopar% {
  comp <- colnames(all)[i]
  corr.2 <- foreach(j = 751:ncol(all), .combine = rbind) %dopar% {
    term <- colnames(all)[j]
    c <- cor.test(x=all[,i],y=all[,j],method = "spearman")
    data.frame(drug = comp, term = term, rho = c$estimate[[1]], pval = c$p.value[[1]])
  }
  return(corr.2)
}
# write_rds(all.corr,"data/derivatives/drug-to-neurosynth-terms.rds", compress="gz")
write_rds(all.corr,"data/derivatives/drug-to-neurosynth-terms-intersect-vox.rds", compress="gz")
# all.corr <- read_rds("data/derivatives/drug-to-neurosynth-terms.rds") %>% 
#   group_by(drug) %>% mutate(FDR = p.adjust(pval,"fdr")) %>% ungroup()
all.corr <- read_rds("data/derivatives/drug-to-neurosynth-terms-intersect-vox.rds") %>% 
  group_by(drug) %>% mutate(FDR = p.adjust(pval,"fdr")) %>% ungroup()
################################################################################
################################################################################

terms.to.keep <- c(
  "behavior", "behavioural", "behavioral.performance", "behavioral.responses",
  "cognition", "cognitive", "cognitive.control", "cognitive.emotional",
  "cognitive.function", "cognitive.functions", "cognitive.impairment",
  "cognitive.neuroscience", "cognitive.performance", "cognitive.tasks",
  "executive", "executive.control", "executive.function", "executive.functions",
  "working.memory", "memory", "memory.task", "memory.tasks",
  "memory.wm", "episodic.memory", "autobiographical.memory", "semantic.memory",
  "recognition.memory", "term.memory", "long.term", "short.term",
  "attention", "attentional", "attentional.control", "selective.attention",
  "sustained.attention", "spatial.attention", "visual.attention",
  "response.inhibition", "inhibitory.control", "inhibition",
  "decision.making", "decision", "choices", "reward", "reward.anticipation",
  "risk", "risk.taking", "goal", "goal.directed",
  "emotion", "emotional", "emotion.regulation", "emotional.valence",
  "emotional.responses", "negative.emotions", "affective", "negative.affect",
  "mood", "motivation", "anxiety", "anxiety.disorders",
  "depression", "major.depression", "major.depressive", "depressive.disorder",
  "ptsd", "posttraumatic", "stress.disorder", "ocd", "compulsive.disorder",
  "obsessive.compulsive", "hyperactivity.disorder", "attention.deficit",
  "adhd", "addiction", "smoking", "drug", "drugs",
  "bipolar", "bipolar.disorder", "schizophrenia", "psychosis",
  "psychiatric", "psychiatric.disorders", "neuropsychiatric",
  "dementia", "alzheimer", "alzheimer.disease", "parkinson", "parkinson.disease",
  "autism", "autism.spectrum", "asd",
  "illness", "disease", "diseases", "disorders", "disorder",
  "symptom", "symptoms", "symptom.severity", "impairments", "cognitive.deficits",
  "deficit", "deficit.hyperactivity",
  "pain", "chronic.pain", "noxious", "painful",
  "social", "social.cognition", "social.interaction", "social.interactions",
  "theory.mind", "mind", "mental", "mental.state", "mental.states",
  "mentalizing", "empathy", "self", "self.referential", "self.report",
  "self.reported", "personality", "personality.traits",
  "learning", "learning.task", "conditioning", "reinforcement.learning",
  "perception", "visual.perception", "speech.perception",
  "imagery", "mental.imagery", "motor.imagery",
  "language", "language.comprehension", "language.network",
  "reading", "sentence", "sentence.comprehension",
  "speech", "speech.production", "verbal", "verbal.fluency",
  "lexical", "lexical.decision", "naming", "comprehension",
  "faces", "face", "face.recognition", "facial", "facial.expression",
  "facial.expressions", "happy.faces", "emotional.faces", "neutral.faces",
  "fear", "fearful", "fearful.faces", "disgust",
  "music", "musical", "empathy", "rewarding",
  "stress", "anxiety", "fear", "anger",
  "motivation", "reward", "punishment", "addiction", "craving",
  "autonomic", "arousal", "valence",
  "attention.network", "default.mode", "default.network", "dmn",
  "frontoparietal.network", "salience.network", "functional.network",
  "functional.networks",
  
  "impulsive", "impulsivity", "sleep"
)

brain.relevant.drugs <- c(
  # Antipsychotics
  "fluphenazine", "trifluoperazine", "thioridazine", "olanzapine",
  "aripiprazole", "quetiapine", "haloperidol", "ziprasidone",
  "risperidone", "lurasidone", "pimozide", "perphenazine",
  "loxapine", "molindone", "amisulpride", "mesoridazine",
  "iloperidone", "pimavanserin", "brexpiprazole", "cariprazine",
  
  # Antidepressants (SSRIs, SNRIs, TCAs, MAOIs, atypical)
  "imipramine", "amitriptyline", "nortriptyline", "clomipramine",
  "maprotiline", "desipramine", "trimipramine", "protriptyline",
  "fluoxetine", "sertraline", "citalopram", "escitalopram",
  "paroxetine", "fluvoxamine", "venlafaxine", "desvenlafaxine",
  "duloxetine", "mirtazapine", "nefazodone", "trazodone",
  "bupropion", "vilazodone", "vortioxetine", "milnacipran",
  "phenelzine", "isocarboxazid", "tranylcypromine",
  
  # Mood stabilizers / anti-epileptics used psychiatrically
  "carbamazepine", "lamotrigine", "topiramate", "valproate",  
  "oxcarbazepine", "zonisamide", "felbamate", "primidone",
  "gabapentin", "tiagabine", "vigabatrin", "ethosuximide",
  "levetiracetam", "stiripentol", "rufinamide", "perampanel",
  
  # Anxiolytics, sedatives, hypnotics
  "lorazepam", "diazepam", "alprazolam", "clonazepam",
  "chlordiazepoxide", "midazolam", "remimazolam",
  "zolpidem", "zaleplon", "eszopiclone", "temazepam",
  "buspirone",
  
  # Stimulants / ADHD / wakefulness
  "methylphenidate", "modafinil", "armodafinil",
  "pemoline", "atomoxetine", "guanfacine", "bupropion",
  
  # Cognition / dementia / neurodegenerative
  "donepezil", "rivastigmine", "galantamine", "memantine",
  "tacrine", "selegiline", "safinamide", "rasagiline",
  "riluzole", "edaravone",
  
  # Parkinson / movement / dopamine-related
  "pramipexole", "ropinirole", "pergolide", "cabergoline",
  "amantadine", "levodopa", "apomorphine", "tetrabenazine",
  
  # Substance use / dependence / SUD-related
  "naltrexone", "naloxone", "acamprosate", "disulfiram",
  "nicotine", "varenicline",
  
  # Pain / analgesia / sedation (CNS active)
  "tramadol", "pentobarbital", "butalbital", "propofol",
  "ketamine", "sufentanil", "vecuronium", "pancuronium",
  "rocuronium", "atracurium",  # mostly anesthetic / motor, still CNS-relevant context
  
  # Other CNS-modulating agents
  "scopolamine", "atropine", "trihexyphenidyl", "oxybutynin",
  "bethanechol", "mestinon", "neostigmine", "rivastigmine",
  "tizanidine", "baclofen", "cyclobenzaprine", "carisoprodol",
  "methocarbamol"
)

################################################################################
################################################################################
################################################################################
all.corr %>% filter(drug %in% c("methylphenidate","venlafaxine","sertraline"),term %in% terms.to.keep) %>%
  ggplot(aes(rho,color=drug))+geom_density()+scale_color_manual(values = palette.1)+
  geom_vline(xintercept = 0,linetype=2,color="pink")+
  bw.theme+labs(x=rho)
all.corr %>% filter(term %in% terms.to.keep)%>%
  ggplot(aes(rho,group=drug))+geom_density(alpha=0.001)+
  bw.theme+labs(x=rho)

all.corr %>% filter(FDR < 0.05,term %in% terms.to.keep) %>% view
all.corr %>% filter(FDR < 0.05,term %in% terms.to.keep, drug %in% brain.relevant.drugs) %>% 
  mutate(rho = signif(rho,4), FDR = signif(FDR,4), val = paste0(rho," (",FDR,")")) %>%
  select(drug, term, rho,FDR) %>% 
  write_tsv("data/derivatives/select-drug-to-select-neurosynth-terms.tsv")

all.corr %>% filter(FDR < 0.05,term %in% terms.to.keep, drug %in% brain.relevant.drugs) %>% 
  mutate(rho = signif(rho,4), FDR = signif(FDR,4), val = paste0(rho," (",FDR,")")) %>%
  pivot_wider(names_from = drug, values_from = val, id_cols = term) %>%
  arrange(term) %>% mutate_all(.funs=function(x)replace_na(x," ")) %>%
  write_tsv("data/derivatives/select-drug-to-select-neurosynth-terms--wide.tsv")

all.corr.filt <- all.corr %>% filter(FDR < 0.05,term %in% terms.to.keep, drug %in% brain.relevant.drugs) %>% 
  mutate(rho = signif(rho,4), FDR = signif(FDR,4), val = paste0(rho," (",FDR,")")) %>%
  select(drug, term, rho,FDR)

all.corr.filt %>% filter(drug=="methylphenidate") %>% arrange(rho) %>%
  write_tsv("data/derivatives/methylphenidate-to-select-neurosynth-terms.tsv")
all.corr.filt %>% filter(drug=="bupropion") %>% arrange(rho) %>%
  write_tsv("data/derivatives/bupropion-to-select-neurosynth-terms.tsv")
all.corr.filt %>% filter(drug=="sertraline") %>% arrange(rho) %>%
  write_tsv("data/derivatives/sertraline-to-select-neurosynth-terms.tsv")
all.corr.filt %>% filter(drug=="haloperidol") %>% arrange(rho) %>%
  write_tsv("data/derivatives/haloperidol-to-select-neurosynth-terms.tsv")
all.corr.filt %>% filter(drug=="lamotrigine") %>% arrange(rho) %>%
  write_tsv("data/derivatives/lamotrigine-to-select-neurosynth-terms.tsv")
all.corr.filt %>% filter(drug=="diazepam") %>% arrange(rho) %>%
  write_tsv("data/derivatives/diazepam-to-select-neurosynth-terms.tsv")



all.corr %>% filter(pval<0.2,term %in% terms.to.keep,
                    drug %in% c("methylphenidate", "haloperidol", "sertraline", "lamotrigine", "diazepam","bupropion")) %>%
  ggplot(aes(drug,term,fill=rho,label=ifelse(FDR<0.05,"*","")))+geom_tile()+my.guides+redblu.col.gradient.2()+
  geom_text()+bw.theme+labs(y="term",x="")+theme(axis.text.x.bottom = element_text(angle = 90,hjust=1,vjust=0.5))
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
