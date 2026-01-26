################################################################################
################################################################################
rm(list = ls()); gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),"/jmichaelson-wdata/msmuhammad/workbench/customized-functions/correct_path.R"))
source(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/projects/brain-drug-map_V02/")
setwd(project.dir)
################################################################################
################################################################################
################################################################################
## load gex
brain.gex <- pdsload("../brain-gene-map/data/derivatives/predicted-5k-gex-2.0mm.rds.pxz")
colnames(brain.gex)[-c(1:3)] <- toupper(colnames(brain.gex)[-c(1:3)])

## drugs passing BBB with > 50% chance
pload("/Dedicated/jmichaelson-sdata/CMAP/LINCS/small_mol_metadata_w_BBB_FDA.Rdata.pxz")
bbb.drugs <- smol_meta %>% filter(FDA == T, BBB_perm>0.5) %>% mutate(pert_name = tolower(pert_name))
## load CMAP
lincs.df.0 <- read_rds("/Dedicated/jmichaelson-wdata/msmuhammad/data/LINCS/cmap.of.int_v2.rds")
rownames(lincs.df.0) <- tolower(rownames(lincs.df.0))
colnames(lincs.df.0) <- toupper(colnames(lincs.df.0))

## clean both datasets to keep intersecting genes and meds passing BBB
genes.of.int <- intersect(colnames(brain.gex)[-c(1:3)], colnames(lincs.df.0))
comp.of.int <- intersect(rownames(lincs.df.0), bbb.drugs$pert_name)

brain.gex.1 <- cbind(brain.gex[,1:3],brain.gex[,genes.of.int])
lincs.df.bbb <- lincs.df.0[comp.of.int,genes.of.int] %>%
  as.data.frame() %>% rownames_to_column("compound")

################################################################################
################################################################################
## per voxel, compute correlation
all(colnames(brain.gex.1)[-c(1:3)]==colnames(lincs.df.bbb)[-1])
gc()
drug.corr <- cor(t(brain.gex.1[,-c(1:3)]), t(lincs.df.bbb[,-1]), method = "spearman")
drug.corr <- cbind(brain.gex.1[,1:3],drug.corr)
drug.corr <- drug.corr %>% rename_at(.vars = vars(c(1:3)), .funs = function(x) paste0("mni_",x))
colnames(drug.corr)[-c(1:3)] <- comp.of.int
# pdssave(drug.corr, file="data/derivatives/voxel-wise-drug-spearman.rds")
mni.labs <- read_rds("/wdata/msmuhammad/refs/labeled-MNI/2mm/voxels-w-labels-R.rds") %>%
  select(-ends_with("_id"), -c(x,y,z))
pdssave(inner_join(mni.labs,drug.corr),file="data/derivatives/voxel-wise-drug-spearman-voxel-labeled.rds")

################################################################################
################################################################################
## save nifti maps
mni.labs2 <- read_rds("/wdata/msmuhammad/refs/labeled-MNI/2mm/voxels-w-labels-R.rds") %>% select(-ends_with("_id"))
source("/wdata/msmuhammad/workbench/customized-functions/make-array-to-nifti.R")
registerDoMC(30)
foreach(d = 1:length(comp.of.int)) %dopar% {
  drug <- comp.of.int[d]
  df <- inner_join(mni.labs2[,1:6], drug.corr %>% select(1:3,drug) %>% rename(intensity = 4)) %>% 
    select(-starts_with("mni")) %>% mutate(intensity_mm = ((intensity-min(intensity))/(max(intensity)-min(intensity)))* 2 - 1)
  summary(df)
  make_nifti_from_df_V2(df, value_col = "intensity_mm",
                        out_path = paste0("data/derivatives/drug-nifti-maps/",drug,".nii"))
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
