#' Get a combined MAF from a set of reports
#' 
#' @param reports data frame with reports
#' @return A combined MAF data table
#' @examples
#' # getCombinedMaf(reports)
getCombinedMaf <- function(reports){
  #mafHeader <- c("Hugo_Symbol", "Entrez_Gene_Id", "Center", "NCBI_Build", "Chromosome", "Start_Position", "End_Position", "Strand", "Variant_Classification", "Variant_Type", "Reference_Allele", "Tumor_Seq_Allele1", "Tumor_Seq_Allele2", "dbSNP_RS", "dbSNP_Val_Status", "Tumor_Sample_Barcode", "Matched_Norm_Sample_Barcode", "Match_Norm_Seq_Allele1", "Match_Norm_Seq_Allele2", "Tumor_Validation_Allele1", "Tumor_Validation_Allele2", "Match_Norm_Validation_Allele1", "Match_Norm_Validation_Allele2", "Verification_Status", "Validation_Status", "Mutation_Status", "Sequencing_Phase", "Sequence_Source", "Validation_Method", "Score", "BAM_File", "Sequencer", "Tumor_Sample_UUID", "Matched_Norm_Sample_UUID", "HGVSc", "HGVSp", "Transcript_ID", "Exon_Number", "t_depth", "t_ref_count", "t_alt_count", "n_depth", "n_ref_count", "n_alt_count", "Effect", "Effect_Impact", "Functional_Class", "Codon_Change", "Amino_Acid_Change", "Amino_Acid_Length", "Gene_Name", "Transcript_BioType", "Gene_Coding", "Transcript_ID.1", "Exon_Rank", "Genotype_Number", "ERRORS", "WARNINGS", "ReportID")
  #mafHeader <- c("Hugo_Symbol","Entrez_Gene_Id","Center","NCBI_Build","Chromosome","Start_Position","End_Position","Strand","Variant_Classification","Variant_Type","Reference_Allele","Tumor_Seq_Allele1","Tumor_Seq_Allele2","dbSNP_RS","dbSNP_Val_Status","Tumor_Sample_Barcode","Matched_Norm_Sample_Barcode","Match_Norm_Seq_Allele1","Match_Norm_Seq_Allele2","Tumor_Validation_Allele1","Tumor_Validation_Allele2","Match_Norm_Validation_Allele1","Match_Norm_Validation_Allele2","Verification_Status","Validation_Status","Mutation_Status","Sequencing_Phase","Sequence_Source","Validation_Method","Score","BAM_File","Sequencer","Tumor_Sample_UUID","Matched_Norm_Sample_UUID","HGVSc","HGVSp","Transcript_ID","Exon_Number","t_depth","t_ref_count","t_alt_count","n_depth","n_ref_count","n_alt_count","all_effects","Effect","Effect_Impact","Functional_Class","Codon_Change","Amino_Acid_Change","Amino_Acid_Length","Gene_Name","Transcript_BioType","Gene_Coding","Transcript_ID","Exon_Rank","Genotype_Number","ERRORS","WARNINGS","DataReportID")
  mafHeader <- c("Hugo_Symbol","Entrez_Gene_Id","Center","NCBI_Build","Chromosome","Start_Position","End_Position","Strand","Variant_Classification","Variant_Type","Reference_Allele","Tumor_Seq_Allele1","Tumor_Seq_Allele2","dbSNP_RS","dbSNP_Val_Status","Tumor_Sample_Barcode","Matched_Norm_Sample_Barcode","Match_Norm_Seq_Allele1","Match_Norm_Seq_Allele2","Tumor_Validation_Allele1","Tumor_Validation_Allele2","Match_Norm_Validation_Allele1","Match_Norm_Validation_Allele2","Verification_Status","Validation_Status","Mutation_Status","Sequencing_Phase","Sequence_Source","Validation_Method","Score","BAM_File","Sequencer","Tumor_Sample_UUID","Matched_Norm_Sample_UUID","HGVSc","HGVSp","Transcript_ID","Exon_Number","t_depth","t_ref_count","t_alt_count","n_depth","n_ref_count","n_alt_count","all_effects","Allele","Gene","Feature","Feature_type","Consequence","cDNA_position","CDS_position","Protein_position","Amino_acids","Codons","Existing_variation","AA_MAF","EA_MAF","ALLELE_NUM","RefSeq","EXON","INTRON","MOTIF_NAME","MOTIF_POS","HIGH_INF_POS","MOTIF_SCORE_CHANGE","DISTANCE","STRAND","CLIN_SIG","CANONICAL","SYMBOL","SYMBOL_SOURCE","SIFT","PolyPhen","GMAF","BIOTYPE","ENSP","DOMAINS","CCDS","HGVSc","HGVSp","AFR_MAF","AMR_MAF","ASN_MAF","EUR_MAF","PUBMED","REPORTID")
  maf <- makeEmptyDataTable(mafHeader)
  
  for(k in 1:nrow(reports)){ #k <- 3
    mafFile   <- paste(reports$prefix[k],reports$PANEL_SOMATIC_MAF[k],sep="")
    
    if(!file.exists(mafFile)){
      next
    }
    tb <- fread( mafFile )
    tb$REPORTID <- reports$REPORTID[k]
    maf <- rbind(maf,tb, fill=TRUE)
    
    dot(k, every = 10)
  }
  maf
}
