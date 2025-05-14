rm(list = ls())
setwd('E:/LZ')
if(! dir.exists("25014")) dir.create("25014")
setwd('./25014')

library(foreign)
library(dplyr)
library(survey)
library(reshape2)
library(do)
library(openxlsx)
library(nhanesA)


# 设置保留小数点后3位
options(digits=3)

# 1. 人口统计学数据-------------
# 读取数据，包括所有子文件夹
setwd("./00_rawdata/")
files <- list.files(pattern=".*_DEMO_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(DEMO_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}

#人口学数据处理，加年份
#DEMO$Year <- "1999-2000"
#DEMO_B$Year <- "2001-2002"
#DEMO_C$Year <- "2003-2004"
DEMO_D$Year <- "2005-2006"
DEMO_E$Year <- "2007-2008"
DEMO_F$Year <- "2009-2010"
DEMO_G$Year <- "2011-2012"
DEMO_H$Year <- "2013-2014"
DEMO_I$Year <- "2015-2016"
DEMO_J$Year <- "2017-2018"

#权重数据提取
wt_dat <- rbind(DEMO_D[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year")],
                DEMO_E[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year")],
                DEMO_F[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year")],
                DEMO_G[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year")],
                DEMO_H[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year")],
                DEMO_I[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year")],
                DEMO_J[,c("SEQN","SDMVSTRA","WTMEC2YR","SDMVPSU","Year")]
                )

#人口学特征提取， 变量名一样，直接合并，性别，年龄，种族，教育程度，婚姻，家庭经济贫困
DEMO <- rbind(DEMO_D[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC3","DMDEDUC2","DMDMARTL","INDFMPIR")],
              DEMO_E[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC3","DMDEDUC2","DMDMARTL","INDFMPIR")],
              DEMO_F[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC3","DMDEDUC2","DMDMARTL","INDFMPIR")],
              DEMO_G[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC3","DMDEDUC2","DMDMARTL","INDFMPIR")],
              DEMO_H[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC3","DMDEDUC2","DMDMARTL","INDFMPIR")],
              DEMO_I[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC3","DMDEDUC2","DMDMARTL","INDFMPIR")],
              DEMO_J[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC3","DMDEDUC2","DMDMARTL","INDFMPIR")]
              ) 

#对变量进行重命名
DEMO_dat1 <- rename(.data=DEMO,
                    Gender=RIAGENDR,
                    Age=RIDAGEYR,
                    Race=RIDRETH1,
                    PIR=INDFMPIR,
                    EDUcation2=DMDEDUC2, #Adults 20+
                    EDUcation3=DMDEDUC3, #Youth 6-19
                    Marital=DMDMARTL)

DEMO_dat<-full_join(DEMO_dat1,wt_dat,by="SEQN")


# 2. 自变量（暴露数据）-------------
files <- list.files(pattern=".*_(DR1TOT|DR2TOT)_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?((DR1TOT|DR2TOT)_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}


DR1 <- rbind(DR1TOT_D[,c("SEQN","DR1TVARA","DR1TVC","DR1TATOC","DR1TZINC","DR1TSELE","DR1TBCAR","WTDRD1")],
              DR1TOT_E[,c("SEQN","DR1TVARA","DR1TVC","DR1TATOC","DR1TZINC","DR1TSELE","DR1TBCAR","WTDRD1")],
              DR1TOT_F[,c("SEQN","DR1TVARA","DR1TVC","DR1TATOC","DR1TZINC","DR1TSELE","DR1TBCAR","WTDRD1")],
              DR1TOT_G[,c("SEQN","DR1TVARA","DR1TVC","DR1TATOC","DR1TZINC","DR1TSELE","DR1TBCAR","WTDRD1")],
              DR1TOT_H[,c("SEQN","DR1TVARA","DR1TVC","DR1TATOC","DR1TZINC","DR1TSELE","DR1TBCAR","WTDRD1")],
              DR1TOT_I[,c("SEQN","DR1TVARA","DR1TVC","DR1TATOC","DR1TZINC","DR1TSELE","DR1TBCAR","WTDRD1")],
              DR1TOT_J[,c("SEQN","DR1TVARA","DR1TVC","DR1TATOC","DR1TZINC","DR1TSELE","DR1TBCAR","WTDRD1")]
) 

DR1_dat <- rename(.data=DR1,
                   VA1=DR1TVARA, #中性粒细胞
                   VC1=DR1TVC, #淋巴细胞
                   VE1=DR1TATOC, #单核细胞
                  zinc1=DR1TZINC, #血小板计数 SI（1000 个细胞/微升）
                  selenium1=DR1TSELE,
                  carotenoids1=DR1TBCAR,
                  wt_DR1 = WTDRD1
) 

#######    D2
DR2 <- rbind(DR2TOT_D[,c("SEQN","DR2TVARA","DR2TVC","DR2TATOC","DR2TZINC","DR2TSELE","DR2TBCAR","WTDR2D")],
             DR2TOT_E[,c("SEQN","DR2TVARA","DR2TVC","DR2TATOC","DR2TZINC","DR2TSELE","DR2TBCAR","WTDR2D")],
             DR2TOT_F[,c("SEQN","DR2TVARA","DR2TVC","DR2TATOC","DR2TZINC","DR2TSELE","DR2TBCAR","WTDR2D")],
             DR2TOT_G[,c("SEQN","DR2TVARA","DR2TVC","DR2TATOC","DR2TZINC","DR2TSELE","DR2TBCAR","WTDR2D")],
             DR2TOT_H[,c("SEQN","DR2TVARA","DR2TVC","DR2TATOC","DR2TZINC","DR2TSELE","DR2TBCAR","WTDR2D")],
             DR2TOT_I[,c("SEQN","DR2TVARA","DR2TVC","DR2TATOC","DR2TZINC","DR2TSELE","DR2TBCAR","WTDR2D")],
             DR2TOT_J[,c("SEQN","DR2TVARA","DR2TVC","DR2TATOC","DR2TZINC","DR2TSELE","DR2TBCAR","WTDR2D")]
) 

DR2_dat <- rename(.data=DR2,
                  VA2=DR2TVARA, 
                  VC2=DR2TVC, 
                  VE2=DR2TATOC, 
                  zinc2=DR2TZINC, 
                  selenium2=DR2TSELE,
                  carotenoids2=DR2TBCAR,
                  wt_DR2=WTDR2D
) 


# 3. 其他协变量--------------------

## -------------淋巴细胞 中性粒细胞 血小板 单核细胞 红细胞分布宽度-------------
files <- list.files(pattern=".*_CBC_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")

for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(CBC_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}

exam <- rbind(CBC_D[,c("SEQN","LBDNENO","LBDLYMNO","LBDMONO","LBXPLTSI","LBXRDW")],
              CBC_E[,c("SEQN","LBDNENO","LBDLYMNO","LBDMONO","LBXPLTSI","LBXRDW")],
              CBC_F[,c("SEQN","LBDNENO","LBDLYMNO","LBDMONO","LBXPLTSI","LBXRDW")],
              CBC_G[,c("SEQN","LBDNENO","LBDLYMNO","LBDMONO","LBXPLTSI","LBXRDW")],
              CBC_H[,c("SEQN","LBDNENO","LBDLYMNO","LBDMONO","LBXPLTSI","LBXRDW")],
              CBC_I[,c("SEQN","LBDNENO","LBDLYMNO","LBDMONO","LBXPLTSI","LBXRDW")],
              CBC_J[,c("SEQN","LBDNENO","LBDLYMNO","LBDMONO","LBXPLTSI","LBXRDW")]
) 

exam_dat <- rename(.data=exam,
                   Neutrophils=LBDNENO, #中性粒细胞
                   Lymphocyte=LBDLYMNO, #淋巴细胞
                   Monocytes=LBDMONO, #单核细胞
                   Platelets=LBXPLTSI, #血小板计数 SI（1000 个细胞/微升）
                   RDW=LBXRDW
) 


## -------------白蛋白-------------
files <- list.files(pattern=".*_BIOPRO_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(BIOPRO_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}

wp <- rbind(BIOPRO_D[,c("SEQN","LBDSALSI")],
            BIOPRO_E[,c("SEQN","LBDSALSI")],
            BIOPRO_F[,c("SEQN","LBDSALSI")],
            BIOPRO_G[,c("SEQN","LBDSALSI")],
            BIOPRO_H[,c("SEQN","LBDSALSI")],
            BIOPRO_I[,c("SEQN","LBDSALSI")],
            BIOPRO_J[,c("SEQN","LBDSALSI")]
) 

wp_dat <- rename(.data=wp,
                 albumin =LBDSALSI #白蛋白
) 

## -------------C反应蛋白-------------
if(F){
  # 使用grep的ignore.case参数来忽略大小写
  files <- list.files(pattern=".*_(CRP|HSCRP)_.*", recursive=TRUE, ignore.case=TRUE)
  cat("找到", length(files), "个文件\n")
  for(i in seq_along(files)){
    file <- files[i]
    cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
    # 提取DEMO_X形式的变量名
    varname <- gsub(".*?((CRP|HSCRP)_[A-Z]).*", "\\1", file)
    assign(varname, read.xport(file.path(file)))
  }
  #因变量名不一致，因此分两次合并
  CRP1 <-  rbind(CRP_D[,c("SEQN","LBXCRP")], #单位是mg/dL
                 CRP_E[,c("SEQN","LBXCRP")],
                 CRP_F[,c("SEQN","LBXCRP")])          
  CRP1$CRP_mg_l <- CRP1$LBXCRP*10       #转化成mg/L
  CRP1 <- CRP1[,c("SEQN","CRP_mg_l")]
  
  CRP2 <- rbind(HSCRP_I[,c("SEQN","LBXHSCRP","LBDHRPLC")], #mg/L
                HSCRP_J[,c("SEQN","LBXHSCRP","LBDHRPLC")]) #最低检测值LBDHRPLC,要转化
  CRP2$CRP_mg_l=ifelse(CRP2$LBDHRPLC==1,CRP2$LBXHSCRP/sqrt(2),CRP2$LBXHSCRP) #最低检测值转化
  CRP2 <- CRP2[,c("SEQN","CRP_mg_l")]
  CRP_dat <- rbind(CRP1,CRP2)
}





## --------------饮酒-------------------
files <- list.files(pattern=".*_ALQ_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(ALQ_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}


ALQ_J <- rename(.data = ALQ_J, ALQ101 = ALQ290)


ALQ <- rbind(ALQ_D[,c("SEQN","ALQ101")],
             ALQ_E[,c("SEQN","ALQ101")],
             ALQ_F[,c("SEQN","ALQ101")],
             ALQ_G[,c("SEQN","ALQ101")],
             ALQ_H[,c("SEQN","ALQ101")],
             ALQ_I[,c("SEQN","ALQ101")],
             ALQ_J[,c("SEQN","ALQ101")]
) 

ALQ_dat <- rename(.data=ALQ,
                  Drink =ALQ101 
) 


## --------------吸烟-------------------
files <- list.files(pattern=".*_SMQ_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(SMQ_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}

SMQ <- rbind(SMQ_D[,c("SEQN","SMQ020","SMQ040")],
             SMQ_E[,c("SEQN","SMQ020","SMQ040")],
             SMQ_F[,c("SEQN","SMQ020","SMQ040")],
             SMQ_G[,c("SEQN","SMQ020","SMQ040")],
             SMQ_H[,c("SEQN","SMQ020","SMQ040")],
             SMQ_I[,c("SEQN","SMQ020","SMQ040")],
             SMQ_J[,c("SEQN","SMQ020","SMQ040")]
) 

SMQ_dat <- rename(.data=SMQ,Smoke_history=SMQ020,smoke_now=SMQ040)



## --------------BMI-------------------
files <- list.files(pattern=".*BMX_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(BMX_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}

BMI <- rbind(BMX_D[,c("SEQN","BMXBMI","BMXWAIST")],
             BMX_E[,c("SEQN","BMXBMI","BMXWAIST")],
             BMX_F[,c("SEQN","BMXBMI","BMXWAIST")],
             BMX_G[,c("SEQN","BMXBMI","BMXWAIST")],
             BMX_H[,c("SEQN","BMXBMI","BMXWAIST")],
             BMX_I[,c("SEQN","BMXBMI","BMXWAIST")],
             BMX_J[,c("SEQN","BMXBMI","BMXWAIST")]
) 

BMI_dat <- rename(.data=BMI, BMI=BMXBMI, WC=BMXWAIST)


## --------------身体活动水平-------------------
files <- list.files(pattern=".*_PAQ_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(PAQ_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}
# 统一列名
PAQ_D <- PAQ_D[,c("SEQN","PAD200","PAD320")]
PAQ_D <- rename(.data = PAQ_D, PAQ665 = PAD200, PAQ650 = PAD320)

PAQ <- rbind(PAQ_D[,c("SEQN","PAQ650","PAQ665")],
             PAQ_E[,c("SEQN","PAQ650","PAQ665")],
             PAQ_F[,c("SEQN","PAQ650","PAQ665")],
             PAQ_G[,c("SEQN","PAQ650","PAQ665")],
             PAQ_H[,c("SEQN","PAQ650","PAQ665")],
             PAQ_I[,c("SEQN","PAQ650","PAQ665")],
             PAQ_J[,c("SEQN","PAQ650","PAQ665")]
) 

PAQ_dat <- rename(.data=PAQ,med=PAQ665,high=PAQ650)   



## --------------高血压-------------------
files <- list.files(pattern=".*_(BPQ|BPX)_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?((BPQ|BPX)_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}

BPQ <- rbind(BPQ_D[,c("SEQN","BPQ020","BPQ080")],
             BPQ_E[,c("SEQN","BPQ020","BPQ080")],
             BPQ_F[,c("SEQN","BPQ020","BPQ080")],
             BPQ_G[,c("SEQN","BPQ020","BPQ080")],
             BPQ_H[,c("SEQN","BPQ020","BPQ080")],
             BPQ_I[,c("SEQN","BPQ020","BPQ080")],
             BPQ_J[,c("SEQN","BPQ020","BPQ080")]
) 

BPQ_dat <- rename(.data=BPQ, hypter=BPQ020,cholesterol=BPQ080)



####_________血压测量水平_________####
BPX <- rbind(BPX_D[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")],
             BPX_E[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")],
             BPX_F[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")],
             BPX_G[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")],
             BPX_H[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")],
             BPX_I[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")],
             BPX_J[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")]
) 

BPX_dat <- BPX


## --------------血脂-------------------
files <- list.files(pattern=".*_(TCHOL|BIOPRO|HDL|TRIGLY)_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?((TCHOL|BIOPRO|HDL|TRIGLY)_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}

####_________TC_________####
TCHOL <- rbind(TCHOL_D[,c("SEQN","LBDTCSI","LBXTC")],
               TCHOL_E[,c("SEQN","LBDTCSI","LBXTC")],
               TCHOL_F[,c("SEQN","LBDTCSI","LBXTC")],
               TCHOL_G[,c("SEQN","LBDTCSI","LBXTC")],
               TCHOL_H[,c("SEQN","LBDTCSI","LBXTC")],
               TCHOL_I[,c("SEQN","LBDTCSI","LBXTC")],
               TCHOL_J[,c("SEQN","LBDTCSI","LBXTC")]
) 

TCHOL_dat <- rename(.data=TCHOL, TC_mmol=LBDTCSI, TC_mg=LBXTC)


####_________TG_________####
TG <- rbind(BIOPRO_D[,c("SEQN","LBDSTRSI","LBXSTR")],
               BIOPRO_E[,c("SEQN","LBDSTRSI","LBXSTR")],
               BIOPRO_F[,c("SEQN","LBDSTRSI","LBXSTR")],
               BIOPRO_G[,c("SEQN","LBDSTRSI","LBXSTR")],
               BIOPRO_H[,c("SEQN","LBDSTRSI","LBXSTR")],
               BIOPRO_I[,c("SEQN","LBDSTRSI","LBXSTR")],
               BIOPRO_J[,c("SEQN","LBDSTRSI","LBXSTR")]
) 

TG_dat <- rename(.data=TG,TG_mmol=LBDSTRSI,TG_mg=LBXSTR)

####_________HDL_________####
HDL <- rbind(HDL_D[,c("SEQN","LBDHDDSI","LBDHDD")],
             HDL_E[,c("SEQN","LBDHDDSI","LBDHDD")],
             HDL_F[,c("SEQN","LBDHDDSI","LBDHDD")],
             HDL_G[,c("SEQN","LBDHDDSI","LBDHDD")],
             HDL_H[,c("SEQN","LBDHDDSI","LBDHDD")],
             HDL_I[,c("SEQN","LBDHDDSI","LBDHDD")],
             HDL_J[,c("SEQN","LBDHDDSI","LBDHDD")]
) 

HDL_dat <- rename(.data=HDL,HDL_mmol=LBDHDDSI,HDL_mg=LBDHDD)

####__________LDL__________####
LDL <- rbind(TRIGLY_D[,c("SEQN","WTSAF2YR","LBDLDLSI","LBDLDL","LBXTR","LBDTRSI")],
             TRIGLY_E[,c("SEQN","WTSAF2YR","LBDLDLSI","LBDLDL","LBXTR","LBDTRSI")],
             TRIGLY_F[,c("SEQN","WTSAF2YR","LBDLDLSI","LBDLDL","LBXTR","LBDTRSI")],
             TRIGLY_G[,c("SEQN","WTSAF2YR","LBDLDLSI","LBDLDL","LBXTR","LBDTRSI")],
             TRIGLY_H[,c("SEQN","WTSAF2YR","LBDLDLSI","LBDLDL","LBXTR","LBDTRSI")],
             TRIGLY_I[,c("SEQN","WTSAF2YR","LBDLDLSI","LBDLDL","LBXTR","LBDTRSI")],
             TRIGLY_J[,c("SEQN","WTSAF2YR","LBDLDLSI","LBDLDL","LBXTR","LBDTRSI")]
) 

LDL_dat <- rename(.data=LDL,LDL_mmol=LBDLDLSI,LDL_mg=LBDLDL,
                  TG2_mmol=LBDTRSI,TG2_mg=LBXTR,
                  wt_LDL=WTSAF2YR)



## --------------糖尿病 -------------------

files <- list.files(pattern=".*_(DIQ|GHB|GLU)_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?((DIQ|GHB|GLU)_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}

####__________自述+药物__________####
c("DIQ010","DIQ050","DID070") %in% names(DIQ_D) # 2005-2006 DID070
c("DIQ010","DIQ050","DID070") %in% names(DIQ_E) # 2007-2008 DID070
c("DIQ010","DIQ050","DIQ070") %in% names(DIQ_F) # 只有09-10的有DIQ070变量
c("DIQ010","DIQ050","DID070") %in% names(DIQ_G) # 
c("DIQ010","DIQ050","DID070") %in% names(DIQ_H) # 
c("DIQ010","DIQ050","DIQ070") %in% names(DIQ_I) # 
c("DIQ010","DIQ050","DIQ070") %in% names(DIQ_J) # 


DIQ_F <- rename(.data = DIQ_F, DID070 = DIQ070)
DIQ_G <- rename(.data = DIQ_G, DID070 = DIQ070)
DIQ_H <- rename(.data = DIQ_H, DID070 = DIQ070)
DIQ_I <- rename(.data = DIQ_I, DID070 = DIQ070)
DIQ_J <- rename(.data = DIQ_J, DID070 = DIQ070)
c("DIQ010","DIQ050","DID070") %in% names(DIQ_F) # 只有09后的有DIQ070变量

DIQ <- rbind(DIQ_D[,c("SEQN","DIQ010","DIQ050","DID070")],
             DIQ_E[,c("SEQN","DIQ010","DIQ050","DID070")],
             DIQ_F[,c("SEQN","DIQ010","DIQ050","DID070")],
             DIQ_G[,c("SEQN","DIQ010","DIQ050","DID070")],
             DIQ_H[,c("SEQN","DIQ010","DIQ050","DID070")],
             DIQ_I[,c("SEQN","DIQ010","DIQ050","DID070")],
             DIQ_J[,c("SEQN","DIQ010","DIQ050","DID070")]
) 

DIQ_dat <- rename(.data=DIQ, Diabetes1=DIQ010,#医生告诉你有糖尿病
                  Insulin=DIQ050,  #服用胰岛素
                  Sugar=DID070) #服用糖尿病药降低血糖

####__________血糖 & 血红蛋白__________####
GHB <- rbind(GHB_D[,c("SEQN","LBXGH")],
             GHB_E[,c("SEQN","LBXGH")],
             GHB_F[,c("SEQN","LBXGH")],
             GHB_G[,c("SEQN","LBXGH")],
             GHB_H[,c("SEQN","LBXGH")],
             GHB_I[,c("SEQN","LBXGH")],
             GHB_J[,c("SEQN","LBXGH")]
) 

GHB_dat <- rename(.data=GHB,Glycohemoglobin=LBXGH) 

GLU <- rbind(GLU_D[,c("SEQN","LBXGLU","WTSAF2YR")],
             GLU_E[,c("SEQN","LBXGLU","WTSAF2YR")],
             GLU_F[,c("SEQN","LBXGLU","WTSAF2YR")], # 子样本权重
             GLU_G[,c("SEQN","LBXGLU","WTSAF2YR")],
             GLU_H[,c("SEQN","LBXGLU","WTSAF2YR")],
             GLU_I[,c("SEQN","LBXGLU","WTSAF2YR")],
             GLU_J[,c("SEQN","LBXGLU","WTSAF2YR")]
) 

GLU_dat <- rename(.data=GLU,Glucose=LBXGLU,wt_glu=WTSAF2YR) 
  






## --------------MCQ 医疗条件-------------------
files <- list.files(pattern=".*_MCQ_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(MCQ_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}


####__________冠心病 心肌梗死 中风__________####
MCQ <- rbind(MCQ_D[,c("SEQN","MCQ160C","MCQ160E", "MCQ160F")],
             MCQ_E[,c("SEQN","MCQ160C","MCQ160E", "MCQ160F")],
             MCQ_F[,c("SEQN","MCQ160C","MCQ160E", "MCQ160F")],
             MCQ_G[,c("SEQN","MCQ160C","MCQ160E", "MCQ160F")],
             MCQ_H[,c("SEQN","MCQ160C","MCQ160E", "MCQ160F")],
             MCQ_I[,c("SEQN","MCQ160C","MCQ160E", "MCQ160F")],
             MCQ_J[,c("SEQN","MCQ160C","MCQ160E", "MCQ160F")]
) 

MCQ_dat <- rename(.data=MCQ, CHD=MCQ160C, MI=MCQ160E, Stroke=MCQ160F) 


## --------------是否怀孕-------------------
files <- list.files(pattern=".*_RHQ_.*", recursive=TRUE)
cat("找到", length(files), "个文件\n")
for(i in seq_along(files)){
  file <- files[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(files), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*?(RHQ_[A-Z]).*", "\\1", file)
  assign(varname, read.xport(file.path(file)))
}
c("SEQN","RHQ074","RHD143","RHQ200","RHD280","RHQ305") %in% names(RHQ_D)
c("SEQN","RHQ074","RHD143","RHQ200","RHD280","RHQ305") %in% names(RHQ_E)
c("SEQN","RHQ074","RHD143","RHQ200","RHD280","RHQ305") %in% names(RHQ_F)

RHQ <- rbind(RHQ_D[,c("SEQN","RHD143","RHQ200")],
             RHQ_E[,c("SEQN","RHD143","RHQ200")],
             RHQ_F[,c("SEQN","RHD143","RHQ200")],
             RHQ_G[,c("SEQN","RHD143","RHQ200")],
             RHQ_H[,c("SEQN","RHD143","RHQ200")],
             RHQ_I[,c("SEQN","RHD143","RHQ200")],
             RHQ_J[,c("SEQN","RHD143","RHQ200")]
            )

RHQ_dat <- rename(.data=RHQ,
                 # be_pregnant=RHQ074,
                  Pregnancy=RHD143,
                  Breastfeeding=RHQ200
                 )
head(RHQ_dat)


# 4. 合并所有变量--------------
if(F){
  dat01<-merge(DEMO_dat,ALQ_dat,by="SEQN", all.x = T)
  dat02<-merge(dat01,BMI_dat,by="SEQN", all.x = T)
  dat03<-merge(dat02,BPQ_dat,by="SEQN", all.x = T)
  dat04<-merge(dat03,MCQ_dat,by="SEQN", all.x = T)
  dat05<-merge(dat04,BPQ_dat,by="SEQN", all.x = T)
  dat06<-merge(dat05,DIQ_dat,by="SEQN", all.x = T)
  dat07<-merge(dat06,GHB_dat,by="SEQN", all.x = T)
  dat08<-merge(dat07,GLU_dat,by="SEQN", all.x = T)
  dat09<-merge(dat08,infla_dat,by="SEQN", all.x = T)
  dat10<-merge(dat09,SMQ_dat,by="SEQN", all.x = T)
}


###### another method to merge  #########
dat_names <- ls(pattern = "_dat$")
dat_names <- dat_names[! dat_names %in% c("CRP_dat", "wp_dat", "wt_dat", "exam_dat")]
dat_list <- lapply(dat_names, get)

# 合并前去除重复列
for (i in 2:length(dat_list)) {
  dup_cols <- intersect(names(dat_list[[1]]), names(dat_list[[i]]))
  dup_cols <- setdiff(dup_cols, "SEQN")
  if (length(dup_cols) > 0) {
    dat_list[[i]] <- dat_list[[i]][, !names(dat_list[[i]]) %in% dup_cols, drop=FALSE]
  }
}

merged_dat <- Reduce(function(x, y) merge(x, y, by = "SEQN", all.x = TRUE), dat_list)
dim(merged_dat)
# [1] 39346    57


write.csv(merged_dat, "mergeData.csv", row.names = FALSE)
