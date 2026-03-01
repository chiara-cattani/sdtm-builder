/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : ML.sas
 PURPOSE       : Create SDTM dataset for Meal Data
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-03 - Chiara Cattani - Initial program
 2025-12-11 - Chiara Cattani - Updated records for Non study feeding.
 2025-12-23 - Chiara Cattani - MLEVINTX="SINCE LAST VISIT" for post-screening records.
 							   MLSCAT="FIBER OR IRON FORTIFIED FOODS OR DRINKS" for solid wf
 							   Updated logic when MLTRT=Other.
 							   Updated MLENRTPT.
 2026-01-08 - Chiara Cattani - Use MLENDTC from chk for breastfeeding records.
 							   Leave MLPRESP/MLOCCUR empty when MLTRT is free text.
 							   Populate MLPRESP/MLOCCUR for SOLID WEANING FOODS.
 2026-01-09 - Chiara Cattani - Update MLEVINTX for MLFYST.
 2026-01-27 - Chiara Cattani - Upcase MLSCAT.
 							   Updated MLSPID.
 							   Added MLLNKID.
 							   Added MLDURX and updated MLDUR.
 							   Updated MLEVINTX as per latest aCRF.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/* Import source data */
%copy_import (inset = chk);
%copy_import (inset = ml);

/* CHK */
data work.chk1;
  set work.chk_1;
  if not missing(chkbrend);
  keep usubjid chkbrend;
run;

/* ML */
proc sort data = ml_1;
	by usubjid mlfcid;
run;

data ml2;
	length studyid domain subjid sourceid mlspid
		   mlcat mlevintx mlpresp mloccur mlscat mltrt mlstdtc mlendtc mldosfrq_ mldosfrq_text mldosu mldostxt mldur mlentpt mlenrtpt $200 mldose 8.;
	set ml_1;
	
	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("ML", $form.));
	%derive_visit_vars;
	
	if eventid="SCR" then do;
	    /* Milk powders - a) Young Child Formula/ Growing up milk */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "DURING 3 WEEKS PRIOR TO VISIT 0";
	    if mlf1a = "Y" then do;
			mlscat  = "Milk powders - a) Young Child Formula/ Growing up milk";
			if not missing(MLYCB1) or not missing(MLYCSP1) then do;
				if not missing(MLYCB1) and upcase(MLYCB1) ne "OTHER" then do;
					mltrt = strip(MLYCB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLYCSP1) then do;
    				mltrt = strip(MLYCSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = mlycsd1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = mlyced1, dtc = mlendtc);
				mldosfrq_ = strip(put(mlycsnx1, best.));
				mldose   = mlycvnx1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLYCB2) or not missing(MLYCSP2) then do;
				if not missing(MLYCB2) and upcase(MLYCB2) ne "OTHER" then do;
					mltrt = strip(MLYCB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLYCSP2) then do;
    				mltrt = strip(MLYCSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = mlycsd2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = mlyced2, dtc = mlendtc);
				mldosfrq_ = strip(put(mlycsnx2, best.));
				mldose   = mlycvnx2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		else if mlf1a = "N" then do;
			mlpresp = "Y";
			mloccur = "N";
			mlscat  = "Milk powders - a) Young Child Formula/ Growing up milk";
			mltrt   = "Milk powders - a) Young Child Formula/ Growing up milk";
			call missing(mlstdtc, mlendtc, mldosfrq_, mldosu, mldostxt, mldur, mlentpt, mlenrtpt, mldose);
            mlspid   = strip(put(mlfcid, best.));
			output;
		end;
		/* Milk powders - b) (Choco) powder drink */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "DURING 3 WEEKS PRIOR TO VISIT 0";
	    if mlf1b = "Y" then do;
			mlscat  = "Milk powders - b) (Choco) powder drink";
			if not missing(MLMPB1) or not missing(MLMPSP1) then do;
				if not missing(MLMPB1) and upcase(MLMPB1) ne "OTHER" then do;
					mltrt = strip(MLMPB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLMPSP1) then do;
    				mltrt = strip(MLMPSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLMPSD1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLMPED1, dtc = mlendtc);
				mldosfrq_ = strip(put(MLMPSNX1, best.));
				mldose   = MLMPVNX1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLMPB2) or not missing(MLMPSP2) then do;
				if not missing(MLMPB2) and upcase(MLMPB2) ne "OTHER" then do;
					mltrt = strip(MLMPB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;
				else if not missing(MLMPSP2) then do;
					mltrt = strip(MLMPSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLMPSD2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLMPED2, dtc = mlendtc);
				mldosfrq_ = strip(put(MLMPSNX2, best.));
				mldose   = MLMPVNX2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		else if mlf1b = "N" then do;
			mlpresp = "Y";
			mloccur = "N";
			mlscat  = "Milk powders - b) (Choco) powder drink";
			mltrt   = "Milk powders - b) (Choco) powder drink";
			call missing(mlstdtc, mlendtc, mldosfrq_, mldosu, mldostxt, mldur, mlentpt, mlenrtpt, mldose);
            mlspid   = strip(put(mlfcid, best.));
			output;
		end;
		/* Milk powders - c) Follow on formula (stage 2: 6-12 months) */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "DURING 3 WEEKS PRIOR TO VISIT 0";
	    if mlf1c = "Y" then do;
			mlscat  = "Milk powders - c) Follow on formula (stage 2: 6-12 months)";
			if not missing(MLFFB1) or not missing(MLFFSP1) then do;
				if not missing(MLFFB1) and upcase(MLFFB1) ne "OTHER" then do;
					mltrt = strip(MLFFB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;    			
    			else if not missing(MLFFSP1) then do;
    				mltrt = strip(MLFFSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLFFSD1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLFFED1, dtc = mlendtc);
				mldosfrq_ = strip(put(MLFFSNX1, best.));
				mldose   = MLFFVNX1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLFFB2) or not missing(MLFFSP2) then do;
				if not missing(MLFFB2) and upcase(MLFFB2) ne "OTHER" then do;
					mltrt = strip(MLFFB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;    			
    			else if not missing(MLFFSP2) then do;
    				mltrt = strip(MLFFSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLFFSD2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLFFED2, dtc = mlendtc);
				mldosfrq_ = strip(put(MLFFSNX2, best.));
				mldose   = MLFFVNX2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		else if mlf1c = "N" then do;
			mlpresp = "Y";
			mloccur = "N";
			mlscat  = "Milk powders - c) Follow on formula (stage 2: 6-12 months)";
			mltrt   = "Milk powders - c) Follow on formula (stage 2: 6-12 months)";
			call missing(mlstdtc, mlendtc, mldosfrq_, mldosu, mldostxt, mldur, mlentpt, mlenrtpt, mldose);
            mlspid   = strip(put(mlfcid, best.));
			output;
		end;
		/* Ready to drink liquid milk */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "DURING 3 WEEKS PRIOR TO VISIT 0";
	    if MLF2 = "Y" then do;
			mlscat  = "Ready to drink liquid milk";
			if not missing(MLLMB1) or not missing(MLLMSP1) then do;
				if not missing(MLLMB1) and upcase(MLLMB1) ne "OTHER" then do;
					mltrt = strip(MLLMB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLLMSP1) then do;
    				mltrt = strip(MLLMSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLLMSD1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLLMED1, dtc = mlendtc);
				mldosfrq_ = strip(put(MLLMSNX1, best.));
				mldose   = MLLMVNX1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLLMB2) or not missing(MLLMSP2) then do;
				if not missing(MLLMB2) and upcase(MLLMB2) ne "OTHER" then do;
					mltrt = strip(MLLMB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;					
    			else if not missing(MLLMSP2) then do;
    				mltrt = strip(MLLMSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLLMSD2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLLMED2, dtc = mlendtc);
				mldosfrq_ = strip(put(MLLMSNX2, best.));
				mldose   = MLLMVNX2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		else if MLF2 = "N" then do;
			mlpresp = "Y";
			mloccur = "N";
			mlscat  = "Milk powders - b) (Choco) powder drink";
			mltrt   = "Milk powders - b) (Choco) powder drink";
			call missing(mlstdtc, mlendtc, mldosfrq_, mldosu, mldostxt, mldur, mlentpt, mlenrtpt, mldose);
            mlspid   = strip(put(mlfcid, best.));
			output;
		end;
		/* Condensed milk */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "DURING 3 WEEKS PRIOR TO VISIT 0";
	    if MLF3 = "Y" then do;
			mlscat  = "Condensed milk";
			if not missing(MLCMB1) or not missing(MLCMSP1) then do;
				if not missing(MLCMB1) and upcase(MLCMB1) ne "OTHER" then do;
					mltrt = strip(MLCMB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLCMSP1) then do;
    				mltrt = strip(MLCMSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLCMSD1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLCMED1, dtc = mlendtc);
				mldosfrq_ = strip(put(MLCMSNX1, best.));
				mldose   = MLCMVNX1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLCMB2) or not missing(MLCMSP2) then do;
				if not missing(MLCMB2) and upcase(MLCMB2) ne "OTHER" then do;
					mltrt = strip(MLCMB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLCMSP2) then do;
    				mltrt = strip(MLCMSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLCMSD2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLCMED2, dtc = mlendtc);
				mldosfrq_ = strip(put(MLCMSNX2, best.));
				mldose   = MLCMVNX2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		else if MLF3 = "N" then do;
			mlpresp = "Y";
			mloccur = "N";
			mlscat  = "Milk powders - b) (Choco) powder drink";
			mltrt   = "Milk powders - b) (Choco) powder drink";
			call missing(mlstdtc, mlendtc, mldosfrq_, mldosu, mldostxt, mldur, mlentpt, mlenrtpt, mldose);
            mlspid   = strip(put(mlfcid, best.));
			output;
		end;
		/* Breastfeeding */
		mlspid   = strip(put(mlfcid, best.));
		mlcat    = "HUMAN MILK";
		mltrt    = "HUMAN MILK";
	    mlevintx = "SINCE BIRTH";
	    mlscat   = "";
	    if MLBR = "Y" then do;
			mlpresp = "Y";
			mloccur = "Y";
			mldur   = strip(ml_brdu);
			if not missing(mlbrongo) then do;
	    		mlentpt = "VISIT 0";
				if mlbrongo = "N" then mlenrtpt = "BEFORE";
				else if mlbrongo = "Y" then mlenrtpt = "ONGOING";
			end;
			else call missing(mlentpt, mlenrtpt);
			call missing(mlstdtc, mlendtc, mldosfrq_, mldosu, mldostxt, mldose);
			output;
		end;
		else if MLBR = "N" then do;
			mlpresp = "Y";
			mloccur = "N";
			call missing(mlstdtc, mlendtc, mldosfrq_, mldosu, mldostxt, mldur, mlentpt, mlenrtpt, mldose);
			output;
		end;
		/* Solid weaning foods */
		if not missing(mlsfwage) then do;
			mlcat    = "NON-STUDY PRODUCT FEEDING";
			mltrt    = "SOLID WEANING FOODS";
	    	mlevintx = "";
	    	mlscat   = "SOLID WEANING FOODS";
	    	mlpresp = "Y";
			mloccur = "Y";
			call missing(mlstdtc, mlendtc, mldosfrq_, mldosu, mldostxt, mldur, mlentpt, mlenrtpt, mldose);
	    	output;
	    end;
		/* Fiber or iron fortified foods or drinks */
		if not missing(MLFYNST) then do;
			mlcat    = "NON-STUDY PRODUCT FEEDING";
	    	mlevintx = "DURING 3 WEEKS PRIOR TO VISIT 0";
	    	mlscat   = "FIBER OR IRON FORTIFIED FOODS OR DRINKS";
	    	mltrt    = strip(MLFYNST);
			mldostxt = strip(MLFYNSA);
			mldosfrq_text = strip(MLFYNSF);
			mlpresp = "";
			mloccur = "";
			call missing(mlstdtc, mlendtc, mldosu, mldur, mlentpt, mlenrtpt, mldose);
			output;
		end;
	end;
	
	else if eventid ne "SCR" then do;
	    /* Milk powders - a) Young Child Formula/ Growing up milk */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "SINCE LAST VISIT";
	    if mlf1a = "Y" then do;
			mlscat  = "Milk powders - a) Young Child Formula/ Growing up milk";
			if not missing(MLYCB1) or not missing(MLYCSP1) then do;
				if not missing(MLYCB1) and upcase(MLYCB1) ne "OTHER" then do;
					mltrt = strip(MLYCB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLYCSP1) then do;
    				mltrt = strip(MLYCSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = mlycsd1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = mlyced1, dtc = mlendtc);
				mldosfrq_ = strip(put(mlycsnx1, best.));
				mldose   = mlycvnx1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLYCB2) or not missing(MLYCSP2) then do;
				if not missing(MLYCB2) and upcase(MLYCB2) ne "OTHER" then do;
					mltrt = strip(MLYCB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLYCSP2) then do;
    				mltrt = strip(MLYCSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = mlycsd2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = mlyced2, dtc = mlendtc);
				mldosfrq_ = strip(put(mlycsnx2, best.));
				mldose   = mlycvnx2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		/* Milk powders - b) (Choco) powder drink */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "SINCE LAST VISIT";
	    if mlf1b = "Y" then do;
			mlscat  = "Milk powders - b) (Choco) powder drink";
			if not missing(MLMPB1) or not missing(MLMPSP1) then do;
				if not missing(MLMPB1) and upcase(MLMPB1) ne "OTHER" then do;
					mltrt = strip(MLMPB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLMPSP1) then do; 
    				mltrt = strip(MLMPSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLMPSD1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLMPED1, dtc = mlendtc);
				mldosfrq_ = strip(put(MLMPSNX1, best.));
				mldose   = MLMPVNX1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLMPB2) or not missing(MLMPSP2) then do;
				if not missing(MLMPB2) and upcase(MLMPB2) ne "OTHER" then do;
					mltrt = strip(MLMPB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLMPSP2) then do;
    				mltrt = strip(MLMPSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLMPSD2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLMPED2, dtc = mlendtc);
				mldosfrq_ = strip(put(MLMPSNX2, best.));
				mldose   = MLMPVNX2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		/* Milk powders - c) Follow on formula (stage 2: 6-12 months) */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "SINCE LAST VISIT";
	    if mlf1c = "Y" then do;
			mlscat  = "Milk powders - c) Follow on formula (stage 2: 6-12 months)";
			if not missing(MLFFB1) or not missing(MLFFSP1) then do;
				if not missing(MLFFB1) and upcase(MLFFB1) ne "OTHER" then do;
					mltrt = strip(MLFFB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLFFSP1) then do;
    				mltrt = strip(MLFFSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLFFSD1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLFFED1, dtc = mlendtc);
				mldosfrq_ = strip(put(MLFFSNX1, best.));
				mldose   = MLFFVNX1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLFFB2) or not missing(MLFFSP2) then do;
				if not missing(MLFFB2) and upcase(MLFFB2) ne "OTHER" then do;
					mltrt = strip(MLFFB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLFFSP2) then do;
    				mltrt = strip(MLFFSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLFFSD2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLFFED2, dtc = mlendtc);
				mldosfrq_ = strip(put(MLFFSNX2, best.));
				mldose   = MLFFVNX2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		/* Ready to drink liquid milk */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "SINCE LAST VISIT";
	    if MLF2 = "Y" then do;
			mlscat  = "Ready to drink liquid milk";
			if not missing(MLLMB1) or not missing(MLLMSP1) then do;
				if not missing(MLLMB1) and upcase(MLLMB1) ne "OTHER" then do;
					mltrt = strip(MLLMB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;    			
    			else if not missing(MLLMSP1) then do;
    				mltrt = strip(MLLMSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLLMSD1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLLMED1, dtc = mlendtc);
				mldosfrq_ = strip(put(MLLMSNX1, best.));
				mldose   = MLLMVNX1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLLMB2) or not missing(MLLMSP2) then do;
				if not missing(MLLMB2) and upcase(MLLMB2) ne "OTHER" then do;
					mltrt = strip(MLLMB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;    			
    			else if not missing(MLLMSP2) then do;
    				mltrt = strip(MLLMSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLLMSD2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLLMED2, dtc = mlendtc);
				mldosfrq_ = strip(put(MLLMSNX2, best.));
				mldose   = MLLMVNX2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		/* Condensed milk */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "SINCE LAST VISIT";
	    if MLF3 = "Y" then do;
			mlscat  = "Condensed milk";
			if not missing(MLCMB1) or not missing(MLCMSP1) then do;
				if not missing(MLCMB1) and upcase(MLCMB1) ne "OTHER" then do;
					mltrt = strip(MLCMB1);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLCMSP1) then do;
    				mltrt = strip(MLCMSP1);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLCMSD1, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLCMED1, dtc = mlendtc);
				mldosfrq_ = strip(put(MLCMSNX1, best.));
				mldose   = MLCMVNX1;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"A";
			end;
			output;
			if not missing(MLCMB2) or not missing(MLCMSP2) then do;
				if not missing(MLCMB2) and upcase(MLCMB2) ne "OTHER" then do;
					mltrt = strip(MLCMB2);
					mlpresp = "Y";
					mloccur = "Y";
				end;
    			else if not missing(MLCMSP2) then do;
    				mltrt = strip(MLCMSP2);
    				call missing(mlpresp, mloccur);
    			end;
    			%sdtm_dt2dtc(dt = MLCMSD2, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLCMED2, dtc = mlendtc);
				mldosfrq_ = strip(put(MLCMSNX2, best.));
				mldose   = MLCMVNX2;
				mldosu   = "mL";
				call missing(mldostxt, mldur, mlentpt, mlenrtpt);
				mlspid   = strip(put(mlfcid, best.))||"B";
			end;
			output;
		end;
		/* Any new fiber or iron fortified foods or drinks */
		mlcat    = "NON-STUDY PRODUCT FEEDING";
	    mlevintx = "SINCE LAST VISIT";
	    if MLF4 = "Y" then do;
			mlscat  = "FIBER OR IRON FORTIFIED FOODS OR DRINKS";
			if not missing(MLFYNSTC) then do;
				mltrt = strip(MLFYNSTC);
    			%sdtm_dt2dtc(dt = MLSFWSD, dtc = mlstdtc);
    			%sdtm_dt2dtc(dt = MLSFWED, dtc = mlendtc);
				mldosfrq_text = strip(MLFYNSFC);
				mldostxt = strip(MLFYNSAC);
				mldosu   = "";
				call missing(mldose, mldur, mlentpt, mlenrtpt, mlpresp, mloccur);
				mlspid   = strip(put(mlfcid, best.));
			end;
			output;
		end;
	end;
run;

/* Merge */
data ml_merged;
	merge chk1 ml2(in=a);
	by usubjid;
	if a;
	if mltrt = "HUMAN MILK" then do;
		%sdtm_dt2dtc(dt = chkbrend, dtc = mlendtc);
	end;
run;

/* Derive MLSTDY and MLENDY */
proc sql;
  create table work.ml_final as
    select a.* ,b.rfstdtc
      from 
        work.ml_merged as a
      left join 
        sdtm.dm         as b 
      on a.usubjid = b.usubjid;
quit;

data work.&sdtm_domain;
	length mlstdy mlendy 8. mllnkid mldosfrq mldurx $200;
    set work.ml_final;
    %sdtm_dtc2dy(dtc= mlstdtc);
    %sdtm_dtc2dy(dtc= mlendtc);
    mlscat = upcase(mlscat);
    mllnkid=strip(mltrt);
    if mldosfrq_ = "1" then mldosfrq = "QD";
    else if strip(mldosfrq_) = "2" then mldosfrq = "BID";
    else if strip(mldosfrq_) = "3" then mldosfrq = "TID";
    else if not missing(mldosfrq_) then mldosfrq = strip(mldosfrq_) || " TIMES PER DAY";
    else if not missing(mldosfrq_text) then mldosfrq = strip(mldosfrq_text);
    else call missing(mldosfrq);
    mldurx = mldur;
    call missing(mldur);
    label mldurx = "Duration of Meal Specification";
run;

/* Finalize */
%sdtm_create_domain_v2 ( domain     = &sdtm_domain
                       , inset      = &sdtm_domain
                       , keys       = STUDYID USUBJID MLCAT MLSCAT MLTRT MLSTDTC VISITNUM  
                       , addsuppvars = mldurx
                       , metavarsset    = metadata.sdtm_study_variables
                       , metadomainsset = metadata.sdtm_study_domains
                       , deldupkeys = N);