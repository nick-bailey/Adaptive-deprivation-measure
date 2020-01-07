* Encoding: UTF-8.
output close all.
dataset close all.
SET ONumbers=Labels OVars=Labels.

*  FRS - matdep adaptive - change v3.sps.
*   extract FRS mapdep qns for work on adaptive scale .
*   v3 - added 1718 and 1011, and equiv income and low inc pov.
*  matdep start 2004/5, then updated in 2010/11 which carries both old and new.
*  gross3 - weights var (household level).
*  sernum - household code.
*  benunit - benefit unit code.
*  person - person code.
*  orig items 2004/5 to 2010/11 - BUT  from 2008/9, only asked of U65 (or dep chld in BU).
* new items from 2010/11.
* OP items from 2008/9 - anyone in BU 65+.



* file handle. 
file handle frs /name='K:\Data Store\FRS'.
cd frs.



**************************************.
* SECTION A .
**************************************.

*** 1718 .
file handle temp / name="FRS 1718".

* benunit - depvn qns. 
* from 1718, houshew (reason why not able to keep home warm) dropped - but not part of MD set anyway.
GET
  FILE='temp/benunit.sav' 
   /keep sernum benunit 
            adddec addhol addins  addmon  adepfur af1 afdep2 houshe1 adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
* NB that houshe1 & houshew combine to give depvn indicator.
* NB that adbtbl and cdelply but recorded as 1 have/ 2 don't have. 
* from 2010/11(?) dropped: addeples [but 'adles' present] addmel addshoe cdepsum.

DATASET NAME benunit.
dataset activate benunit.
sort cases by sernum benunit.
* descriptives sernum.

* hbai - at benunit level.
*  from 1718, tentyp2 and tenure replaced with ptentyp2 or tenhbai (latter recommended) - diff codings.
* from 1718, hbenhh gone, ehbenbu appears.
* gnewhhp gone. 
GET
  FILE='temp/hbai.sav' 
   /keep sernum benunit gvtregn tenhbai ptentyp2 adulth depchldh ethgrphh
            s_oe_bhc s_oe_ahc ehbenbu eqobhchh eqoahchh low50bhc low60bhc low70bhc low50ahc low60ahc low70ahc
            mdch mdscorech lowincmdch lowincmdchsev 
            g_newhh gs_newhh gs_newbu gs_newad gs_newch gs_newwa gs_newpn gs_newpp.
dataset name hbai.
dataset activate hbai.
compute year=2017.
compute hbai=1.
sort cases by sernum benunit.
* descriptives sernum.

* merge benunit qns into hbai.
MATCH FILES /FILE=*
  /TABLE='benunit'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to adult data.
GET
  FILE='temp/adult.sav' 
   /keep sernum benunit person sex empstatc hdage iagegr2 iagegrp.
* dropped:  edattn3 . 
dataset name frs1718.
dataset activate frs1718.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv adult.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to child data.
GET
  FILE='temp/child.sav' 
   /keep sernum benunit person sex age .
dataset name frs1718ch.
dataset activate frs1718ch.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv child.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

dataset close benunit .
dataset close hbai.


*** 1617 .
file handle temp / name="FRS 1617".

* benunit - depvn qns. 
GET
  FILE='temp/benunit.sav'
   /keep sernum benunit 
            adddec addhol addins  addmon  adepfur af1 afdep2 houshe1 houshew adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
* NB that houshe1 & houshew combine to give depvn indicator.
* NB that adbtbl and cdelply but recorded as 1 have/ 2 don't have. 
* from 2010/11(?) dropped: addeples [but 'adles' present] addmel addshoe cdepsum.

DATASET NAME benunit.
dataset activate benunit.
sort cases by sernum benunit.
* descriptives sernum.

* hbai - at benunit level.
GET
  FILE='temp/hbai.sav' 
   /keep sernum benunit gvtregn tenure tentyp2 adulth depchldh ethgrphh
            s_oe_bhc s_oe_ahc hbenhh eqobhchh eqoahchh low50bhc low60bhc low70bhc low50ahc low60ahc low70ahc
            mdch mdscorech lowincmdch lowincmdchsev 
            g_newhh gnewhhp gs_newhh gs_newbu gs_newad gs_newch gs_newwa gs_newpn gs_newpp.
dataset name hbai.
dataset activate hbai.
compute year=2016.
compute hbai=1.
sort cases by sernum benunit.
* descriptives sernum.

* merge benunit qns into hbai.
MATCH FILES /FILE=*
  /TABLE='benunit'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to adult data.
GET
  FILE='temp/adult.sav' 
   /keep sernum benunit person sex empstatc hdage iagegr2 iagegrp.
* dropped:  edattn3 . 
dataset name frs1617.
dataset activate frs1617.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv adult.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to child data.
GET
  FILE='temp/child.sav' 
   /keep sernum benunit person sex age .
dataset name frs1617ch.
dataset activate frs1617ch.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv child.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

dataset close benunit .
dataset close hbai.


***  1516.
file handle temp / name="FRS 1516".

* benunit - depvn qns. 
GET
  FILE='temp/benunit.sav'
   /keep sernum benunit 
            adddec addhol addins  addmon  adepfur af1 afdep2 houshe1 houshew adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
* NB that houshe1 & houshew combine to give depvn indicator.
* dropped: addeples [but 'adles' present] addmel addshoe cdepsum.
*            debt1 debt2 debt3 debt4 debt5 debt6 debt7 debt8 debt9 .

DATASET NAME benunit.
dataset activate benunit.
sort cases by sernum benunit.
* descriptives sernum.

* hbai - at benunit level.
GET
  FILE='temp/hbai.sav' 
   /keep sernum benunit gvtregn tenure tentyp2 adulth depchldh ethgrphh
            s_oe_bhc s_oe_ahc hbenhh eqobhchh eqoahchh low50bhc low60bhc low70bhc low50ahc low60ahc low70ahc
            mdch mdscorech lowincmdch lowincmdchsev 
            g_newhh gnewhhp gs_newhh gs_newbu gs_newad gs_newch gs_newwa gs_newpn gs_newpp.
dataset name hbai.
dataset activate hbai.
compute year=2015.
compute hbai=1.
sort cases by sernum benunit.
* descriptives sernum.

* merge benunit qns into hbai.
MATCH FILES /FILE=*
  /TABLE='benunit'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to adult data.
GET
  FILE='temp/adult.sav' 
   /keep sernum benunit person sex empstatc hdage iagegr2 iagegrp.
* dropped:  edattn3 . 
dataset name frs1516.
dataset activate frs1516.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv adult.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to child data.
GET
  FILE='temp/child.sav' 
   /keep sernum benunit person sex age .
dataset name frs1516ch.
dataset activate frs1516ch.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv child.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

dataset close benunit .
dataset close hbai.


*** 1415 .
file handle temp / name="FRS 1415".

* benunit - depvn qns. 
GET
  FILE='temp/benunit.sav'
   /keep sernum benunit 
            adddec addhol addins  addmon  adepfur af1 afdep2 houshe1 houshew adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
* NB that houshe1 & houshew combine to give depvn indicator.
* dropped: addeples [but 'adles' present] addmel addshoe cdepsum.
*            debt1 debt2 debt3 debt4 debt5 debt6 debt7 debt8 debt9 .

DATASET NAME benunit.
dataset activate benunit.
sort cases by sernum benunit.
* descriptives sernum.

* hbai - at benunit level.
GET
  FILE='temp/hbai.sav' 
   /keep sernum benunit gvtregn tenure tentyp2 adulth depchldh ethgrphh
            s_oe_bhc s_oe_ahc hbenhh eqobhchh eqoahchh low50bhc low60bhc low70bhc low50ahc low60ahc low70ahc
            mdch mdscorech lowincmdch lowincmdchsev 
            g_newhh gnewhhp gs_newhh gs_newbu gs_newad gs_newch gs_newwa gs_newpn gs_newpp.
dataset name hbai.
dataset activate hbai.
compute year=2014.
compute hbai=1.
sort cases by sernum benunit.
* descriptives sernum.

* merge benunit qns into hbai.
MATCH FILES /FILE=*
  /TABLE='benunit'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to adult data.
GET
  FILE='temp/adult.sav' 
   /keep sernum benunit person sex empstatc hdage iagegr2 iagegrp.
* dropped:  edattn3 . 
dataset name frs1415.
dataset activate frs1415.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv adult.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to child data.
GET
  FILE='temp/child.sav' 
   /keep sernum benunit person sex age .
dataset name frs1415ch.
dataset activate frs1415ch.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv child.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

dataset close benunit .
dataset close hbai.


*** 1314 .
file handle temp / name="FRS 1314".

* benunit - depvn qns. 
GET
  FILE='temp/benunit.sav'
   /keep sernum benunit 
            adddec addhol addins  addmon  adepfur af1 afdep2 houshe1 houshew adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
* NB that houshe1 & houshew combine to give depvn indicator.
* dropped: addeples [but 'adles' present] addmel addshoe cdepsum.
*            debt1 debt2 debt3 debt4 debt5 debt6 debt7 debt8 debt9 .

DATASET NAME benunit.
dataset activate benunit.
sort cases by sernum benunit.
* descriptives sernum.

* hbai - at benunit level.
GET
  FILE='temp/hbai.sav' 
   /keep sernum benunit gvtregn tenure tentyp2 adulth depchldh ethgrphh
            s_oe_bhc s_oe_ahc hbenhh eqobhchh eqoahchh low50bhc low60bhc low70bhc low50ahc low60ahc low70ahc
            mdch mdscorech lowincmdch lowincmdchsev 
            g_newhh gnewhhp gs_newhh gs_newbu gs_newad gs_newch gs_newwa gs_newpn gs_newpp.
dataset name hbai.
dataset activate hbai.
compute year=2013.
compute hbai=1.
sort cases by sernum benunit.
* descriptives sernum.

* merge benunit qns into hbai.
MATCH FILES /FILE=*
  /TABLE='benunit'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to adult data.
GET
  FILE='temp/adult.sav' 
   /keep sernum benunit person sex empstatc hdage iagegr2 iagegrp.
* dropped:  edattn3 . 
dataset name frs1314.
dataset activate frs1314.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv adult.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to child data.
GET
  FILE='temp/child.sav' 
   /keep sernum benunit person sex age .
dataset name frs1314ch.
dataset activate frs1314ch.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv child.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

dataset close benunit .
dataset close hbai.


*** 1213 .
file handle temp / name="FRS 1213".

* benunit - depvn qns. 
GET
  FILE='temp/benunit.sav'
   /keep sernum benunit 
            adddec addhol addins  addmon  adepfur af1 afdep2 houshe1 houshew adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
* NB that houshe1 & houshew combine to give depvn indicator.
* dropped: addeples [but 'adles' present] addmel addshoe cdepsum.
*            debt1 debt2 debt3 debt4 debt5 debt6 debt7 debt8 debt9 .

DATASET NAME benunit.
dataset activate benunit.
sort cases by sernum benunit.
* descriptives sernum.

* hbai - at benunit level.
GET
  FILE='temp/hbai.sav' 
   /keep sernum benunit gvtregn tenure tentyp2 adulth depchldh ethgrphh
            s_oe_bhc s_oe_ahc hbenhh eqobhchh eqoahchh low50bhc low60bhc low70bhc low50ahc low60ahc low70ahc
            mdch mdscorech lowincmdch lowincmdchsev 
            g_newhh gnewhhp gs_newhh gs_newbu gs_newad gs_newch gs_newwa gs_newpn gs_newpp.
dataset name hbai.
dataset activate hbai.
compute year=2012.
compute hbai=1.
sort cases by sernum benunit.
* descriptives sernum.

* merge benunit qns into hbai.
MATCH FILES /FILE=*
  /TABLE='benunit'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to adult data.
GET
  FILE='temp/adult.sav' 
   /keep sernum benunit person sex empstatc hdage iagegr2 iagegrp.
* dropped:  edattn3 . 
dataset name frs1213.
dataset activate frs1213.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv adult.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to child data.
GET
  FILE='temp/child.sav' 
   /keep sernum benunit person sex age .
dataset name frs1213ch.
dataset activate frs1213ch.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv child.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

dataset close benunit .
dataset close hbai.


*** 1112 .
file handle temp / name="FRS 1112".

* benunit - depvn qns. 
*** NB that in 2011/12, housew missing. 
GET
  FILE='temp/benunit.sav'
   /keep sernum benunit 
            adddec addhol addins  addmon  adepfur af1 afdep2 houshe1 adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
* NB that houshe1 & houshew combine to give depvn indicator.
* dropped: addeples [but 'adles' present] addmel addshoe cdepsum.
*            debt1 debt2 debt3 debt4 debt5 debt6 debt7 debt8 debt9 .

DATASET NAME benunit.
dataset activate benunit.
sort cases by sernum benunit.
* descriptives sernum.

* hbai - at benunit level.
GET
  FILE='temp/hbai.sav' 
   /keep sernum benunit gvtregn tenure tentyp2 adulth depchldh ethgrphh
            s_oe_bhc s_oe_ahc hbenhh eqobhchh eqoahchh low50bhc low60bhc low70bhc low50ahc low60ahc low70ahc
            mdch mdscorech lowincmdch lowincmdchsev 
            g_newhh gnewhhp gs_newhh gs_newbu gs_newad gs_newch gs_newwa gs_newpn gs_newpp.
dataset name hbai.
dataset activate hbai.
compute year=2011.
compute hbai=1.
sort cases by sernum benunit.
* descriptives sernum.

* merge benunit qns into hbai.
MATCH FILES /FILE=*
  /TABLE='benunit'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to adult data.
GET
  FILE='temp/adult.sav' 
   /keep sernum benunit person sex empstatc hdage iagegr2 iagegrp.
* dropped:  edattn3 . 
dataset name frs1112.
dataset activate frs1112.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv adult.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to child data.
GET
  FILE='temp/child.sav' 
   /keep sernum benunit person sex age .
dataset name frs1112ch.
dataset activate frs1112ch.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv child.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

dataset close benunit .
dataset close hbai.


*** 1011 .
file handle temp / name="FRS 1011".

* benunit - depvn qns. 
*** NB that before 2011/12, housew missing. 
GET
  FILE='temp/benunit.sav'
   /keep sernum benunit 
            adddec addhol addins  addmon  adepfur af1 afdep2 houshe1 adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
* NB that houshe1 & houshew combine to give depvn indicator.
* dropped: addeples [but 'adles' present] addmel addshoe cdepsum.
*            debt1 debt2 debt3 debt4 debt5 debt6 debt7 debt8 debt9 .

DATASET NAME benunit.
dataset activate benunit.
sort cases by sernum benunit.
* descriptives sernum.

* hbai - at benunit level.
*  in 10/11, mdscorech is based on old set of items - use mdscorechnew.
GET
  FILE='temp/hbai.sav' 
   /keep sernum benunit gvtregn tenure tentyp2 adulth depchldh ethgrphh
            s_oe_bhc s_oe_ahc hbenhh eqobhchh eqoahchh low50bhc low60bhc low70bhc low50ahc low60ahc low70ahc
            mdch mdscorechnew lowincmdch lowincmdchsev 
            g_newhh gnewhhp gs_newhh gs_newbu gs_newad gs_newch gs_newwa gs_newpn gs_newpp.
dataset name hbai.
dataset activate hbai.
RENAME VARIABLES (mdscorechnew = mdscorech).
compute year=2010.
compute hbai=1.
sort cases by sernum benunit.
* descriptives sernum.

* merge benunit qns into hbai.
MATCH FILES /FILE=*
  /TABLE='benunit'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to adult data.
GET
  FILE='temp/adult.sav' 
   /keep sernum benunit person sex empstatc hdage iagegr2 iagegrp.
* dropped:  edattn3 . 
dataset name frs1011.
dataset activate frs1011.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv adult.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

* attach hbai data to child data.
GET
  FILE='temp/child.sav' 
   /keep sernum benunit person sex age .
dataset name frs1011ch.
dataset activate frs1011ch.
sort cases by sernum benunit person.
* descriptives sernum.

* match benunit-level data on matdep to indiv child.
MATCH FILES /FILE=*
  /TABLE='hbai'
  /BY SERNUM BENUNIT.
EXECUTE.

dataset close benunit .
dataset close hbai.




*** part B: merging.

* merge adult data.
dataset activate frs1718.
add files file = *
 /file = frs1617 
 /file = frs1516 
 /file = frs1415 
 /file = frs1314 
 /file = frs1213 
 /file = frs1112 
 /file = frs1011.
execute.

dataset close frs1617.
dataset close frs1516.
dataset close frs1415.
dataset close frs1314.
dataset close frs1213.
dataset close frs1112.
dataset close frs1011.

* merge child data.
dataset activate frs1718ch.
add files file = *
 /file = frs1617ch 
 /file = frs1516ch 
 /file = frs1415ch 
 /file = frs1314ch 
 /file = frs1213ch 
 /file = frs1112ch 
 /file = frs1011ch .
execute.

dataset close frs1617ch.
dataset close frs1516ch.
dataset close frs1415ch.
dataset close frs1314ch.
dataset close frs1213ch.
dataset close frs1112ch.
dataset close frs1011ch.

* 'adhous' - keep house warm. 
*** for v2 cw v1, dropping use of houshew. 
* previous explored creating version using houshew so closer to other 'lack' items. 
* BUT clear this isn't how DWP do it. 
* Nevertheless, retain renaming to avoid disruption to later code including R code. 
rename variables (houshe1 = adhous).
var labels adhous 'keep home adequately warm'.
execute.

* remove empty cases .
* freq cmd shows that cases that miss one depvn item miss all - v few each year. 
* freq adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact.
select if (not sysmis(cdelply)).

* make sum of depvns - NB that this gives value '0' even if all sysmis.
count mdsum = adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact (2).
freq mdsum.
crosstabs mdsum by mdch.

* calculate prev figures. 
weight by gs_newch.
crosstabs adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
            by year /cells column.

* testing v1 - [1 and 3] as % of [1-4].
temp. 
recode adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
            (1, 3=1) (2, 4=0).
formats adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
              (f5.4).
means adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
      by year /cells mean.

* testing v2 - [1] as % of [1-3] - looks closer in 2016 at least.
temp. 
recode adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
            (1=1) (2, 3=0) (4=-9).
formats adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
              (f5.4).
means adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
      by year /cells mean.



*** to create csv.
* reduce dep qs as lack - 0/1.
recode  adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
            (1,3,4=0) (2=1) (else=-1)
   into
              home_decor ad_holiday insurance savings furniture fridge money_self home_warm bills 
             outdoor_play bedrooms celebrate leisure ch_holiday  hobby  friend_round school_trip playgroup fruit_veg warm_coat activities.
variable level 
   home_decor ad_holiday insurance savings furniture fridge money_self home_warm bills 
   outdoor_play bedrooms celebrate leisure ch_holiday  hobby  friend_round school_trip playgroup fruit_veg warm_coat activities (ORDINAL).
missing values 
   home_decor ad_holiday insurance savings furniture fridge money_self home_warm bills 
   outdoor_play bedrooms celebrate leisure ch_holiday  hobby  friend_round school_trip playgroup fruit_veg warm_coat activities (-1).
value labels
    home_decor ad_holiday insurance savings furniture fridge money_self home_warm bills 
   outdoor_play bedrooms celebrate leisure ch_holiday  hobby  friend_round school_trip playgroup fruit_veg warm_coat activities
   0 'Not lack' 1 'Lack'.
freq home_decor ad_holiday insurance savings furniture fridge money_self home_warm bills
   outdoor_play bedrooms celebrate leisure ch_holiday hobby friend_round school_trip playgroup fruit_veg warm_coat activities.

* reduce dep qs to have (for prevalence) - 0/1.
recode  adddec addhol addins  addmon  adepfur af1 afdep2 adhous adbtbl 
            cdelply cdepbed cdepcel cdepeqp cdephol cdeples cdeptea cdeptrp cplay cdepveg cdpcoat cdepact 
            (1=1) (2,3=0) (else=-1)
   into
              have_home_decor have_ad_holiday have_insurance have_savings have_furniture have_fridge have_money_self have_home_warm have_bills 
             have_outdoor_play have_bedrooms have_celebrate have_leisure have_ch_holiday  have_hobby  have_friend_round have_school_trip 
                have_playgroup have_fruit_veg have_warm_coat have_activities.
variable level 
              have_home_decor have_ad_holiday have_insurance have_savings have_furniture have_fridge have_money_self have_home_warm have_bills 
             have_outdoor_play have_bedrooms have_celebrate have_leisure have_ch_holiday  have_hobby  have_friend_round have_school_trip 
                have_playgroup have_fruit_veg have_warm_coat have_activities (ORDINAL).
missing values 
              have_home_decor have_ad_holiday have_insurance have_savings have_furniture have_fridge have_money_self have_home_warm have_bills 
             have_outdoor_play have_bedrooms have_celebrate have_leisure have_ch_holiday  have_hobby  have_friend_round have_school_trip 
                have_playgroup have_fruit_veg have_warm_coat have_activities (-1).
value labels
              have_home_decor have_ad_holiday have_insurance have_savings have_furniture have_fridge have_money_self have_home_warm have_bills 
             have_outdoor_play have_bedrooms have_celebrate have_leisure have_ch_holiday  have_hobby  have_friend_round have_school_trip 
                have_playgroup have_fruit_veg have_warm_coat have_activities 0 'Not have' 1 'Have'.
weight by gs_newch.
freq 
              have_home_decor have_ad_holiday have_insurance have_savings have_furniture have_fridge have_money_self have_home_warm have_bills 
             have_outdoor_play have_bedrooms have_celebrate have_leisure have_ch_holiday  have_hobby  have_friend_round have_school_trip 
                have_playgroup have_fruit_veg have_warm_coat have_activities.
means
              have_home_decor have_ad_holiday have_insurance have_savings have_furniture have_fridge have_money_self have_home_warm have_bills 
             have_outdoor_play have_bedrooms have_celebrate have_leisure have_ch_holiday  have_hobby  have_friend_round have_school_trip 
                have_playgroup have_fruit_veg have_warm_coat have_activities by year /cells mean.

* other vars.
variable level year mdch (ordinal). 

* create csv.
SAVE TRANSLATE OUTFILE='frs_md.csv'
  /TYPE=CSV
  /ENCODING='UTF8'
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES
  /keep = 
   year 
     home_decor ad_holiday insurance savings furniture fridge money_self home_warm bills
       outdoor_play bedrooms celebrate leisure ch_holiday hobby friend_round school_trip playgroup fruit_veg warm_coat activities
    have_home_decor have_ad_holiday have_insurance have_savings have_furniture have_fridge have_money_self have_home_warm have_bills 
       have_outdoor_play have_bedrooms have_celebrate have_leisure have_ch_holiday  have_hobby  have_friend_round have_school_trip 
       have_playgroup have_fruit_veg have_warm_coat have_activities
    mdch mdscorech gs_newch
    s_oe_bhc s_oe_ahc low60bhc low60ahc. 

