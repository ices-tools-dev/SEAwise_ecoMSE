// Create a file to run the TMB version of sprat
#include <TMB.hpp>
#include <iostream>

template<class Type>
Type posfun(Type x, Type eps, Type &pen) {
  pen += CppAD::CondExpLt(x,eps,Type(0.01)*pow(x-eps,2),Type(0));
  Type xp = -(x/eps-1);
  return CppAD::CondExpGe(x,eps,x,
    eps*(1/(1+xp+pow(xp,2)+pow(xp,3)+pow(xp,4)+pow(xp,5))));
}

template<class Type>
Type average(vector<Type> xin){
  Type avgtmp = 0;
  for(int i=0;i<xin.size();i++){
    avgtmp += xin(i);
  }
Type export_number = avgtmp/xin.size();

return(export_number);
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data input
  DATA_ARRAY(weca); // Weight in the beginning of the year
  DATA_ARRAY(west); // Weight in catch
  DATA_ARRAY(Surveyobs); // Surveys
  DATA_ARRAY(Catchobs); // Catch observations
  DATA_ARRAY(env_matrix); // environmental observations
  DATA_ARRAY(M_matrix); // Predator observations (preferably consumption)
//   // //
// // // // Age
  DATA_VECTOR(age); // ages
  DATA_VECTOR(Fbarage); // min and max ages used in Fbar calculations
  DATA_INTEGER(nage); // Number of age groups
  DATA_INTEGER(nseason);
  DATA_INTEGER(nyears); // Number of years
  DATA_INTEGER(nsurvey); // Number of surveys
  DATA_INTEGER(nenv); // number of environemtnal covariates
  DATA_INTEGER(nalphaM); // Number of external predators
  //DATA_INTEGER(nMext); // Number of unique age groupings for external predator s
  DATA_INTEGER(recseason); // Season where recruitment occurs
  DATA_INTEGER(Fminage);
  DATA_INTEGER(Fmaxage); // Max age to fish
  DATA_INTEGER(useEffort); // use effort or estimate F
  DATA_INTEGER(useBlocks); // Use blocks for species selectivity?
  DATA_INTEGER(estimateCreep); // Bolean, estimate creep from effort data
  DATA_INTEGER(randomF); // Random walk on F
  DATA_INTEGER(randomR); // Random walk on R
  DATA_INTEGER(randomM); // Random walk on M
  DATA_INTEGER(M_nparms); // Number of M variances and estimates
  DATA_IVECTOR(CminageSeason); // Minimum age with fishing mortality per season
  DATA_IVECTOR(Qminage); // Minium ages in surveys
  DATA_IVECTOR(Qmaxage); // Maximum age in survey
  DATA_IVECTOR(Qlastage);
  DATA_IVECTOR(bidx); // Indexes for blocks of fishing mortality



  //DATA_INTEGER(endFseason); // Season in last year where fishing ends
  DATA_VECTOR(isFseason);
  DATA_VECTOR(surveyEnd);
  DATA_VECTOR(surveyStart);
  DATA_VECTOR(surveySeason);
  DATA_SCALAR(minSDsurvey);
  DATA_SCALAR(minSDcatch); // minium catch CV
  DATA_SCALAR(maxSDcatch); // Maximum catch CV
  DATA_SCALAR(peneps); // Tuning parameters for min catch and survey CV
  DATA_SCALAR(penepsC);
  DATA_SCALAR(penepsCmax); // Tuning parameter for maximum catch CV
  DATA_SCALAR(Mprior); // Prior to keep M within reasonable bounds
  DATA_ARRAY(powers);
  DATA_ARRAY(no); // Number of catch observations
  DATA_ARRAY(nocatch); // Matrix sized (year x season) determines wehter F>0
  DATA_ARRAY(scv); // time varying catch cv
  DATA_IVECTOR(Qidx);
  DATA_IARRAY(Qidx_CV); // survey catchability matrix
  DATA_IARRAY(Cidx_CV);
  DATA_IARRAY(Midx_CV); // Number of M variances and estimates
  //DATA_IARRAY(Midx); // Index for age grouping on assessing M2
//   // // // Time varying arrays
//   //DATA_ARRAY(F0);
  DATA_ARRAY(M); // Natural mortality
  DATA_ARRAY(propM); // Proportion of M before spawning
  DATA_ARRAY(propF); // Proportion of F before spawning
  DATA_ARRAY(Mat); // Maturity
  DATA_ARRAY(effort); // Effort input
// //
  DATA_IARRAY(catchCV); // Ages included in catch variation
//
// // // Recruitment
  DATA_INTEGER(recmodel); // Which recruitment model?
  //DATA_SCALAR(alpha); // Alpha parameter for recruitment
  //DATA_SCALAR(beta); // Beta parameter for recruitment
  //DATA_SCALAR(logSDrec); // Recruitment deviations
  DATA_VECTOR(nllfactor); // weight of likelihood functions
  DATA_IVECTOR(estCV); // Determine which SDs are calculated or estimated
   // Parameters
  PARAMETER_VECTOR(logRin); // Recruitment
  PARAMETER_VECTOR(logNinit); // Initial distribution
  PARAMETER_VECTOR(logFyear);
  PARAMETER_ARRAY(Fseason);
  PARAMETER_ARRAY(logFage);
  PARAMETER_VECTOR(SDsurvey); // Survey variation
  PARAMETER_VECTOR(SDcatch); // Catch SD's
  PARAMETER(creep); // Estimated technical creep
// //
  PARAMETER_VECTOR(logQ);
  PARAMETER(pin);
  PARAMETER(logalpha);
  PARAMETER(logbeta);
  PARAMETER(logSDrec);
  PARAMETER(logSDF); // Fishing mortality variability
  PARAMETER_VECTOR(logSDM); // M time varying
  PARAMETER_VECTOR(env);
  PARAMETER_ARRAY(ext_M); // External natural mortality
  PARAMETER_ARRAY(alphaM); // MICE input of external predators or other mortality inducing things
  PARAMETER(logR0);
  PARAMETER(logh);


//
array<Type>Catch(nage,nyears, nseason);
array<Type>CatchN(nage,nyears, nseason);
array<Type>Zsave(nage,nyears,nseason);
array<Type>Nsave(nage,nyears+1,nseason);
array<Type>survey(nage, nyears,nsurvey);
array<Type>Qsurv(nage, nsurvey);
array<Type>logQsurv(nage, nsurvey);
array<Type>SDS(nage, nsurvey);
array<Type>SDC(nage, nseason);
array<Type>F0(nage, nyears, nseason);
array<Type>log_exp_pattern(nage, nseason);
array<Type>Fquarter(nage,nyears,nseason);
array<Type>Fsel(nage,nyears,nseason);
array<Type>p(nage,nsurvey);
array<Type>Fagein(nage, nyears);
//array<Type>SDR_catch(nage, nyears);
array<Type>env_in(nenv,nyears);
array<Type>M_in(nalphaM, nyears);
array<Type>M_new(nage, nyears,nseason);

vector<Type>SSB(nyears+1);
vector<Type>TSB(nyears);
vector<Type>Fyear(nyears);
vector<Type>Rsave(nyears);
vector<Type>logRec(nyears);
vector<Type>term_logN_next(nage); // Numbers at age in the future year
vector<Type>env_tot(nyears);
vector<Type>M_tot(nyears); // Total predator contribution to M
vector<Type>Rin(nyears);
vector<Type>Catchtot(nyears);
// vector<Type>powerin(nsurvey);
vector<Type>Q(logQ.size());
vector<Type>SDM(logSDM.size());

//REPORT(Ravg)
Type alpha = exp(logalpha);
Type beta = exp(logbeta);
Type SDrec = exp(logSDrec);
Type SDF = exp(logSDF);
Type Rinit = exp(logR0);
Type h = exp(logh);

for(int i=0;i<logSDM.size();i++){
  SDM(i) = exp(logSDM(i));
}



Catch.setZero();
CatchN.setZero();
Catchtot.setZero();
Rsave.setZero();
SSB.setZero();
TSB.setZero();
Zsave.setZero();
Nsave.setZero();
survey.setZero();
SDC.setZero();
SDS.setZero();
Qsurv.setZero();
logQsurv.setZero();
Fsel.setZero();
log_exp_pattern.setZero();
Fyear.setZero();
env_tot.setZero();
M_tot.setZero();
env_in.setZero();
M_new.setZero();

Fyear(0) = 1;

// Retransform and set up for model

for(int time=0;time<nyears;time++){
    if(recmodel == 1){
      Rin(time)=exp(logRin(time));
    }
      if(time > 0){
        Fyear(time) = exp(logFyear(time-1));
      }
}



if(nenv >0){
// Retransform and set up for model
for(int time=0;time<nyears;time++){
  for(int k=0;k<nenv;k++){
  env_in(k,time) = env(k)*env_matrix(k,time);
    }
  }

  REPORT(env_in)

  for(int time=0;time<nyears;time++){
    for(int k=0;k<nenv;k++){
      env_tot(time) += env_in(k,time);
    }
  }
}




if(randomM == 1){

  if(nalphaM >0){
  // Retransform and set up for model
  for(int k=0;k<nalphaM;k++){
    for(int time=0;time<nyears;time++){
      for(int i=0;i<(nage);i++){ //
        M_in(i,time,k) = alphaM(k)*M_matrix(k,time);
      }
    }
  }
}

  for(int time=0;time<nyears;time++){
    for(int k=0;k<nalphaM;k++){
      M_tot(time) += M_in(k,time);
    }
  }
  ADREPORT(M_tot)

  for(int time=0;time<nyears;time++){
    for(int i=0;i<(nage);i++){ //
      if(Midx_CV(i) > -1){
        if(time == 0){
          M_new(i,time,0) = M(i,time,0)*exp(ext_M(time,Midx_CV(i))+M_tot(time));
        }else{
          M_new(i,time,0) = M_new(i, time-1,0)*exp(ext_M(time,Midx_CV(i))+M_tot(time));
        }
      }else{
        M_new(i,time,0) = M(i,time,0);
      }
    }
  }
}else{
  for(int time=0;time<nyears;time++){
    for(int i=0;i<(nage);i++){ //
      for(int s=0;s<(nseason);s++){
        M_new(i,time,s) = M(i,time,s);
        }
      }
    }
  }


REPORT(ext_M)

REPORT(M_new)
ADREPORT(M_new)

REPORT(env_tot)

// Calculate mean R



//Type Ravg = beta;

for(int time=1;time<nyears;time++){
  Fyear(time) = exp(logFyear(time-1));
}




for(int i=0;i<(logQ.size());i++){ //
  Q(i) = exp(logQ(i));
}

// Age contribution of fishing mortality

Fagein.setZero();
//
for(int i=0;i<(nage);i++){ //
  for(int j=0;j<(nyears);j++){

  if(i >= Fminage){

    if(i < Fmaxage){
      Fagein(i,j) = exp(logFage(i-Fminage, bidx(j)));
    }else{
      Fagein(i,j) = exp(logFage(Fmaxage-Fminage,bidx(j)));
      }
    }
   }
}

REPORT(Fagein)
//
// // Seasonal contribution of fishing mortality
Fquarter.setZero();
// //
if(useBlocks == 0){
  for(int j=0;j<(nyears);j++){
    for(int i=0;i<nage;i++){ // Loop over other ages (recruits excluded)
      for(int qrts=0;qrts<nseason;qrts++){ // Loop over other ages

        if(isFseason(qrts) == 1){
          if(i < CminageSeason(qrts)){
          Fquarter(i,j,qrts) = Type(0.0);

        }else{

          if(useEffort == 0 && nseason > 1){

            if(i >= CminageSeason(qrts)){ //  && i <= Fmaxage Is not used for some reason
            Fquarter(i,j,qrts) = Fseason(qrts,i-(CminageSeason(qrts)));
            }
            // if(i > Fmaxage){
            // Fquarter(i,j,qrts) = Fseason(qrts, Fmaxage);
            // }
        }else{
          Fquarter(i,j,qrts) = Fseason(qrts,bidx(j));
        }

        }
        }else{

          if(i < CminageSeason(qrts)){
          Fquarter(i,j,qrts) = 0;//Type(1);
          }else{
          if(i == 0){
            if(nseason == 2){
                Fquarter(i,j,qrts) = Type(1);
              }else{
                Fquarter(i,j,qrts) = Type(1)/nseason; // Morton's trick to fix a degree of freedom that isn't needed for the parameterization
            }
            }else{
            Fquarter(i,j,qrts) = Type(1)/nseason; // Where does this come from?
            }
          }
        }
      }
    }
  }
}else{
  for(int j=0;j<(nyears);j++){
    for(int i=0;i<nage;i++){ // Loop over other ages (recruits excluded)
      for(int qrts=0;qrts<nseason;qrts++){ // Loop over other ages

        if(isFseason(qrts) == 1){
          if(i < CminageSeason(qrts)){
          Fquarter(i,j,qrts) = Type(0.0);
          }else{
          Fquarter(i,j,qrts) = Fseason(qrts,bidx(j));
          }
        }else{

          if(i == 0){ // Not sure why this is happening.
            if(nseason == 2){
                Fquarter(i,j,qrts) = Type(1);
              }else{
                Fquarter(i,j,qrts) = Type(1)/nseason; // This is a hack - ask Morten
            }
          }else{
            Fquarter(i,j,qrts) = Type(1)/nseason; // Where does this come from?
          }
        }
      }
    }
  }
}
//
REPORT(Fquarter)
// REPORT(Fagein)
// REPORT(Fyear)
//
// // // // // // Annual contribution of fishing mortality
array<Type>effort_creep(nyears);
array<Type>effort_new(nyears, nseason);

effort_creep(0) = 1;

// Estimate creep
if(useEffort == 1){
  if(estimateCreep == 1){
      for(int time=0;time<(nyears);time++){ // Loop over years excluding last one
        for(int qrts=0;qrts<nseason;qrts++){ // Loop over other seasons

          if(time == 0){
            effort_new(0,qrts) = 1*effort(time,qrts);
          }else{
          effort_creep(time) = effort_creep(time-1)*exp(creep);
          effort_new(time,qrts) = effort(time, qrts)*effort_creep(time);
        }
      }
    }
  }else{

    if(useEffort == 1){
      for(int time=0;time<(nyears);time++){ // Loop over years excluding last one
        for(int qrts=0;qrts<nseason;qrts++){ // Loop over other seasons
          effort_new(time,qrts) = effort(time, qrts);
        }
      }
    }
  }
}




// F0.setZero();
// //
// //
if(useEffort == 1){
  for(int time=0;time<(nyears);time++){ // Loop over years excluding last one
    for(int i=0;i<nage;i++){ // Loop over other ages (recruits excluded)
      for(int qrts=0;qrts<nseason;qrts++){ // Loop over other seasons

          //  if(qrts != (nseason-1)){
              if(nocatch(time, qrts)>0){
              F0(i,time,qrts) = Fquarter(i,time,qrts)*Fagein(i,time)*effort_new(time, qrts);
              Fsel(i,time,qrts) = log(Fquarter(i,time,qrts)*Fagein(i,time));
              }


              if(time == (nyears-1)){
                  if(Fquarter(i, time, qrts) > 0 && Fagein(i,time)> 0){

                  log_exp_pattern(i,qrts) = log(Fquarter(i,time,qrts)*Fagein(i,time));
                }else{
                  log_exp_pattern(i, qrts) = -1000;
                }
              }

             Zsave(i,time,qrts) = F0(i,time,qrts)+M_new(i,time,qrts);
     }
    }
  }
}else{
  for(int time=0;time<(nyears);time++){ // Loop over years excluding last one
    for(int i=0;i<nage;i++){ // Loop over other ages (recruits excluded)
      for(int qrts=0;qrts<nseason;qrts++){ // Loop over other seasons

          if(nocatch(time, qrts)>0){
            F0(i,time,qrts) = Fquarter(i,time,qrts)*Fagein(i,time)*Fyear(time);
            Fsel(i,time,qrts) = log(Fquarter(i,time,qrts)*Fagein(i,time));
          }

          if(time == (nyears-1)){
                if(Fquarter(i, time, qrts) > 0 && Fagein(i,time)> 0){

                log_exp_pattern(i,qrts) = log(Fquarter(i,time,qrts)*Fagein(i,time));
              }else{
                log_exp_pattern(i, qrts) = -1000;
              }
            }
          Zsave(i,time,qrts) = F0(i,time,qrts)+M_new(i,time,qrts);

     }
    }
  }
}
// // //
// // //
// //
// //
for(int i=0;i<nage;i++){ // Loop over other ages //
  for(int k=0;k<(nsurvey);k++){
    //
     if(powers(i,k) == 1){
       p(i,k) = pin;
     }else{
       p(i,k) = Type(1.0);
     }
    }
  }
//
REPORT(p)
// //
// //
// // //
if(nsurvey>1){
  for(int i=0;i<nage;i++){ // Loop over other ages (recruits excluded)
    for(int k=0;k<(nsurvey);k++){ // Loop over surveys
        if(i >= Qminage(k) && i <= Qmaxage(k)){

            if(i < Qlastage(k)){
              Qsurv(i,k) = Q(Qidx(k)+i-Qminage(k));
              logQsurv(i,k) = log(Qsurv(i,k));
            }else{
              Qsurv(i,k) = Q(Qidx(k)+Qlastage(k)-Qminage(k));
              logQsurv(i,k) = log(Qsurv(i,k));
            }
        }
        if(i > Qmaxage(k)){
          Qsurv(i,k) = Type(0.0);
        }
      }

    }
}else{
    for(int i=0;i<nage;i++){
      if(i >= Qminage(0) && i <= Qmaxage(0)){

        if(i < Qlastage(0)){
            Qsurv(i,0) = Q(Qidx(0)+i-Qminage(0));
            logQsurv(i,0) = log(Qsurv(i,0));
          }else{
            Qsurv(i,0) = Q(Qidx(0)+Qlastage(0)-Qminage(0));
            logQsurv(i,0) = log(Qsurv(i,0));
        }
    }
    if(i > Qmaxage(0)){
      Qsurv(i,0) = Type(0.0);
    }
  }
}
// // // //
// // // // //
Type penSDsurvey;
penSDsurvey= 0;//penalty if SDsurvey is close to minSDsurvey
Type tmpdiff; //temporarily store SDsurvey-minSDsurvey
// // // // // //
if(nsurvey>1){
  for(int k=0;k<(nsurvey);k++){ // Loop over surveys
    for(int i=0;i<nage;i++){ // Loop over other ages
        if(i >= Qminage(k) && i <= Qmaxage(k)){
          tmpdiff = SDsurvey(Qidx_CV(i,k))-minSDsurvey;
          tmpdiff = posfun(tmpdiff, peneps, penSDsurvey);
          SDsurvey(Qidx_CV(i,k)) = tmpdiff+minSDsurvey;
          SDS(i,k) = SDsurvey(Qidx_CV(i,k));
        }
    }
  }
}else{
  for(int i=0;i<nage;i++){ // Loop over other ages
      if(i >= Qminage(0) && i <= Qmaxage(0)){
        tmpdiff = SDsurvey(Qidx_CV(i,0))-minSDsurvey;
        tmpdiff = posfun(tmpdiff, peneps, penSDsurvey);
        SDsurvey(Qidx_CV(i,0)) = tmpdiff+minSDsurvey;
        SDS(i,0) = SDsurvey(Qidx_CV(i,0));
      }
  }
}

// //
// // // // Set up at Nat age in first Quarter
Nsave(0,0,0) = 0; // Nothing here in the fist year

for(int i=1;i<(nage);i++){ // Get initial ages from age 1
  Nsave(i,0,0) = exp(logNinit(i-1));
}
//
vector<Type> Nzero(nage); // Numbers with no fishing
Type SSB0 = 0;

if(recmodel == 3){
Nzero(0) = Rinit*exp(-0.5*SDrec);
  for(int i=1;i<(nage-1);i++){
    Nzero(i) = Rinit * exp(-(M_new(i,0,0)*i)-0.5*SDrec);
    Nsave(i,0,0) = Nzero(i)*exp(logNinit(i-1));
  }
  Nzero(nage-1) = (Rinit*exp(-(M_new(nage-2,0,0)*(nage-1))-0.5*SDrec))/(Type(1.0)-exp(-M_new(nage-1,0,0)));
  Nsave(nage-1,0,0) = Nzero(nage-1)*exp(logNinit(nage-2));

  for(int i=0;i<nage;i++){ // Loop over ages
      SSB0 += Nzero(i)*west(i,0,0)*Mat(i,0,0)*exp(-(M_new(i,0,0)*propM(i,0,0)));
    }
    REPORT(Nzero)
    REPORT(SSB0)
}



// // // // //
for(int time=0;time<(nyears);time++){ // Start time loop
  for(int qrts=0; qrts<(nseason);qrts++){
      if(qrts == 0){ // Spawning stock biomass is from season 1
        for(int i=0;i<nage;i++){ // Loop over other ages
             SSB(time) += Nsave(i,time,0)*west(i,time,0)*Mat(i,time,0)*exp(-(M_new(i,time,qrts)*propM(i,time,qrts)+F0(i,time,qrts)*propF(i,time,qrts))); // Fix SSB
             TSB(time) += Nsave(i,time,0)*west(i,time,0); // TSB in the beginning of the season
          }
      }
      if(qrts == (recseason-1)){ // Recruitment season

         if(recmodel == 1){ // Different recruitment models
             Rsave(time) = Rin(time);
             Nsave(0,time,qrts) = Rsave(time);
             logRec(time) = log(Rin(time));
           }
          if(recmodel == 2){

           if(SSB(time) >= beta){
             if(time >= (nyears-1)){
              Rin(time)= alpha*exp(env_tot(time)-0.5*SDrec);
            }else{
              Rin(time)=alpha*exp(logRin(time)+env_tot(time)-0.5*SDrec);
            }
          }else{
            if(time >= (nyears-1)){
              Rin(time) = (alpha/beta)*SSB(time)*exp(env_tot(time)-0.5*SDrec);
            }else{
              Rin(time) = (alpha/beta)*SSB(time)*exp(logRin(time)+env_tot(time)-0.5*SDrec);
            }
          }
            Rsave(time) = Rin(time);
            Nsave(0,time,qrts) = Rsave(time);
            logRec(time) = log(Rin(time));
          }

          if(recmodel == 3){
            if(time >= (nyears-1)){
              Rin(time) = (4*h*Rinit*SSB(time)/(SSB0*(1-h)+ SSB(time)*(5*h-1)))*exp(-0.5*SDrec+env_tot(time));
          }else{
              Rin(time) = (4*h*Rinit*SSB(time)/(SSB0*(1-h)+ SSB(time)*(5*h-1)))*exp(-0.5*SDrec+logRin(time)+env_tot(time));

          }

            Rsave(time) = Rin(time);
            Nsave(0,time,qrts) = Rsave(time);
            logRec(time) = log(Rin(time));
          }

       }
        if(qrts < (nseason-1)){
         for(int i=0;i<(nage);i++){ // Loop over other ages
         Nsave(i,time,qrts+1) = Nsave(i,time,qrts)*exp(-Zsave(i,time,qrts));
        }

        for(int i=0;i<nage;i++){ // Loop over other ages (starts at age 1)

           if(F0(i, time, qrts)>0){
           Catch(i,time,qrts)= (F0(i,time,qrts)/(Zsave(i,time,qrts)))*(1-exp(-Zsave(i,time,qrts)))*Nsave(i,time,qrts)*weca(i,time,qrts);// Calculate the catch in kg
           CatchN(i,time,qrts)= (F0(i,time,qrts)/(Zsave(i,time,qrts)))*(1-exp(-Zsave(i,time,qrts)))*Nsave(i,time,qrts);// Calculate the catch in #s
         }

           for(int k=0;k<nsurvey;k++){

             if(qrts == (surveySeason(k)-1)){
              if(surveyEnd(k) == 0){
               survey(i,time,k) = Nsave(i,time,qrts);//*Qsurv(i,k);
             }else{
               Type Ntmp = Nsave(i,time,qrts)*(exp(-Zsave(i,time,qrts)*surveyStart(k)));
               survey(i,time,k) = Ntmp*(1-exp(-Zsave(i,time,qrts)*(surveyEnd(k)-surveyStart(k))))/(Zsave(i,time,qrts)*(surveyEnd(k)-surveyStart(k)));
             }
            }
           }
        }

      }else{ // Last quarter

        for(int i=0;i<(nage-1);i++){ // Loop over other ages
        Nsave(i+1,time+1,0) = Nsave(i,time,qrts)*exp(-Zsave(i,time,qrts));
       }
       // Plus group
       Nsave(nage-1,time+1,0) = Nsave(nage-2,time,qrts)*exp(-Zsave(nage-2,time,qrts))+Nsave(nage-1,time,qrts)*exp(-Zsave(nage-1,time,qrts));

       for(int i=0;i<nage;i++){ // Loop over other ages

            Catch(i,time,qrts) = (F0(i,time,qrts)/(Zsave(i,time,qrts)))*(1-exp(-Zsave(i,time,qrts)))*Nsave(i,time,qrts)*weca(i,time,qrts);// Calculate the catch in kg
            CatchN(i,time,qrts)= (F0(i,time,qrts)/(Zsave(i,time,qrts)))*(1-exp(-Zsave(i,time,qrts)))*Nsave(i,time,qrts);// Calculate the catch in #s


          // Calculate survey abundance
          for(int k=0;k<nsurvey;k++){

            if(qrts == (surveySeason(k)-1)){
             if(surveyEnd(k) == 0){
              survey(i,time,k) =  Nsave(i,time,qrts+1);//*Qsurv(i,k);
            }else{
              Type Ntmp = Nsave(i,time,qrts)*(exp(-Zsave(i,time,qrts)*surveyStart(k)));
              survey(i,time,k) = Ntmp*(1-exp(-Zsave(i,time,qrts)*(surveyEnd(k)-surveyStart(k))))/(Zsave(i,time,qrts)*(surveyEnd(k)-surveyStart(k)));//*Qsurv(i,k)*;
            }
           }
          }
         }
       }
 // //
 } // End Quarter loop
} // End time loop
// // //
// // //
// // // Calculate SSB and recruitment in the new year
// //
for(int i=0;i<nage;i++){ // Loop over other ages
     SSB(nyears) += Nsave(i,nyears,0)*west(i,nyears,0)*Mat(i,nyears,0)*exp(-(M_new(i,nyears-1,0)*propM(i,nyears,0)+F0(i,nyears-1,0)*propF(i,nyears,0))); //
     term_logN_next(i) = log(Nsave(i, nyears,0));
}
// // // // //
// // // // // Stock recruitment
vector<Type> SRpred(nyears);
vector<Type> xR(nyears);
vector<Type> xR2(nyears);

for(int time=0;time<(nyears);time++){ // Loop over years

// SRpred(time) = alpha + log(SSB(time));
     //   if(SSB(time) > beta){
     //      SRpred(time) = alpha+log(beta);
     // }
     //
     if(recmodel == 1){
       if(SSB(time)< beta){
           xR(time) = log(Rsave(time))-(log(alpha)+log(SSB(time)));
           xR2(time) = pow(log(Rsave(time))-(log(alpha)+log(SSB(time))),2);
           //SRpred(time) = exp(log(alpha)+log(SSB(time)));
           SRpred(time) = alpha*exp(env_tot(time)-0.5*SDrec);
         }else{
           xR(time)  = log(Rsave(time))-(log(alpha)+log(beta));
           xR2(time) = pow(log(Rsave(time))-(log(alpha)+log(beta)),2);
           SRpred(time) = alpha*exp(env_tot(time)-0.5*SDrec);
         }
        }

        if(recmodel == 3){
        SRpred(time) = (4*h*Rinit*SSB(time)/(SSB0*(1-h)+ SSB(time)*(5*h-1)))*exp(-0.5*SDrec+env_tot(time));
        }

        if(recmodel == 2){
          SRpred(time) = alpha;
        }

  }


// // // // // // //
// Rsave(nyears) = exp(SRpred(nyears)); Enable to predict R from SR
// logRec(nyears) = SRpred(nyears);
// //
// //
// // // Run catch residuals for preliminary calcs
array<Type> resid_catch(nage,nyears, nseason); // Save residuals for SDR calculation
// // For catch variability calculation
// //
for(int time=0;time<(nyears);time++){ // No catches in last year
  for(int i=0;i<nage;i++){ // Loop over other ages
    for(int qrts=0;qrts<nseason;qrts++){ // Loop over seasons

      if(Catchobs(i,time,qrts)> 0 && CatchN(i, time, qrts) > 0){ // Log likelihood
          resid_catch(i,time,qrts) = log(Catchobs(i,time,qrts))-log(CatchN(i,time,qrts));
          }else{
            resid_catch(i, time, qrts) = Type(-99.);
      }
    }
  }
}
//
// // Calculate catch CV internally
int ncatch = catchCV.rows();
int astart = 0;
int aend = 1;
// // //
//array<Type> no(ncatch,nseason);
array<Type> sumx(ncatch,nseason);
array<Type> sumx2(ncatch,nseason);


REPORT(ncatch)
REPORT(Rin)

if(estCV(1) == 2){
  for(int k=0;k<(ncatch);k++){ // Loop over number of catch CVs
  //
     for(int qrts=0;qrts<nseason;qrts++){ // Loop over other ages

      astart = catchCV(k,qrts);
      if(k == (ncatch-1)){
        aend = nage;
      }else{
        aend = catchCV(k+1, qrts);
      }

       for(int time=0;time<(nyears);time++){ // No catches in last year


         for(int i=astart;i<aend;i++){

            if(Catchobs(i,time,qrts)> 0 && CatchN(i,time,qrts) > 0){ // Log likelihood
          //      if(i >= CminageSeason(qrts)){
                sumx(k,qrts) += log(CatchN(i,time,qrts))-log(Catchobs(i,time,qrts)); //resid_catch(i,time,qrts)*resid_catch(i,time,qrts);
                sumx2(k,qrts) += pow(log(CatchN(i,time,qrts))-log(Catchobs(i,time,qrts)),2); //resid_catch(i,time,qrts)*resid_catch(i,time,qrts);
            //    }
            }
          }
        }
       }
     }
   }
   REPORT(sumx2)
  REPORT(sumx)
// //
// // // Now assign SDR to each age
array<Type>SD_catch2(nage, nseason);

SD_catch2.setZero();

if(estCV(1) == 2){ // Calculate the catch CV
  for(int k=0;k<(ncatch);k++){ // Loop over number of catch CVs
    for(int qrts=0;qrts<nseason;qrts++){ // Loop over other ages
      astart = catchCV(k,qrts);

      if(k == (ncatch-1)){
        aend = nage;
      }else{
        aend = catchCV(k+1, qrts);
      }

      for(int i=astart;i<aend;i++){
          if(no(k, qrts)>0){ // Only calculate if there are any observations
          //SD_catch2(i,qrts) = sqrt(sumx2(k,qrts)/no(k,qrts));
          SD_catch2(i, qrts) = sqrt((no(k,qrts)*sumx2(k,qrts)-pow(sumx(k,qrts),2))/pow(no(k,qrts),2));

          if(no(k,qrts) == 1){
            SD_catch2(i, qrts) = 0.01; // This is causing an error for sprat
          }

          }
        }
      }
    }
}
//
Type penSDcatch;
penSDcatch= 0;//penalty if SDsurvey is close to minSDsurvey
Type penSDcatchmax;
penSDcatchmax = 0;


if(estCV(1) == 0){ // Estimate
  // Fix CV of catches
   Type tmpdiffC; //temporarily store SDsurvey-minSDsurvey
  Type tmpdiffCmax; //temporarily store SDsurvey-minSDsurvey

  // // // // // // //
  if(nseason>1){
    for(int k=0;k<(nseason);k++){ // Loop over surveys
      for(int i=0;i<nage;i++){ // Loop over other ages
          if(i >= CminageSeason(k)){
            tmpdiffC = SDcatch(Cidx_CV(i,k))-minSDcatch;
            tmpdiffC = posfun(tmpdiffC, penepsC, penSDcatch);
            SDcatch(Cidx_CV(i,k)) = tmpdiffC+minSDcatch;

            //
            tmpdiffCmax = maxSDcatch-SDcatch(Cidx_CV(i,k));
            tmpdiffCmax = posfun(tmpdiffCmax, penepsCmax, penSDcatchmax);
            SDcatch(Cidx_CV(i,k)) = maxSDcatch-tmpdiffCmax;

          }
      }
    }
  }else{
    for(int i=0;i<nage;i++){ // Loop over other ages
        if(i >= CminageSeason(0)){
          tmpdiffC = SDcatch(Cidx_CV(i,0))-minSDcatch;
          tmpdiffC = posfun(tmpdiffC, penepsC, penSDcatch);
          SDcatch(Cidx_CV(i,0)) = tmpdiffC+minSDcatch;
        //  SDS(i,0) = SDsurvey(Qidx_CV(i,0));

        //
        tmpdiffCmax = maxSDcatch-SDcatch(Cidx_CV(i,0));
        tmpdiffCmax = posfun(tmpdiffCmax, penepsCmax, penSDcatchmax);
        SDcatch(Cidx_CV(i,0)) = maxSDcatch-tmpdiffCmax;

        }
    }
  }

  if(nseason>1){
    for(int qrts=0;qrts<(nseason);qrts++){ // Loop over surveys
      for(int i=0;i<nage;i++){ // Loop over other ages
          if(i >= CminageSeason(qrts)){
            SD_catch2(i,qrts) = pow(SDcatch(Cidx_CV(i,qrts)),1);
          }
        }

      }
  }else{
    for(int i=0;i<nage;i++){ // Loop over other ages
        if(i >= CminageSeason(0)){
            SD_catch2(i,0) = pow(SDcatch(Cidx_CV(i,0)),1);
          }
        }
      }

  // for(int k=0;k<(ncatch);k++){ // Loop over number of catch CVs
  //   for(int qrts=0;qrts<(nseason);qrts++){
  //
  //     astart = catchCV(k,qrts);
  //
  //     if(k == (ncatch-1)){
  //       aend = nage;
  //     }else{
  //       aend = catchCV(k+1, qrts);
  //     }
  //
  //     for(int i=astart;i<aend;i++){
  //         SDR_catch2(i,qrts) = SDcatch(k);
  //         }

      // if(i < catchCV(0,qrts)){
      // SDR_catch2(i,qrts) = Type(0.0);
      // }
      // if( (i >= catchCV(0,qrts)) && (i < catchCV(1,qrts)) ){
      //  SDR_catch2(i,qrts) = SDcatch(i,qrts);
      // }
      // if(i >= (catchCV(1,qrts))){
      // SDR_catch2(i,qrts) = SDcatch(catchCV(1,qrts),qrts);
      // }
  //   }
  // }
}







// //
REPORT(SD_catch2)
// Add survey residuals
array<Type> resid_survey(nage,nyears,nsurvey); // Save residuals for SDR calculation
Type sumsurv = 0;

for(int time=0;time<nyears;time++){ // Loop over other ages
  for(int i=0;i<nage;i++){ // Loop over other ages
      for(int k=0;k<nsurvey;k++){ // Loop over surveys

        if(Surveyobs(i, time,k) > 0){ // Non existent values have a -1 flag

          resid_survey(i,time,k) = log(Surveyobs(i,time,k))-(log(survey(i, time,k))*p(i,k)+log(Qsurv(i,k)));
          sumsurv += resid_survey(i,time,k);
        }else{
           resid_survey(i,time,k) = Type(-99.);
         }
      }
  }
}


Type nllC = 0.0; // log likelihood for Catch
//
 for(int time=0;time<(nyears);time++){ // No catches in last year
   for(int i=0;i<nage;i++){ // Loop over other ages
     for(int qrts=0;qrts<nseason;qrts++){ // Loop over seasons
       if(Catchobs(i,time,qrts)> 0 && CatchN(i, time, qrts) > 0){ // Log likelihood

       nllC += -dnorm(log(CatchN(i, time, qrts)),log(Catchobs(i, time, qrts)), SD_catch2(i,qrts), true);
       Catchtot(time) += Catch(i,time, qrts);

     }
    }
   }
 }


Type nllsurv = Type(0.0); // log likelihood for survey observations
array<Type> Surveyout(nage,nyears,nsurvey); // Save residuals for SDR calculation
array<Type> SDSout(nage,nyears,nsurvey);// Total SD
Surveyout.setZero();

for(int time=0;time<nyears;time++){ // Loop over other ages
  for(int i=0;i<nage;i++){ // Loop over other ages
      for(int k=0;k<nsurvey;k++){ // Loop over surveys


        if(Surveyobs(i, time,k) > 0){ // Non existent values have a -1 flag
        // Export survey numbers
        Surveyout(i,time,k) = log(survey(i, time,k))*p(i,k)+log(Qsurv(i,k));
        //SDSout(i,time,k) = sqrt(pow(SDS(i,k),2)+pow(scv(i,time,k),2));

        //nllsurv += -dnorm(pow(log(survey(i, time, qrts,k),1)),log(Surveyobs(i, time, qrts,k)), SDS(i,k), true);
        nllsurv += -dnorm(Surveyout(i,time,k),log(Surveyobs(i, time,k)), SDS(i,k), true);

        //nllsurv += -dnorm(log(survey(i, time,k)),log(Surveyobs(i, time,k)), SDS(i,k), true);

        }else{
         survey(i,time,k) = Type(-1.0);
       }
      }
    }
 }




// // // //
// // // // // Penalty function for recruitment errors
Type resid_x = 0;
Type resid_x2 = 0;
vector<Type> resid_x_export(nyears);
//
// // Calculate SDrec
//
for(int i=0;i<nyears;i++){ // Loop over years
  resid_x += log(Rsave(i))-log(SRpred(i));
  resid_x2 += pow(log(Rsave(i))- log(SRpred(i)),2);
  resid_x_export(i) = log(Rsave(i))-log(SRpred(i));
}
//

// //
Type SDrec2;

if(estCV(2) == 2){// Calculate the standard deviation of recruitment
  SDrec2 = (((nyears*resid_x2)-pow(resid_x,2))/pow(nyears,2))*SDrec;
  // =(no*sumx2-square(sumx))/square(no);

}else{
  SDrec2 = SDrec;
}
REPORT(SDrec2)
REPORT(resid_x)
REPORT(resid_x2)
// // //
//Type prec = Type(0.0);
Type pXr = Type(0.0);
// vector<Type>ptest(nyears);
// //
// // // //
// for(int time=0;time<(nyears);time++){ // Loop over other years
//       prec += -dnorm(SRpred(time),log(Rsave(time)), SDrec, true);
//   //    ptest(time) = -dnorm(SRpred(time),log(Rsave(time)), SDrec, true);
// }

//Model recruitment as in original sms
for(int i=0;i<nyears;i++){ // Loop over years
  pXr += xR2(i);
}

Type prec = 0.0;

  if(randomR == 1){

    if(recmodel > 1){
    for(int time=0;time<(nyears-1);time++){ // Loop over years
    prec += -dnorm(logRin(time), Type(0.0) , SDrec2, true);
    }
    }

      if(recmodel == 1){ // Assume random walk if randomR is one with recmodel 1
        for(int time=1;time<(nyears);time++){ // Loop over years
          prec += -dnorm(logRin(time), logRin(time-1) , SDrec2, true);
        }
    }

  }else{
    prec = nyears*log(sqrt(SDrec2))+pXr*0.5/SDrec2;   // likelihood
  }

  if(recmodel == 3){
    for(int i=0;i<(nage-1);i++){ // Loop over years
      prec+= -dnorm(logNinit(i), Type(0.0), SDrec2, true);
    }
  }

// // // // // // //

// Do some reporting in log space
vector<Type>logSSB(nyears+1);
vector<Type>logTSB(nyears);
vector<Type>logCatchtot(nyears);
array<Type>logF0(nage, nyears, nseason);
array<Type>logCatch(nage,nyears, nseason);
array<Type>logCatchN(nage,nyears, nseason);
array<Type>logN(nage,nyears, nseason);
array<Type>logBiomass(nage, nyears, nseason);
vector<Type>Favg(nyears);
vector<Type>logFavg(nyears);

//
logSSB.setZero();
logTSB.setZero();
logF0.setZero();
logN.setZero();
logCatch.setZero();
logCatchtot.setZero();
logCatchN.setZero();
logBiomass.setZero();
Favg.setZero();
//
for(int time=0;time<(nyears);time++){ // Loop over other years
  logSSB(time) = log(SSB(time));
  logTSB(time) = log(TSB(time));
  logCatchtot(time) = log(Catchtot(time));

}

logSSB(nyears) = log(SSB(nyears));
//
for(int time=0;time<nyears;time++){ // Loop over years
  for(int i=0;i<nage;i++){ // Loop over ages
      for(int k=0;k<nseason;k++){ // Loop over seasons
        if(F0(i, time,k) >0){
          logF0(i,time,k) = log(F0(i,time,k));
          logCatch(i, time, k) = log(Catch(i,time,k));
          logCatchN(i,time,k) = log(CatchN(i,time,k));
        }

        if(Nsave(i,time,k) > 0){
          logN(i,time,k) = log(Nsave(i,time,k));
          logBiomass(i,time,k) = log(Nsave(i,time,k)*west(i,time,k));
        }

        if((age(i)>=Fbarage(0)) && (age(i)<=Fbarage(1))){
          Favg(time) +=F0(i,time,k)/(Fbarage(1)-Fbarage(0)+1);
        }
      }
    }
    logFavg(time)=log(Favg(time));
  }


// add random F possibility
Type ansF = 0.0;

if(randomF == 1){

  for(int time=1;time<nyears;time++){ // Loop over years
    ansF += -dnorm(Fyear(time), Fyear(time-1), SDF, true);
    //ansR += -dnorm(Fyear(time), Fyear(time-1), SDR, true);
  }
}

// add random F possibility
Type ansM = 0.0;

if(randomM == 1){

  for(int time=0;time<(nyears);time++){ // Loop over years
    for(int i=0;i<M_nparms;i++){ // Loop over ages
  //  ansM += -dnorm(M_new(0,time,0), M_new(0,time-1,0), SDM, true);
    ansM += -dnorm(ext_M(time, i), Type(0.0), SDM(i), true);
  }
}

  for(int time=0;time<(nyears);time++){ // Loop over years
    ansM += -dnorm(M_new(Midx_CV(0),time,0), M(Midx_CV(0),time,0), Type(Mprior), true); // Penalty for deviations from initial year
      }



//  ansM += dnorm(SDM, Type(0.2), Type(0.01), true);
}

// Do a penalty for too low ansM

Type ans = 0.0;
//
ans = nllsurv*nllfactor(0)+nllC*nllfactor(1)+prec*nllfactor(2)+penSDsurvey+penSDcatch+penSDcatchmax+ansF+ansM;
// // //
vector<Type> ansvec(4);
ansvec(0) = nllsurv;
ansvec(1) = nllC;
ansvec(2) = prec;
ansvec(3) = ansM;
//

REPORT(ansvec)
REPORT(SRpred)
REPORT(xR2)
REPORT(SSB)
REPORT(F0)
REPORT(Catch)
REPORT(CatchN)
REPORT(Rsave)
REPORT(Nsave)
REPORT(Zsave)
REPORT(Fyear)
REPORT(M)
REPORT(no)
REPORT(Qsurv)
REPORT(term_logN_next)
REPORT(log_exp_pattern)
REPORT(Surveyout)
REPORT(resid_catch)
REPORT(resid_survey)
REPORT(SDS)
REPORT(SDSout)
REPORT(p)
REPORT(logFavg)
REPORT(effort_creep)
// REPORT(survey)
// REPORT(ans)
// REPORT(alpha)
// REPORT(Fage)
// REPORT(Fquarter)
// REPORT(Qsurv)
// REPORT(Catchtot)
// REPORT(Surveyobs)
// REPORT(nll)
// //
// //
ADREPORT(logSSB)
ADREPORT(logTSB)
ADREPORT(logF0)
ADREPORT(logCatch)
ADREPORT(logCatchN)
ADREPORT(logN)
ADREPORT(ansvec)
ADREPORT(SDrec)
ADREPORT(logCatchtot)
ADREPORT(logRec)
ADREPORT(logBiomass)
ADREPORT(Fsel)
// ADREPORT(F0)
// ADREPORT(Catch)
// ADREPORT(CatchN)
ADREPORT(logQsurv)
ADREPORT(Nsave)
ADREPORT(Surveyout)
ADREPORT(SDS)
ADREPORT(SDSout)
ADREPORT(SD_catch2)
ADREPORT(Rsave)
ADREPORT(resid_catch)
ADREPORT(resid_survey)
ADREPORT(xR)
ADREPORT(log_exp_pattern)
ADREPORT(term_logN_next)
ADREPORT(effort_creep)
ADREPORT(resid_x_export)
//ADREPORT(Catchtot)
ADREPORT(logbeta)
ADREPORT(SDrec2)
ADREPORT(SRpred)
ADREPORT(logFavg)
ADREPORT(SSB0)
ADREPORT(M_new)
// //
// Type ans = 0.0;


  return ans;
}
