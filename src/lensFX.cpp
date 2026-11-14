#include <Rcpp.h>
using namespace Rcpp;

/* ============================================================
 Core inline base functions (physics kernel)
 ============================================================ */

// --- Groundwater level in lens
inline double get_GWS_core(double A, double B,
                           double Mv, double PorFc, double PorFr) {
  return (A + B - Mv * PorFc) / PorFr;
}

// --- Groundwater level in base aquifer
inline double get_GV_core(double B, double PorSat) {
  return B / PorSat;
}

// --- Seepage (kwel)
inline double get_Kwel_core(double H1, double GWS, double c0) {
  return (H1 - GWS) / c0;
}

// --- Total drainage
inline double get_Dr_core(double GWS,
                          double HSloot,
                          double Wdr,
                          double inf_fact) {
  double x = (GWS - HSloot) / Wdr;
  return (x > 0.0) ? x : inf_fact * x;
}

// --- Drainage from B
inline double get_DrB_core(double GV, double GWS, double Dr) {
  return (GV < GWS) ? (GV / GWS) * Dr : Dr;
}

// --- Drainage from A
inline double get_DrA_core(double GWS, double GV, double Dr) {
  double x = (GWS - GV) * Dr / GWS;
  return (x > 0.0) ? x : 0.0;
}

// --- Surface runoff / overflow
inline double get_OA_core(double GWS, double Gr, double Cklein) {
  return (GWS < Gr) ? 0.0 : (GWS - Gr) / Cklein;
}

/* ============================================================
 Exported Rcpp wrappers (R interface)
 ============================================================ */

// [[Rcpp::export]]
double get_GWS_cpp(double A, double B,
                   double Mv, double PorFc, double PorFr) {
  return get_GWS_core(A, B, Mv, PorFc, PorFr);
}

// [[Rcpp::export]]
double get_GV_cpp(double B, double PorSat) {
  return get_GV_core(B, PorSat);
}

// [[Rcpp::export]]
double get_Kwel_cpp(double H1, double GWS, double c0) {
  return get_Kwel_core(H1, GWS, c0);
}

// [[Rcpp::export]]
double get_Dr_cpp(double GWS,
                  double HSloot,
                  double Wdr,
                  double inf_fact) {
  return get_Dr_core(GWS, HSloot, Wdr, inf_fact);
}

// [[Rcpp::export]]
double get_DrB_cpp(double GV, double GWS, double Dr) {
  return get_DrB_core(GV, GWS, Dr);
}

// [[Rcpp::export]]
double get_DrA_cpp(double GWS, double GV, double Dr) {
  return get_DrA_core(GWS, GV, Dr);
}

// [[Rcpp::export]]
double get_OA_cpp(double GWS, double Gr, double Cklein) {
  return get_OA_core(GWS, Gr, Cklein);
}

/* ============================================================
 Main derivative function (ODE RHS)
 ============================================================ */

// [[Rcpp::export]]
List lens_derivs_cpp(double t, NumericVector y, List pars) {
  
  // --- State variables
  const double A = y[0];
  const double B = y[1];
  
  // --- Forcing (constant during time step)
  const double H1     = pars["H1"];
  const double HSloot = pars["HSloot"];
  const double NN     = pars["NN"];
  
  // --- Parameters
  const double PorSat = pars["PorSat"];
  const double PorFr  = pars["PorFr"];
  const double PorFc  = pars["PorFc"];
  const double Mv     = pars["Mv"];
  const double c0     = pars["c0"];
  const double Wdr    = pars["Wdr"];
  const double Cklein = pars["Cklein"];
  const double Gr     = pars["Gr"];
  const double inf_fact = pars["inf_fact"];
  
  // --- Derived states
  const double GWS  = get_GWS_core(A, B, Mv, PorFc, PorFr);
  const double GV   = get_GV_core(B, PorSat);
  const double Kwel = get_Kwel_core(H1, GWS, c0);
  const double Dr = get_Dr_core( GWS, HSloot, Wdr, inf_fact);
  const double DrB  = get_DrB_core(GV, GWS, Dr);
  const double DrA  = get_DrA_core(GWS, GV, Dr);
  const double OA   = get_OA_core(GWS, Gr, Cklein);
  
  // --- Flux handling (identical to R)
  double delt = NN - OA - DrA;
  double dA   = delt;
  
  if ((delt < 0.0) && (A <= 0.0)) {
    dA   = 0.0;
  } else {
    delt = 0.0;
  }
  
  const double dB = Kwel - DrB + delt;
  
  // --- Cumulative derivatives
  return List::create(
    _["dy"] = NumericVector::create(
      dA, dB,
      NN,
      OA,
      DrA,
      Kwel,
      DrB
    ),
    _["aux"] = List::create(
      _["GWS"]    = GWS,
      _["GV"]     = GV,
      _["Dr"]     = Dr,
      _["OA"]     = OA,
      _["Kwel"]   = Kwel,
      _["H1"]     = H1,
      _["HSloot"] = HSloot
    )
  );
}

/* ============================================================
 State correction (Delphi behaviour)
 ============================================================ */

// [[Rcpp::export]]
List correct_state_cpp(double A, double B,
                       List state_aux,
                       List pars) {
  
  double BA_cum  = state_aux["BA_cum"];
  double AB_cum  = state_aux["AB_cum"];
  double ErrCorr = state_aux["ErrCorr"];
  
  const double PorFr = pars["PorFr"];
  const double PorFc = pars["PorFc"];
  const double Mv    = pars["Mv"];
  
  // --- Initial groundwater level
  const double GWS = get_GWS_core(A, B, Mv, PorFc, PorFr);
  
  // --- 1) A >= Amin
  const double Amin = PorFc * (Mv - GWS);
  double Corr = A - Amin;
  
  if (Corr < 0.0) {
    B += Corr;
    A  = Amin;
    
    BA_cum += -Corr;
    if (BA_cum > Amin) BA_cum = Amin;
    
  } else if (Corr > 0.0) {
    
    if (Corr >= BA_cum) {
      B += BA_cum;
      A -= BA_cum;
      BA_cum = 0.0;
    } else {
      B += Corr;
      A -= Corr;
      BA_cum -= Corr;
    }
  }
  
  // --- 2) B >= 0
  Corr = B;
  
  if (Corr < 0.0) {
    A += Corr;
    B  = 0.0;
    AB_cum += -Corr;
    
  } else if (Corr > 0.0) {
    
    B -= AB_cum;
    ErrCorr += AB_cum;
    
    if (B < 0.0) {
      AB_cum  = -B;
      B       = 0.0;
      ErrCorr -= AB_cum;
    } else {
      AB_cum = 0.0;
    }
  }
  
  // --- Error condition (Delphi behaviour)
  if (get_GWS_core(A, B, Mv, PorFc, PorFr) < 0.0) {
    stop("Grondwaterstand onder basis watervoerend pakket. "
           "Model niet geschikt voor dit probleem (Delphi-gedrag).");
  }
  
  return List::create(
    _["A"]       = A,
    _["B"]       = B,
    _["BA_cum"]  = BA_cum,
    _["AB_cum"]  = AB_cum,
    _["ErrCorr"] = ErrCorr
  );
}

/* ============================================================
 Main simulation routine
 ============================================================ */

// [[Rcpp::export]]
DataFrame simulate_lens_cpp(
    double PorSat,
    double PorFr,
    double Mv,
    double Gr,
    double c0,
    double Wdr,
	double inf_fact,
    double Cklein,
    double I_GWS,
    double I_GV,
    double dt,
    int    nstep,
    int    nSub,
    NumericVector H1,
    NumericVector HSloot,
    NumericVector NN
) {
  if (H1.size()     != nstep) stop("H1 length must equal nstep");
  if (HSloot.size() != nstep) stop("HSloot length must equal nstep");
  if (NN.size()     != nstep) stop("NN length must equal nstep");
  
  const double PorFc = PorSat - PorFr;
  const int N = nstep + 1;
  
  NumericVector Tijd(N), A(N), B(N), GWS(N), GV(N);
  NumericVector NN_out(N), OA(N), DrA(N), DrB(N), Dr(N), Kwel(N);
  NumericVector NN_cum(N), OA_cum(N), DrA_cum(N), DrB_cum(N), Kwel_cum(N);
  NumericVector ErrTot(N);
  
  // --- Correct initial state
  double Bcur = I_GV * PorSat;
  double Acur = I_GWS * PorFr - Bcur + Mv * PorFc;
  
  double NNc = 0.0, OAc = 0.0, DrAc = 0.0, DrBc = 0.0, Kwelc = 0.0;
  double BA_cum = 0.0, AB_cum = 0.0, ErrCorr = 0.0;
  
  for (int i = 0; i <= nstep; ++i) {
    
    Tijd[i] = i * dt;
    A[i] = Acur;
    B[i] = Bcur;
    
    GWS[i] = get_GWS_core(Acur, Bcur, Mv, PorFc, PorFr);
    GV[i]  = get_GV_core(Bcur, PorSat);
    
    if (i < nstep) {
      NN_out[i] = NN[i];
      Kwel[i] = get_Kwel_core(H1[i], GWS[i], c0);
      Dr[i]   = get_Dr_core(GWS[i], HSloot[i], Wdr, inf_fact);
      DrB[i]  = get_DrB_core(GV[i], GWS[i], Dr[i]);
      DrA[i]  = get_DrA_core(GWS[i], GV[i], Dr[i]);
      OA[i]   = get_OA_core(GWS[i], Gr, Cklein);
    } else {
      NN_out[i] = NN_out[i - 1];
      Kwel[i]   = Kwel[i - 1];
      Dr[i]     = Dr[i - 1];
      DrB[i]    = DrB[i - 1];
      DrA[i]    = DrA[i - 1];
      OA[i]     = OA[i - 1];
    }
    
    NN_cum[i]   = NNc;
    OA_cum[i]   = OAc;
    DrA_cum[i]  = DrAc;
    DrB_cum[i]  = DrBc;
    Kwel_cum[i] = Kwelc;
    ErrTot[i]   = ErrCorr;
    
    if (i == nstep) break;
    
    List pars = List::create(
      _["PorSat"]  = PorSat,
      _["PorFr"]   = PorFr,
      _["PorFc"]   = PorFc,
      _["Mv"]      = Mv,
      _["Gr"]      = Gr,
      _["c0"]      = c0,
      _["Wdr"]     = Wdr,
	  _["inf_fact"] = inf_fact,
      _["Cklein"]  = Cklein,
      _["H1"]      = H1[i],
                       _["HSloot"]  = HSloot[i],
                                            _["NN"]      = NN[i]
    );
    
    const double h = dt / nSub;
    
    for (int s = 0; s < nSub; ++s) {
      
      NumericVector y = NumericVector::create(
        Acur, Bcur, NNc, OAc, DrAc, Kwelc, DrBc
      );
      
      NumericVector k1 = as<NumericVector>(lens_derivs_cpp(0.0, y, pars)["dy"]);
      NumericVector k2 = as<NumericVector>(lens_derivs_cpp(0.0, y + 0.5*h*k1, pars)["dy"]);
      NumericVector k3 = as<NumericVector>(lens_derivs_cpp(0.0, y + 0.5*h*k2, pars)["dy"]);
      NumericVector k4 = as<NumericVector>(lens_derivs_cpp(0.0, y + h*k3,     pars)["dy"]);
      
      NumericVector inc = (k1 + 2*k2 + 2*k3 + k4) * (h / 6.0);
      
      Acur  += inc[0];
      Bcur  += inc[1];
      NNc   += inc[2];
      OAc   += inc[3];
      DrAc  += inc[4];
      Kwelc += inc[5];
      DrBc  += inc[6];
    }
    
    List corr = correct_state_cpp(
      Acur, Bcur,
      List::create(
        _["BA_cum"]  = BA_cum,
        _["AB_cum"]  = AB_cum,
        _["ErrCorr"] = ErrCorr
      ),
      pars
    );
    
    Acur    = as<double>(corr["A"]);
    Bcur    = as<double>(corr["B"]);
    BA_cum  = as<double>(corr["BA_cum"]);
    AB_cum  = as<double>(corr["AB_cum"]);
    ErrCorr = as<double>(corr["ErrCorr"]);
    
    if (i % 100 == 0) checkUserInterrupt();
  }
  
  return DataFrame::create(
    _["Tijd"]     = Tijd,
    _["A"]        = A,
    _["B"]        = B,
    _["GWS"]      = GWS,
    _["GV"]       = GV,
    _["NN"]       = NN_out,
    _["OA"]       = OA,
    _["DrA"]      = DrA,
    _["Kwel"]     = Kwel,
    _["DrB"]      = DrB,
    _["Dr"]       = Dr,
    _["NN_cum"]   = NN_cum,
    _["OA_cum"]   = OA_cum,
    _["DrA_cum"]  = DrA_cum,
    _["DrB_cum"]  = DrB_cum,
    _["Kwel_cum"] = Kwel_cum,
    _["ErrTot"]   = ErrTot
  );
}
