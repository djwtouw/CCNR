// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>
#include "ccncpp/include/cc_network.h"


// [[Rcpp::export(.ccn_fit)]]
Rcpp::List ccn_fit(const Eigen::MatrixXd& X, const Eigen::MatrixXd& Y,
                   const Eigen::VectorXd& params_vec, double q, double alpha,
                   double c1, double c2, double tol, std::string loss_type,
                   double heaviside_k, double heaviside_t) {
    CCNResult result = ccn_logistic(
        X, Y, params_vec, q, alpha, c1, c2, tol, loss_type, heaviside_k,
        heaviside_t
    );

    Rcpp::List R_result;
    R_result["coefficients"] = result.params;
    R_result["loss"] = result.loss;

    return R_result;
}

// [[Rcpp::export(.ccn_predict)]]
Eigen::MatrixXd ccn_predict(const Eigen::MatrixXd& X,
                       const Eigen::VectorXd& params_vec, int L) {
    Eigen::MatrixXd result = ccn_prediction(X, params_vec, L);

    return result;
}
