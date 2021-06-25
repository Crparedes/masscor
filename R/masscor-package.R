#' \code{masscor}: Mass Measurement Corrections and Uncertainties Using Balance Calibration Data.
#'
#' The \code{R} package \code{masscor} provides functions, classes and methods
#' to support mass measurements using non automatic balances as described in (Euramet, 2015).
#' The new classes are objects that can store the calibration information for
#' balances and mass standards. Those objects can be used to convert balance readings to
#' both conventional mass and mass, and to perform routine balance verification
#' (by using the normalized error function).
#' Air buoyancy correction factors are calculated using local air density that
#' can be calculated using measured environmental conditions and applying one
#' of several models available in the package. The uncertainty of (corrected)
#' mass measurements can also be evaluated allowing to further asses the suitability
#' of a given mass measurement.
#'
#' @section \code{masscor} functions:
#' This package uses list objects of class \code{'calibCert'} to store information
#' of balance calibration certificates.
#' The functions use the information of this object to convert balance reading
#' indications to conventional mass and calculate mass
#' uncertainties.
#'
#' Several models for calculating air density are included and this
#' information can be used to calculate the
#' Magnitude of Air Buoyancy Correction (MABC).
#' Uncertainties calculations are made using Gauss Approximation according to the
#' Guide to the expression of Uncertainty in Measurement (GUM) implemented in
#' \code{R} by the package
#' \link[propagate]{propagate} (Spiess, 2018).
#'
#' @section Disclaimer:
#' The Instituto Nacional de Metrologia de Colombia (INM) has published
#' this package to ease some calculations involved in mass metrology
#' applications. We endeavor to update it on a regular basis, but cannot
#' guarantee the accuracy of all implemented methods. Any reference to commercial
#' measurement instruments does not imply any approval,
#' endorsement or recommendation by the INM.
#'
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @references
#' EURAMET, Calibration Guide No. 18. 2015. Guidelines on the Calibration of
#' Non-Automatic Weighing Instruments.
#' https://www.euramet.org/Media/docs/Publications/calguides/I-CAL-GUI-018_Calibration_Guide_No._18_web.pdf.
#'
#' Picard, A; Davis, R S; Gläser, M; Fujii, K  (2008).  Revised formula for
#' the density of moist air (CIPM-2007).
#' Metrologia, 45(2), 149–155. doi:10.1088/0026-1394/45/2/004
#'
#' Harris, G. (2019). Selected Laboratory and Measurement Practices and Procedures to Support Basic
#' Mass Calibrations. SOP 2 - Recommended Standard Operating Procedure for Applying Air Buoyancy
#' Corrections. National Institute of Standards and Technology (NIST). doi:10.6028/NIST.IR.6969-2019
#'
#' BIMP JCGM (2008) Evaluation of measurement data — Guide to the
#' expression of uncertainty in measurement.
#'
#' Andrej-Nikolai Spiess (2018). propagate: Propagation of Uncertainty.
#' R package version 1.0-6.
#' https://CRAN.R-project.org/package=propagate
#'
#' @docType package
#' @name masscor-package
NULL
#globalVariables(c('', ''))
