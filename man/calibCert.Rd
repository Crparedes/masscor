% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibCert.R
\name{calibCert}
\alias{calibCert}
\title{Information of balance calibration certificate}
\usage{
calibCert(balanceID = "BalanceID", serial = NULL, certificate = NULL, d,
  d.units = "mg", indError, indError.units = c("g", "mg", "mg"),
  expanded = TRUE, k = 2, rep, rep.units = c("g", "mg"), eccen,
  eccen.units = "mg", classSTD = NULL, traceability = NULL,
  Temp = NULL, p = NULL, h = NULL, unitsENV = c("deg.C", "hPa", "\%"),
  institution = NULL, accreditation = NULL, date = NULL,
  add.info = NULL)
}
\arguments{
\item{balanceID}{character with balance identification. May include
balance model, brand or internal location.}

\item{serial}{serial number of the balance.}

\item{certificate}{character with the calibration certificate
number and date of issue.}

\item{d}{division scale of the balance.}

\item{d.units}{character with the units of the division scale of the
balance. Default value is \code{'mg'}. See Details for more options.}

\item{indError}{\code{data.frame} with the indication error test results in
three columns containing balance reading, indication error and associated
uncertainties, respectively, for at least two mass standards.}

\item{indError.units}{character of length three with the units for each column
in the data frame provided in \code{indError}. Default value is
\code{c('g', 'mg', 'mg')}.}

\item{expanded}{if \code{TRUE} (the default), uncertainties provided in
\code{indError} are assumed to be expanded uncertainties,
instead of standard uncertainties.}

\item{k}{coverage factor for the expanded uncertainties when
\code{expanded = TRUE}.}

\item{rep}{results of the repeatability test. If the test is performed
in only one point, then \code{rep} is a numeric vector of length two
with the balance load and standard deviation for the same object
measured under repeatability conditions. If the test is performed
at more than one point \code{rep} is a data frame with balance loadings
in the first column and standard deviations in the second.}

\item{rep.units}{character of length two with the units for balance loads
and standard deviations provided in \code{rep}. Default value is
\code{c('g', 'mg')}.}

\item{eccen}{numeric vector of length two with balance load and maximal
reading difference obtained during eccentricity test.}

\item{eccen.units}{character of length two with the units for balance loads
and maximal reading difference provided in \code{eccen}. Default value is
\code{c('g', 'mg')}.}

\item{classSTD}{character with the class of the mass standards used.}

\item{traceability}{character with information regarding the
traceability of the calibration.}

\item{Temp}{ambient temperature at the moment of the calibration.}

\item{p}{barometric pressure at the moment of the calibration.}

\item{h}{relative humidity at the moment of the calibration.}

\item{unitsENV}{character vector of length three with the units of
\code{p}, \code{Temp} and \code{h}.
Default is \code{c('deg.C', 'hPa', '\%')}. See \strong{unitsENV} below for more options.}

\item{institution}{character with the identification of the calibration laboratory.}

\item{accreditation}{character with the accreditation information of the
calibration laboratory.}

\item{date}{character with the date of the measurements.}

\item{add.info}{named list or vector with any additional details included in
the calibration certificate.}
}
\value{
Object of class \code{calibCert} with information of
the calibration certificate for a balance.
}
\description{
Creates an object of class \code{calibCert} that contains
the information of a balance calibration certificate.
The object can later be used to correct mass readings
and calculate mass uncertainties.
Mandatory arguments for this function are the balance division scale (\code{d}),
the results of the indication error test (\code{indError}),
the results of repeatability test (\code{rep}),
and the results of the eccentricity test (\code{eccen}).
}
\details{
The units of \code{d}, \code{indError}, \code{rep} and \code{eccen}
shall be provided to the arguments \code{d.units}, \code{indError.units},
\code{rep.units} and \code{eccen.units}, respectively. The units can be
any multiple or subdivision of the SI unit for mass, the kilogram.
The greek letter \eqn{\mu} used to represent a millionth part,
is replaced by the vocal \code{u}.
Remember that both \code{R} and the SI prefixes are case sensitive.
}
\section{unitsENV}{

Temperature units (\code{Temp}) can be either \code{'deg.C'}
(for Celsius degrees) or \code{'K'}.
Pressure units (\code{p}) can be any of \code{'mmHg'},
\code{'Pa'}, \code{'hPa'} or \code{'kPa'}.
Relative humidity (\code{h}) can be expressed as
fraction (\code{'frac'}) or as percentage (\code{'\%'}).
A typical arrangement for the parameter \code{unitsENV}
would be \code{c('deg.C', 'hPa', '\%')}.
}

\examples{
massSTD  <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220)  ## [g]
indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
uncert   <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) ## [mg]
d <- 0.1 ## [mg]

Balance.D1 <- calibCert(balanceID = 'MT XPE 204', serial = 'B403223982',
                        d = d, d.units = 'mg',
                        indError = data.frame(massSTD, indError, uncert),
                        indError.units = c('g', 'mg', 'mg'),
                        rep = data.frame(load = c(0.1, 100, 220),
                                         sd = c(0.00, 0.04, 0.03)),
                        rep.units = c('g', 'mg'),
                        eccen = c(100, 0.1), eccen.units = c('g', 'mg'),
                        Temp = c(17.4, 17.9), ## [deg.C]
                        p = c(750.4, 751.0), ## [hPa]
                        h = c(70.5, 71.4), ## [\%]
                        unitsENV = c('deg.C', 'hPa', '\%'),
                        institution = 'Instituto Nacional de Metrologia de Colombia',
                        date = '2021-03-18')
print(Balance.D1)

}
\seealso{
S3 methods \code{\link[=print.calibCert]{print.calibCert()}} and \code{\link[=plot.calibCert]{plot.calibCert()}} are available.
See \code{\link[=convertMassUnitsSI]{convertMassUnitsSI()}} for information about mass units.
}
