% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_modify.R
\name{factor_modify}
\alias{factor_modify}
\title{Create or modify factor in the modeling dataset}
\usage{
factor_modify(setup, ...)
}
\arguments{
\item{setup}{Setup object. Created at the start of the workflow. Usually piped in from previous step.}

\item{...}{Expression. One or more named expressions (similar to dplyr::mutate).}
}
\value{
Setup object with updated attributes.
}
\description{
Modifies one or more factors in the modeling dataset. Predictors in the model formula can also be modified directly.
}
\examples{
require(dplyr) # for the pipe operator
data('sev_train')

setup <- setup(
  data_train = sev_train,
  target = 'sev',
  weight = 'numclaims',
  family = 'gamma',
  keep_cols = c('pol_nbr', 'exposure', 'premium')
)

modeling <- setup \%>\%
  factor_add(pol_yr) \%>\%
  factor_add(agecat) \%>\%
  model_fit() \%>\%
  model_save('model1') \%>\%
  factor_modify(agecat = variate(agecat, type = 'non_prop', mapping = c(1, 2, 3, 4, 5, 6))) \%>\%
  model_fit()

}
\seealso{
\code{\link{custom_factor}}, \code{\link{variate}}, \code{\link{as_simple_factor}},
\code{\link{offset}}
}
