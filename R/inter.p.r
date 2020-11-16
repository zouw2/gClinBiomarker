

#' Title likelihood ratio test of interaction effect
#'
#' @param ds input data set
#' @param tte a text string that returns a Survival Object using \code{Surv}
#' @param reducedText main effect model without interaction term
#' @param fullText full model with interaction term
#'
#' @return
#' @export
#'
#' @examples data(input); p1 <- inter.p(ds=subset(input, !is.na(KRAS.mutant)), tte='Surv(time = PFS, event=PFS.event)', reducedText = 'KRAS.mutant + Arm', fullText = 'KRAS.mutant * Arm')
#'
inter.p <- function(ds, tte='Surv(time = tte, event=event)', reducedText='BM + trt', fullText='BM * trt'){

        reduced <- eval(parse(text = paste("coxph(", tte, "~" , reducedText, " , data=ds)")))
        full <- eval(parse(text = paste("coxph(", tte, "~" , fullText, ", data=ds )")))

        L.full <- logLik(full)[1]; n.full <- attr(logLik(full), "df");
        L.reduced <- logLik(reduced)[1]; n.reduced <- attr(logLik(reduced), "df")
        stat <-

        pchisq((-L.reduced + L.full)*2, df=n.full-n.reduced,lower.tail=F)
}
