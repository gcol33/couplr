Gilles Colling
Department of Botany and Biodiversity Research
University of Vienna
Rennweg 14, 1030 Vienna, Austria
gilles.colling051@gmail.com

To the Editors of The R Journal

Dear Editors,

I am submitting "couplr: Optimal Pairing and Matching via Linear Assignment"
for consideration as a contributed research article.

The paper describes an R package for the linear assignment problem and for the
matching workflows built on it. My reason for sending it to The R Journal
rather than to a software-note venue is that the interesting content here is
software design, which is what your readership reads the journal for, and there
is enough of it to fill an article rather than a note. The package sits between
two groups of R packages that do not currently meet: causal-inference matching
packages, which wrap one fixed solver behind a treated/control interface, and
solver packages, which expose one algorithm each with no preprocessing,
constraints or diagnostics. Building something that serves both forced a set of
design decisions that I think are worth writing up: an S3 object model with a
shared parent class so that diagnostics and conversions are written once across
five matching designs, C++ solvers templated on their cost source so that the
dense and the on-demand cost representations run the same solver body and cannot
drift apart, a dispatcher whose rules follow from a per-solver benchmark, and a
memory model that reads available system RAM per platform and switches
representation before an oversized allocation.

The package implements nineteen assignment algorithms from scratch in C++, with
no external LP or MIP solver. One of them, the Gabow-Tarjan bit-scaling
algorithm, appears not to have had a publicly available open-source
implementation in any language before this; the paper says so carefully and
without making it the contribution.

The article reports a per-solver benchmark, a balance comparison against MatchIt
and optmatch on the LaLonde NSW data where all three agree to three decimal
places, and a scaling comparison in which couplr reaches problem sizes at which
both alternatives stop. Every number in the paper is reproducible from the
scripts and data files included in the submission; the article's own code chunks
run in well under a minute, since the expensive benchmarks are shipped as CSV
output from the scripts that produced them.

The audience is R users doing observational-study matching, and R users with a
plain assignment problem who currently reach for clue or lpSolve. couplr is on
CRAN, has vignettes, a test suite in which every optimal solver is checked
against exhaustive enumeration, and a public issue tracker.

I confirm that this work has not been published elsewhere and is not under
consideration at another journal. I have no competing interests to declare.

Thank you for considering the manuscript.

Yours sincerely,

Gilles Colling
ORCID: 0000-0003-3070-6066
