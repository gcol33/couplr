Gilles Colling
Department of Botany and Biodiversity Research
University of Vienna
Rennweg 14, 1030 Vienna, Austria
gilles.colling051@gmail.com

To the Editors of The R Journal

Dear Editors,

I am submitting "couplr: Optimal Matching with Verifiable Certificates and Column
Generation" for consideration as a contributed research article.

The paper describes an R package for the linear assignment problem and for the
matching workflows built on it. My reason for sending it to The R Journal rather
than to a software-note venue is that the interesting content here is software
design, which is what your readership reads the journal for, and there is enough
of it to fill an article rather than a note. The package sits between two groups
of R packages that do not currently meet: causal-inference matching packages,
which wrap one fixed solver behind a treated/control interface, and solver
packages, which typically expose one algorithm each with no preprocessing,
constraints or diagnostics.

Two limits are common to both groups, and they are what the article is about.
A solve returns a matching and a status, and the status records the state the
solver terminated in rather than a statement about the matching that a reader
can check; the dual variables that would support such a check are not part of
what the matching packages return. And the set of admissible pairs is materialised
before it is solved, so the largest problem that can be attempted is the largest
graph that fits in memory.

The article's five contributions follow from addressing those two. Every
matching design compiles into one internal flow model, so node potentials come
back from all of them rather than from the one-to-one case alone. Those
potentials make a solution checkable by a third party, and `verify_assignment()`
and `verify_flow()` report each optimality condition separately rather than
returning a verdict; the article gives the rectangular case in which a plausible
verifier that omits one half of complementary slackness accepts a solution whose
cost is above the optimum. On that certificate the package solves without
materialising the complete graph: each unit starts with its nearest admissible
partners, the omitted pairs are priced against the potentials the restricted
solve returns, and pairs enter until none prices in, at which point those
potentials bound how far the restricted solution can sit from the complete
problem's optimum. Warm starting the same
loop traces a matching path over a swept constraint. Nineteen assignment algorithms, written from
scratch in C++ with no external LP or MIP solver, are the engine room. For one
of them, the Gabow-Tarjan bit-scaling algorithm, we are not aware of a prior
publicly available open-source implementation, and the paper says so carefully
and without making it the contribution.

The article reports five measurements. A per-solver benchmark over the nineteen
algorithms. A balance comparison against MatchIt and optmatch on the LaLonde NSW
data, where all three agree to three decimal places and, scored on one common
objective, to within a thousandth of a distance unit. A scaling comparison in
which couplr reaches problem sizes at which both alternatives stop. The
edge-generation loop itself: matching 16,667 treated units to 33,333 controls,
it holds 0.29% of the pairs and comes back certified for the complete problem
within the bound the certificate reports, in a third of the time the same
problem takes traversing the complete graph lazily. On the
four sizes where the dense solve can also be run, the two return the same
pairing unit by unit. And a caliper sweep of twenty values solved as
one warm-started path costs about half what the twenty independent solves cost,
with every point returning the status, matched count and objective its
independent solve returns.

Every number in the paper comes from the scripts and data files included in the
submission, run from the released commit; the article's own code chunks run in
well under a minute, since the expensive benchmarks are shipped as CSV output
from the scripts that produced them.

The audience is R users doing observational-study matching, R users who want an
answer they can check rather than trust, and R users with a plain assignment
problem who currently reach for clue or lpSolve. couplr 1.7.0 is on CRAN, has
vignettes, a test suite in which every optimal solver is checked against
exhaustive enumeration and the implicit path is checked against the dense one,
and a public issue tracker.

I confirm that this work has not been published elsewhere and is not under
consideration at another journal. I have no competing interests to declare.

Thank you for considering the manuscript.

Yours sincerely,

Gilles Colling
ORCID: 0000-0003-3070-6066
