# Round 2 review

Reviewed artefact: `rjournal(4).pdf`, 20 pages, the build at commit 6b403f4.
Returned in two messages: a correctness verdict, then a structural assessment
that revised the verdict.

---

## Message 1: minor revision, then submit

Scientifically strong and substantially cleaner than version 3.

Fixed since version 3:

- All troublesome floats on pages 13-18 correctly ordered; no sentences or
  limitation bullets interrupted.
- "Every design" correctly limited to "every flow-representable design".
- The metric-tree discussion distinguishes whitened distance summaries from
  original-coordinate caliper boxes.
- The memory guard states its 4x/10x multipliers and the measurements behind them.
- The 0.98x dispatcher result is explained rather than left puzzling.
- "Roughly half the cost" accurately summarises the warm-start results.
- Abstract at 236 words, within the 250-word limit.
- The nineteen-algorithm portfolio remains a headline contribution.

Three items remained.

**1. Blocking: the software versions are inconsistent.**
Page 2 says results produced with 1.6.2; page 12 says benchmarks used 1.6.2 at
commit `a63b760`; the page 19 bibliography says 1.7.0; CRAN lists 1.6.1.
Preferred resolution: release 1.7.0, then state explicitly which version is
described and which produced the benchmarks. If meaningful solver changes
occurred between those commits, rerun the benchmarks under 1.7.0.

**2. The abstract slightly outruns the evidence shown.**
It says the 16,667 + 33,333 implicit run "returns the dense solve's assignment",
but page 16 establishes unit-by-unit identity only at the four smaller sizes;
the two largest dense values come from another table. Either add the explicit
50,000-unit pairing comparison or use the cleaner claim: "retains 0.29 percent
of the pairs and returns a certified optimum." Certification is the stronger
and more relevant result anyway.

**3. Two production-level details.**
Several unembedded Helvetica font objects, apparently from the figures; embed
them or regenerate through a Cairo PDF device. Ensure figures and tables have
source-level alt text.

Optional precision edit: replace "the pruning is itself the certificate" with a
statement that each prune certifies that its subtree contains no violating edge,
and that the prunes together with the examined leaves certify the pricing result.

---

## Message 2: one structural editing pass needed first

Recognisably an R Journal paper and stronger than average technically, but too
fragmented and insufficiently hierarchical. Reorganise before submission; do not
rewrite the science.

Comparison with recent R Journal package papers (pencal 20pp, causaloptim 16pp,
ASML 21pp, theft 26pp, OTrecod 28pp): the typical pattern is not shorter but
uses fewer main sections and deeper subsections. couplr had 11 main sections in
20 pages, several only a page long.

What already fits the journal well: the opening (states the problem, maps the
ecosystem, identifies the missing capability, ends with a contribution list);
the theory-to-code balance; the sustained hospital-staff example.

Where the structure loses coherence:

1. Too many peer-level sections. Seven sections presented as equals fall into
   three natural groups: user-facing workflow, optimisation architecture,
   solver/software implementation.
2. The solver story is split across four locations (sections 2, 5, 8 and 10),
   forcing the reader to reopen the same thread. Section 2 should visibly be the
   core assignment engine.
3. "Software design" is a catch-all holding three unrelated topics: dispatch
   belongs with the solver portfolio, memory modes with the edge representations,
   verification with certificates or the evaluation methodology.
4. Section 10 is not really "Performance": it holds eight topics, and balance and
   feature coverage are not performance. It gives the generic solver benchmark
   priority over the paper's headline innovation, so the 0.29% edge result is not
   met until page 16. Rename it "Empirical evaluation" and order it by the
   paper's contributions.
5. The evidence sits too far from the mechanisms. Each methods section should end
   with a forward reference to where it is evaluated.
6. The discussion does not summarise the empirical result. Add a compact
   synthesis before the limitations.

Recommended outline: Introduction (plus a two-sentence roadmap); The core
assignment engine; Matching with couplr; Common optimization architecture;
Implementation and verification; Empirical evaluation; Summary and discussion.

Overall: scientific depth unusually strong, evidence substantially stronger than
average, introduction excellent, limitations excellent; hierarchy and navigation
weaker than the journal's better package papers. Main risk: it reads as several
excellent technical notes concatenated rather than one layered package article.
No scientific material needs removing.
