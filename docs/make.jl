using Documenter, Nemo, AbstractAlgebra

DocMeta.setdocmeta!(Nemo, :DocTestSetup, Nemo.doctestsetup(); recursive = true)
DocMeta.setdocmeta!(AbstractAlgebra, :DocTestSetup, AbstractAlgebra.doctestsetup(); recursive = true)

const render_pdf = "--pdf" in ARGS
const run_doctests = !("--nodoctests" in ARGS)

makedocs(
         format = [
            Documenter.HTML(;
                size_threshold_warn = 204800,
                size_threshold = 409600,
            ),
            (render_pdf ? (Documenter.LaTeX(),) : ())...,
         ],
         sitename = "Nemo.jl",
         modules = [Nemo, AbstractAlgebra],
         clean = true,
         checkdocs = :none,
         doctest = run_doctests,
         pages    = [
                     "index.md",
                     "about.md",
                     "types.md",
                     "constructors.md",
                     "Rings" => [
                                 "integer.md",
                                 "polynomial.md",
                                 "mpolynomial.md",
                                 "series.md",
                                 "puiseux.md",
                                 "residue.md",
                                ],
                     "Fields" => [
                                  "fraction.md",
                                  "rational.md",
                                  "algebraic.md",
                                  "exact.md",
                                  "complex.md",
                                  "real.md",
                                  "arb.md",
                                  "acb.md",
                                  "gfp.md",
                                  "finitefield.md",
                                  "ff_embedding.md",
                                  "numberfield.md",
                                  "padic.md",
                                  "qadic.md",
                                 ],
                     "matrix.md",
                     "factor.md",
                     "misc.md",
                     "Developer" => [
                                     "developer/introduction.md",
                                     "developer/conventions.md",
                                     "developer/typesystem.md",
                                     "developer/parents.md",
                                     "developer/interfaces.md",
                                     "developer/topics.md",
                                    ]
                    ]
        )

deploydocs(
           repo   = "github.com/Nemocas/Nemo.jl.git",
           target = "build",
           push_preview = true
          )
