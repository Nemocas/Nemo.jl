using Documenter, Nemo

makedocs()

deploydocs(
   deps   = Deps.pip("mkdocs", "python-markdown-math"),
   repo = "github.com/Nemocas/Nemo.jl.git",
   julia = "0.5"
)

