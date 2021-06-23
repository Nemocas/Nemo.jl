# Directions for overriding the C libraries (flint)

In `.julia/artifacts` you should have or make a file `Overrides.toml` with the following
entry (replace `/usr/local` with the location of your installation)

```
[e134572f-a0d5-539d-bddf-3cad8db41a82]
FLINT = "/usr/local"
```

The UUID is that of `FLINT_jll`, which is available in Nemo's `Project.toml`.
Note the case sensitivity of the artifact name: if the jll is called `XxX_jll`, the entry should be `XxX = `.
This is all that is needed to override the flint location. If it has worked:

```
julia> using Nemo
julia> Nemo.libflint
"/usr/local/lib/libflint.so"
```

Troubleshooting guide:

- If setting up the override for the first time, Nemo should be re-precompiled. This can be triggered by touching a source file.
- Serveral of the other libraries depending on flint might want a specific so version. This can be tweaked by hacking the flint makefile.

## arb

This should set `Nemo.libarb`.

```
[d9960996-1013-53c9-9ba4-74a4155039c3]
Arb = "/usr/local"
```

## antic

## calcium
