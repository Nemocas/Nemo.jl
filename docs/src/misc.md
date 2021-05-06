# Miscellaneous

## Global variables and precompilation

Due to limitations of the precompilation of modules julia, global variables
referring to certain Nemo types require special attention when used inside
modules. As a simple example, the following code for a module called `A` will
not work as expected:

```julia
module A

using Nemo
Qx, x = QQ["x"]
f(n) = x^n
end
```

When running julia and loading the module via `using/import A`, calling
`f` will lead to segmentation faults. The preferred workaround is to
put the definitions of the global variables into the `__init__()` function
of the module as follows:

```julia
module A

using Nemo

function __init__()
  global (Qx, x) = QQ["x"]
end

f(n) = x^n
end
```

Alternatively, one can disable precompilation by adding `__precompile__(false)` inside `A`.
Note that this might have other unwanted side effects.

