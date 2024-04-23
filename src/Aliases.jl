# alternative names for some functions from Base
# (this list contained stuff along the lines of `@alias is_equal isequal`, but everything has moved to AbstractAlgebra)


# TODO: next breaking release: remove the if guard around the block
if @__MODULE__() == Nemo

    # alternative names for some functions from LinearAlgebra
    # we don't use the `@alias` macro here because we provide custom
    # docstrings for these aliases
    const eigenvalues = eigvals
end

# predeclare some functions to allow defining aliases for some of our own functions
function eigenvalues_simple end
@alias eigvals_simple eigenvalues_simple # for consistency with eigvals/eigenvalues


# TODO: next breaking release: remove this block
# Oscar includes this file for historical reasons, but expects it to contain the Base.@deprecate_binding calls.
# Until this is changed and released there, we thus need to include the deprecations here.
@static if Symbol(@__MODULE__()) in [:Hecke, :Oscar]
    Nemo.@include_deprecated_bindings()
end

