# alternative names for some functions from Base
# (this list contained stuff along the lines of `@alias is_equal isequal`, but everything has moved to AbstractAlgebra)


# alternative names for some functions from LinearAlgebra
# we don't use the `@alias` macro here because we provide custom
# docstrings for these aliases
const eigenvalues = eigvals


# predeclare some functions to allow defining aliases for some of our own functions
function eigenvalues_simple end
@alias eigvals_simple eigenvalues_simple # for consistency with eigvals/eigenvalues
