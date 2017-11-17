###############################################################################
#
#   Error objects
#
###############################################################################

mutable struct ErrorConstrDimMismatch <: Exception
  expect_r::Int
  expect_c::Int
  get_r::Int
  get_c::Int
  get_l::Int

  function ErrorConstrDimMismatch(er::Int, ec::Int, gr::Int, gc::Int)
    e = new(er, ec, gr, gc, -1)
    return e
  end

  function ErrorConstrDimMismatch(er::Int, ec::Int, gl::Int)
    e = new(er, ec, -1, -1, gl)
    return e
  end

  function ErrorConstrDimMismatch(er::Int, ec::Int, a::Array{T, 2}) where {T}
    gr, gc = size(a)
    return ErrorConstrDimMismatch(er, ec, gr, gc)
  end

  function ErrorConstrDimMismatch(er::Int, ec::Int, a::Array{T, 1}) where {T}
    gl = length(a)
    return ErrorConstrDimMismatch(er, ec, gl)
  end
end

function Base.showerror(io::IO, e::ErrorConstrDimMismatch)
  if e.get_l == -1
    print(io, "Expected dimension $(e.expect_r) x $(e.expect_c), ")
    print(io, "got $(e.get_r) x $(e.get_c)")
  else
    print(io, "Expected an array of length $(e.expect_r * e.expect_c), ")
    print(io, "got $(e.get_l)")
  end
end

const error_dim_negative = ErrorException("Dimensions must be non-negative")

