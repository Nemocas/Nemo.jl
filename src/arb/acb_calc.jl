function acb_calc_func_wrap(res::Ptr{ComplexFieldElem}, x::Ptr{ComplexFieldElem}, param::Ptr{Nothing}, order::Int, prec::Int)
  xx = unsafe_load(x)
  F = unsafe_pointer_to_objref(param)
  w = F(xx)
  _acb_set(res, w)
  return zero(Cint)
end

acb_calc_func_wrap_c() = @cfunction(acb_calc_func_wrap, Cint,
                                    (Ptr{ComplexFieldElem}, Ptr{ComplexFieldElem}, Ptr{Nothing}, Int, Int))

const ARB_CALC_SUCCESS = UInt(0)
const ARB_CALC_NO_CONVERGENCE = UInt(2)

function integrate(C::ComplexField, F, a, b;
    rel_tol = -1.0,
    abs_tol = -1.0,
    deg_limit::Int = 0,
    eval_limit::Int = 0,
    depth_limit::Int = 0,
    use_heap::Int = 0,
    verbose::Int = 0)

  opts = acb_calc_integrate_opts(deg_limit, eval_limit, depth_limit,
                                 Cint(use_heap), Cint(verbose))

  lower = C(a)
  upper = C(b)

  cgoal = 0

  if rel_tol === -1.0
    cgoal = precision(Balls)
  else
    cgoal = -Int(exponent(rel_tol))
    @assert big(2.0)^(-cgoal) <= rel_tol
  end

  ctol = mag_struct(0, 0)
  @ccall libflint.mag_init(ctol::Ref{mag_struct})::Nothing

  if abs_tol === -1.0
    @ccall libflint.mag_set_ui_2exp_si(ctol::Ref{mag_struct}, 1::UInt, (-precision(Balls))::Int)::Nothing
  else
    t = BigFloat(abs_tol, RoundDown)
    expo = Ref{Clong}()
    d = ccall((:mpfr_get_d_2exp, :libmpfr), Float64,
              (Ref{Clong}, Ref{BigFloat}, Cint),
              expo, t,
              Base.convert(Base.MPFR.MPFRRoundingMode, RoundDown))
    @ccall libflint.mag_set_d(ctol::Ref{mag_struct}, d::Float64)::Nothing
    @ccall libflint.mag_mul_2exp_si(ctol::Ref{mag_struct}, ctol::Ref{mag_struct}, Int(expo[])::Int)::Nothing
  end

  res = C()

  status = @ccall libflint.acb_calc_integrate(res::Ref{ComplexFieldElem}, acb_calc_func_wrap_c()::#res
                  Ptr{Nothing}, F::#func
                  Any, lower::#params
                  Ref{ComplexFieldElem}, upper::#a
                  Ref{ComplexFieldElem}, cgoal::#b
                  Int, ctol::#rel_goal
                  Ref{mag_struct}, opts::#abs_tol
                  Ref{acb_calc_integrate_opts}, precision(Balls)::#opts
                  Int)::UInt

  @ccall libflint.mag_clear(ctol::Ref{mag_struct})::Nothing

  if status == ARB_CALC_SUCCESS
    nothing
  elseif status == ARB_CALC_NO_CONVERGENCE
    @warn("Integration did not converge")
  end
  return res
end
