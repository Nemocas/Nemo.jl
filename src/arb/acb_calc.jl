export integrate

function acb_calc_func_wrap(res::Ptr{ComplexElem}, x::Ptr{ComplexElem}, param::Ptr{Nothing}, order::Int, prec::Int)
    xx = unsafe_load(x)
    F = unsafe_pointer_to_objref(param)
    w = F(xx)
    ccall((:acb_set, libarb), Ptr{Nothing}, (Ptr{ComplexElem}, Ref{ComplexElem}), res, w)
    return zero(Cint)
end

acb_calc_func_wrap_c() = @cfunction(acb_calc_func_wrap, Cint,
        (Ptr{ComplexElem}, Ptr{ComplexElem}, Ptr{Nothing}, Int, Int))

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
   ccall((:mag_init, libarb), Nothing, (Ref{mag_struct},), ctol)

   if abs_tol === -1.0
      ccall((:mag_set_ui_2exp_si, libarb), Nothing, (Ref{mag_struct}, UInt, Int), ctol, 1, -precision(Balls))
   else
      t = BigFloat(abs_tol, RoundDown)
      expo = Ref{Clong}()
      d = ccall((:mpfr_get_d_2exp, :libmpfr), Float64,
                (Ref{Clong}, Ref{BigFloat}, Cint),
                expo, t,
                Base.convert(Base.MPFR.MPFRRoundingMode, RoundDown))
      ccall((:mag_set_d, libarb), Nothing, (Ref{mag_struct}, Float64), ctol, d)
      ccall((:mag_mul_2exp_si, libarb), Nothing,
            (Ref{mag_struct}, Ref{mag_struct}, Int), ctol, ctol, Int(expo[]))
   end

   res = C()

   status = ccall((:acb_calc_integrate, libarb), UInt,
                  (Ref{ComplexElem},                       #res
                   Ptr{Nothing},                      #func
                   Any,                            #params
                   Ref{ComplexElem},                       #a
                   Ref{ComplexElem},                       #b
                   Int,                            #rel_goal
                   Ref{mag_struct},                #abs_tol
                   Ref{acb_calc_integrate_opts},   #opts
                   Int),
      res, acb_calc_func_wrap_c(), F, lower, upper, cgoal, ctol, opts, precision(Balls))

   ccall((:mag_clear, libarb), Nothing, (Ref{mag_struct},), ctol)

   if status == ARB_CALC_SUCCESS
      nothing
   elseif status == ARB_CALC_NO_CONVERGENCE
      @warn("Integration did not converge")
   end
   return res
end
