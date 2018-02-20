function acb_calc_func_wrap(res::acb_struct, x::acb, param::Ptr{Void}, order::Int, prec::Int)

    acbF = unsafe_pointer_to_objref(param)::Function
    FF = AcbField(prec)
    z = acbF(FF(x))

    _acb_set(res, acb_struct(z))
    return Cint(0)::Cint
end

acb_calc_func_wrap_c() = cfunction(acb_calc_func_wrap, Cint,
        (Ref{acb_struct}, Ref{acb}, Ptr{Void}, Int, Int))

const ARB_CALC_SUCCESS = UInt(0)
const ARB_CALC_NO_CONVERGENCE = UInt(2)

function integrate(F::Function, a::acb, b::acb;
    rel_goal::Int=prec(parent(a)),
    abs_tol::mag_struct=mag_set_ui_2exp_si(mag_struct(), 1, -prec(parent(a))),
    opts::acb_calc_integrate_opts=acb_calc_integrate_opts(),
    precision::Int=prec(parent(a)))

    res = AcbField(precision)()
    ptrF = pointer_from_objref(F)

    status = ccall((:acb_calc_integrate, :libarb), UInt,
        (Ref{acb},   #res
        Ptr{Void},   #func
        Ptr{Void},   #params
        Ref{acb},    #a
        Ref{acb},    #b
        Int,         #rel_goal
        Ref{mag_struct},    #abs_tol
        Ref{acb_calc_integrate_opts}, #opts
        Int),
        res, acb_calc_func_wrap_c(), ptrF, a, b, rel_goal, abs_tol, opts, precision)

    if status == ARB_CALC_SUCCESS
        nothing
    elseif status == ARB_CALC_NO_CONVERGENCE
        warn("Integration did not obtained convergence")
    end
    return res
end
