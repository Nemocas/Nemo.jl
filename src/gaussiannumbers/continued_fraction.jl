export continued_fraction, continued_fraction_with_matrix, convergents

macro new_struct(T, args...)
   return esc(Expr(:new, T, args...))
end

mutable struct _fmpq_ball
   left_num::Int
   left_den::Int
   right_num::Int
   right_den::Int
   exact::Cint
end

mutable struct _fmpq_cfrac_list
   array::Ptr{Int}
   length::Int
   alloc::Int
   limit::Int
   alt_sum::Int
   want_alt_sum::Cint
end

mutable struct _fmpz_mat22
   _11::Int
   _12::Int
   _21::Int
   _22::Int
   det::Cint
end

function steal_fmpz_data(data::Int)
   z = @new_struct(fmpz, data)
   finalizer(Nemo._fmpz_clear_fn, z)
   return z
end

function steal_data!(z::fmpz)
   r = z.d
   z.d = 0
   return r
end

function copy_data(a::fmpz)
   z = Ref(0)
   ccall((:fmpz_init_set, libflint), Nothing,
         (Ref{Int}, Ref{fmpz}),
         z, a)
   return z[]
end

function shift_right!(z::fmpz, a::fmpz, b::Union{Int, UInt})
   ccall((:fmpz_fdiv_q_2exp, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, UInt),
         z, a, UInt(b))
   return z
end

# l_infinity shortest vector in the ZZ-rowspace of [c b; 0 a]
# is u = y*(c,b) - x*(0,a) for some t = (y,-x) in ZZ^2
# (x/y) is either the last convergent of b/a that lies outside [(b-c)/a, (b+c)/a]
#              or the first convergent that lies inside
#
# Simple fact: If I is a finite interval containing an integer with I > 1,
#     then for any z in I, either the first or second convergent of z is in I.
function _shortest_l_infinity(c::fmpz, b::fmpz, a::fmpz)
   @assert c > 0 && a > b && b >= 0
   t1 = fmpz(0)
   t2 = fmpz(1)
   t3 = fmpz(-1)
   b_plus_c = b + c
   b_minus_c = b - c
   if a <= c
      return (fmpz(0), a), (t1, t2)
   elseif b_minus_c <= 0
      return (c, b), (t2, t1)
   elseif a <= b_plus_c
      return (c, b - a), (t2, t3)
   end
   # s is fake and shallow
   s = _fmpq_cfrac_list(C_NULL, -1, 0, typemax(Int), 0, 0)
   # will cleanup m while stealing its entries later
   m = _fmpz_mat22(1,0,0,1,1)
   # will cleanup x while stealing its enties later
   x = _fmpq_ball(copy_data(a), steal_data!(b_plus_c),
                  copy_data(a), steal_data!(b_minus_c), 0)
   ccall((:_fmpq_ball_get_cfrac, libflint), Nothing,
         (Ref{_fmpq_cfrac_list}, Ref{_fmpz_mat22}, Cint, Ref{_fmpq_ball}),
         s, m, 1, x)

   m11 = steal_fmpz_data(m._11)
   m12 = steal_fmpz_data(m._12)
   m21 = steal_fmpz_data(m._21)
   m22 = steal_fmpz_data(m._22)
   xld = steal_fmpz_data(x.left_den)
   xrd = steal_fmpz_data(x.right_den)
   xln = steal_fmpz_data(x.left_num)
   xrn = steal_fmpz_data(x.right_num)

   # all arithmetic is inplace now
   v12 = add!(xld, xld, xrd); shift_right!(v12, v12, 1)
   v22 = add!(xln, xln, xrn); shift_right!(v22, v22, 1)
   if m.det < 0
      neg!(v12, v12)
   else
      neg!(v22, v22)
   end
   v11 = mul!(xrd, m11, c)
   v21 = mul!(xrn, m12, c)

   # v is supposed to be m 'applied' to [c b; 0 a]
   @assert v11 == m11*c
   @assert v12 == m11*b - m21*a
   @assert v21 == m12*c
   @assert v22 == m12*b - m22*a

   vcmp = cmpabs(v11, v12)
   # get_cfrac ensures I = M^-1([a/(b+c) a/(b-c)]) satisfies the simple fact.
   # We have |m11*c| >= |m11*b - m21*a| iff a/(b-c) <= m11/21 <= a/(b+c).
   @assert vcmp < 0  # since infty = M^-1(m11/m21) is outside of I.
   # u is best, t is transformation to best
   u1 = b_plus_c; u2 = b_minus_c # reuse temp objs
   set!(u1, v11); set!(u2, v12)
   set!(t1, m11); neg!(t2, m21)

   # The simple fact is satisfied with z = M^-1(a/b): generate at most two more
   # convergents q1 and q2. |v12| is decreasing and |v11| is increasing. As soon
   # as |v11| >= |v12|, we are done since then a/(b+c) <= m11/21 <= a/(b-c),
   # which is the same thing as q1 or q1+1/q2 in I.
   triesleft = 2
   while (triesleft -= 1) >= 0 && vcmp < 0
      Q = tdiv(v22, v12)
      @assert cmp(Q, 0) < 0
      @assert cmpabs(u1, u2) < 0
      submul!(m12, m11, Q); swap!(m12, m11)
      submul!(m22, m21, Q); swap!(m22, m21)
      submul!(v21, v11, Q); swap!(v21, v11)
      submul!(v22, v12, Q); swap!(v22, v12)
      vcmp = cmpabs(v11, v12)
      if cmpabs(vcmp < 0 ? v12 : v11, u2) < 0
         if triesleft > 0 && vcmp < 0
            set!(u1, v11); set!(u2, v12)
            set!(t1, m11); neg!(t2, m21)
         else
            u1 = v11; u2 = v12
            t1 = m11; t2 = neg!(m21, m21)
         end
      end
   end
   return (u1, u2), (t1, t2)
end

# return shortest vector in the ZZ rowspace along with the ZZ's in that linear combo
function shortest_l_infinity_with_transform(m::fmpz_mat)
   ncols(m) == 2 || error("not implemented in $(ncols(m)) dimensions")
   r = nrows(m)
   if r < 1
      return fmpz[fmpz(0), fmpz(0)], fmpz[]
   elseif r == 1
      return fmpz[m[1,1], m[1,2]], fmpz[fmpz(1)]
   end
   M, U = hnf_with_transform(m)
   c = M[1,1]
   b = M[1,2]
   a = M[2,2]
   if iszero(a)
      return fmpz[c, b], fmpz[U[1,i] for i in 1:r]
   else
      (v1, v2), (t1, t2) = _shortest_l_infinity(c, b, a)
      return fmpz[v1, v2], fmpz[t1*U[1,i] + t2*U[2,i] for i in 1:r]
   end
end

###############################################################################
#
# continued_fraction
#
###############################################################################

function _push_and_clear!(v::Vector{fmpz}, s::_fmpq_cfrac_list)
   for i in 1:s.length
      push!(v, steal_fmpz_data(unsafe_load(s.array, i)))
   end
   for i in s.length:s.alloc-1
      ccall((:fmpz_clear, libflint), Nothing, (Ptr{Int},), s.array + sizeof(Int)*i)
   end
   ccall((:flint_free, libflint), Nothing, (Ptr{Int},), s.array)
end

function _doit_exact!(xn::fmpz, xd::fmpz, v::Vector{fmpz}, lim::Int, wantM::Bool)
   ok = xd > 0 && xn > xd
   x = _fmpq_ball(steal_data!(xn), steal_data!(xd), 0, 0, 1)
   m = _fmpz_mat22(1,0,0,1,1)
   if ok
      s = _fmpq_cfrac_list(C_NULL, 0, 0, lim, 0, 0)
      ccall((:_fmpq_ball_get_cfrac, libflint), Nothing,
            (Ref{_fmpq_cfrac_list}, Ref{_fmpz_mat22}, Cint, Ref{_fmpq_ball}),
            s, m, wantM, x)
      _push_and_clear!(v, s)
   end
   xn.d = x.left_num; x.left_num = 0
   xd.d = x.left_den; x.left_den = 0
   return (steal_fmpz_data(m._11), steal_fmpz_data(m._12),
           steal_fmpz_data(m._21), steal_fmpz_data(m._22))
end

# xn and xd should be mutatable
function _continued_fraction_exact(xn::fmpz, xd::fmpz, lim::Int, wantM::Bool)
   lim = lim < 1 ? typemax(Int) : lim
   v = fmpz[]
   if xn > xd
      (m11, m12, m21, m22) = _doit_exact!(xn, xd, v, lim, wantM)
   else
      q, xn = fdivrem(xn, xd)
      swap!(xn, xd)
      push!(v, q)
      (m11, m12, m21, m22) = _doit_exact!(xn, xd, v, lim - 1, wantM)
      addmul!(m21, m11, q); swap!(m21, m11)
      addmul!(m22, m12, q); swap!(m22, m12)
   end
   while length(v) < lim && !iszero(xd)
      q, xn = fdivrem(xn, xd)
      swap!(xn, xd)
      push!(v, q)
      addmul!(m12, m11, q); swap!(m12, m11)
      addmul!(m22, m21, q); swap!(m22, m21)
   end
   return (v, (m11, m12, m21, m22))
end

function continued_fraction(x::fmpq; limit::Int = 0)
   return _continued_fraction_exact(numerator(x), denominator(x), limit, false)[1]
end

function continued_fraction_with_matrix(x::fmpq; limit::Int = 0)
   cf, m = _continued_fraction_exact(numerator(x), denominator(x), limit, true)
   return cf, matrix(FlintZZ, 2, 2, [m...])
end

function _doit_ball!(
   xln::fmpz, xld::fmpz,
   xrn::fmpz, xrd::fmpz,
   v::Vector{fmpz},
   lim::Int,
   wantM::Bool)

   x = _fmpq_ball(steal_data!(xln), steal_data!(xld),
                  steal_data!(xrn), steal_data!(xrd), 0)
   m = _fmpz_mat22(1,0,0,1,1)
   s = _fmpq_cfrac_list(C_NULL, 0, 0, lim, 0, 0)
   ccall((:_fmpq_ball_get_cfrac, libflint), Nothing,
         (Ref{_fmpq_cfrac_list}, Ref{_fmpz_mat22}, Cint, Ref{_fmpq_ball}),
         s, m, wantM, x)
   _push_and_clear!(v, s)
   xln.d = x.left_num; x.left_num = 0
   xld.d = x.left_den; x.left_den = 0
   xrn.d = x.right_num; x.right_num = 0
   xrd.d = x.right_den; x.right_den = 0
   return (steal_fmpz_data(m._11), steal_fmpz_data(m._12),
           steal_fmpz_data(m._21), steal_fmpz_data(m._22))
end

# xln, xld, xrd, xrd should all be mutatable
function _continued_fraction_ball(
   xln::fmpz, xld::fmpz,
   xrn::fmpz, xrd::fmpz,
   lim::Int,
   wantM::Bool)

   lim = lim < 1 ? typemax(Int) : lim
   v = fmpz[]
   if xln > xld
      (m11, m12, m21, m22) = _doit_ball!(xln, xld, xrn, xrd, v, lim, wantM)
      ok = true
   else
      (q, xln) = fdivrem(xln, xld)
      submul!(xrn, xrd, q)
      swap!(xln, xrd)
      swap!(xld, xrn)
      ok = xrd > 0 && xld > 0 && xln > xld
      if ok
         push!(v, q)
         (m11, m12, m21, m22) = _doit_ball!(xln, xld, xrn, xrd, v, lim - 1, wantM)
         addmul!(m21, m11, q); swap!(m21, m11)
         addmul!(m22, m12, q); swap!(m22, m12)
      else
         (m11, m12, m21, m22) = (fmpz(1), fmpz(0), fmpz(0), fmpz(1))
      end
   end
   while ok && length(v) < lim
      (q, xln) = fdivrem(xln, xld)
      submul!(xrn, xrd, q)
      swap!(xln, xrd)
      swap!(xld, xrn)
      ok = xrd > 0 && xld > 0 && xln > xld
      xln > xld || break
      push!(v, q)
      addmul!(m12, m11, q); swap!(m12, m11)
      addmul!(m22, m21, q); swap!(m22, m21)
   end
   return (v, (m11, m12, m21, m22))
end

# return four mutatable fmpz's
function _left_and_right(x::arb)
   a = fmpz()
   b = fmpz()
   f = fmpz()
   ccall((:arb_get_interval_fmpz_2exp, libarb), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{arb}),
         a, b, f, x)
   !fits(Int, f) && error("Result does not fit into an fmpq")
   e = Int(f)
   if f >= 0
      return (a << e, fmpz(1), b << e, fmpz(1))
   else
      return (a, fmpz(1) << -e, b, fmpz(1) << -e)
   end
end

function continued_fraction(x::arb; limit::Int = 0)
   xln, xld, xrn, xrd = _left_and_right(x)
   return _continued_fraction_ball(xln, xld, xrn, xrd, limit, false)[1]
end

function continued_fraction_with_matrix(x::arb; limit::Int = 0)
   xln, xld, xrn, xrd = _left_and_right(x)
   cf, m = _continued_fraction_ball(xln, xld, xrn, xrd, limit, true)
   return cf, matrix(FlintZZ, 2, 2, [m...])
end

###############################################################################
#
# convergents
#
###############################################################################

struct ConvergentsIterator{T, R}
   v::Vector{T}
end

Base.lastindex(it::ConvergentsIterator) = length(it.v)
Base.length(it::ConvergentsIterator)    = length(it.v)
Base.eltype(::Type{ConvergentsIterator{T, R}}) where {T, R} = R
Base.eltype(it::ConvergentsIterator{T, R})     where {T, R} = R
Base.collect(it::ConvergentsIterator{T, R})    where {T, R} = collect(R, it)

function Base.getindex(it::ConvergentsIterator{T, R}, n) where {T, R}
   m = mapreduce(q -> (q, T(1), T(1), T(0)),
                 (x, y) -> (x[1]*y[1] + x[2]*y[3], x[1]*y[2] + x[2]*y[4],
                            x[3]*y[1] + x[4]*y[3], x[3]*y[2] + x[4]*y[4]),
                 it.v[1:n])
   return m[1]//m[3]
end

function Base.iterate(it::ConvergentsIterator{T, R}) where {T, R}
   isempty(it.v) && return nothing
   return (it.v[1]//T(1), ((deepcopy(it.v[1]), T(1), T(1), T(0)), 2))
end

function Base.iterate(it::ConvergentsIterator{T, R}, state) where {T, R}
   n = state[2]
   n > length(it.v) && return nothing
   m = state[1]
   addmul!(m[2], m[1], it.v[n]); swap!(m[2], m[1])
   addmul!(m[4], m[3], it.v[n]); swap!(m[4], m[3])
   return (m[1]//m[3], (m, n + 1))
end

function convergents(cf::Vector{fmpz})
   return ConvergentsIterator{fmpz, fmpq}(cf)
end

