###############################################################################
#
#   PariTypes.jl : Pari types
#
###############################################################################

###############################################################################
#
#   PariIntegerRing / pari_int
#
###############################################################################

type PariIntegerRing <: Ring{Pari}
end

type pari_int <: IntegerRingElem
   d::Ptr{Int}

   function pari_int(s::Int)
      g = new(ccall((:pari_malloc, :libpari), Ptr{Int}, 
                    (Int,), s*BITS_IN_WORD))
      finalizer(g, _pari_int_clear_fn)
      return g
   end
end

function _pari_int_clear_fn(g::pari_int)
   ccall((:pari_free, :libpari), Void, (Ptr{UInt},), g.d)
end

###############################################################################
#
#   PariRationalField / pari_rat
#
###############################################################################

type PariRationalField <: Field{Pari}
end

type pari_rat <: FracElem{pari_int}
   d::Ptr{Int}

   function pari_rat(s::Int)
      g = new(ccall((:pari_malloc, :libpari), Ptr{Int}, 
                    (Int,), s*BITS_IN_WORD))
      finalizer(g, _pari_rat_clear_fn)
      return g
   end
end

_pari_rat_clear_fn(g::pari_rat) = ccall((:pari_free, :libpari), Void, 
                                        (Ptr{UInt},), g.d)

###############################################################################
#
#   PariVector / pari_vec
#
###############################################################################

type PariVector{T <: SetElem}
   base_ring::Set{Pari}
end

type pari_vec{T <: SetElem}
   data::Ptr{Int}
   parent::PariVector{T}

   function pari_vec{R <: Set{Pari}}(v::Ptr{Int}, par::R)
      r = new(gclone(v), PariVector{T}(par))
      finalizer(r, _pari_vec_unclone)
      return r
   end
end

_pari_vec_unclone(a::pari_vec) = gunclone(a.data)

###############################################################################
#
#   PariPolyRing / pari_poly
#
###############################################################################

const PariPolyID = ObjectIdDict()

type PariPolyRing{T <: RingElem} <: Ring{Pari}
   base_ring::Ring
   pol_0::Ptr{Int}
   S::Symbol

   function PariPolyRing(R::Ring, s::Symbol)
      z = ccall((:pari_malloc, :libpari), Ptr{Int}, (Int,), 2*sizeof(Int))
      unsafe_store!(z, evaltyp(t_POL) | 2, 1) 
      unsafe_store!(z, evalsigne(0) | evalvarn(0), 2)
      try
         return PariPolyID[R, s]
      catch
         r = PariPolyID[R, s] = new(R, z, s)
         finalizer(r, _pari_poly_zero_clear_fn)
         return r
      end
      
   end
end

function _pari_poly_zero_clear_fn(p::PariPolyRing)
   ccall((:pari_free, :libpari), Void, (Ptr{Int},), p.pol_0)
end

type pari_poly{T <: RingElem} <: PolyElem{T}
   d::Ptr{Int}
   parent::PariPolyRing{T}

   function pari_poly(data::Ptr{Int})
      g = new(gclone(data))
      finalizer(g, _pari_poly_unclone)
      return g
   end

   function pari_poly(s::Int)
      g = new(ccall((:pari_malloc, :libpari), Ptr{Int}, (Int,), s*sizeof(Int)))
      finalizer(g, _pari_poly_clear_fn)
      return g
   end
end

function _pari_poly_clear_fn(g::pari_poly)
   ccall((:pari_free, :libpari), Void, (Ptr{UInt},), g.d)
end

_pari_poly_unclone(g::pari_poly) = gunclone(g.d)

###############################################################################
#
#   PariPolModRing / pari_polmod
#
###############################################################################

const PariPolModID = Dict{Tuple{DataType, Symbol}, Ring}()

type PariPolModRing{S <: PolyElem} <: Ring{Pari}
   T::Symbol

   function PariPolModRing(t::Symbol)
      try
         return PariPolModID[S, t]
      catch
         return PariPolModID[S, t] = new(t)
      end
   end
end

type pari_polmod{S <: PolyElem} <: ResElem{S}
   data::Ptr{Int}
   parent::PariPolModRing{S}

   function pari_polmod(data::Ptr{Int})
      r = new(gclone(data))
      finalizer(r, _pari_polmod_unclone)
      return r
   end
end

_pari_polmod_unclone(a::pari_polmod) = gunclone(a.data)

###############################################################################
#
#   PariNumberField
#
###############################################################################

const PariNumberFieldID = Dict{fmpq_poly, Ring}()

type PariNumberField <: Ring{Pari}
   data::Ptr{Int}
   nf::AnticNumberField
   
   function PariNumberField(nf::AnticNumberField)
      try
         return PariNumberFieldID[nf.pol]
      catch
         av = unsafe_load(avma, 1)
         p = pari(nf.pol)
         d = gclone(ccall((:nfinit, :libpari), Ptr{Int}, 
                           (Ptr{Int}, Int), p.d, 5))
         unsafe_store!(avma, av, 1)
         ord = new(d, nf)
         finalizer(ord, _pari_nf_unclone)
         return PariNumberFieldID[nf.pol] = ord
      end
   end
end

_pari_nf_unclone(a::PariNumberField) = gunclone(a.data)

###############################################################################
#
#   PariMaximalOrder / pari_maximal_order_elem
#
###############################################################################

type PariMaximalOrder <: Ring{Pari}
   pari_nf::PariNumberField
end

type pari_maximal_order_elem <: MaximalOrderElem
   data::Ptr{Int}
   parent::PariMaximalOrder

   function pari_maximal_order_elem(a::Ptr{Int}, par::PariMaximalOrder)
      r = new(gclone(a), par)
      finalizer(r, _pari_maximal_order_elem_clear_fn)
      return r
   end
end

_pari_maximal_order_elem_clear_fn(a::pari_maximal_order_elem) = gunclone(a.data)

###############################################################################
#
#   PariIdealSet / PariIdeal
#
###############################################################################

const PariIdealSetID = ObjectIdDict()

type PariIdealSet <: Set{Pari}
   order::PariMaximalOrder

   function PariIdealSet(ord::PariMaximalOrder)
      return try
         PariIdealSetID[ord]
      catch
         PariIdealSetID[ord] = new(ord)
      end
   end
end

type PariIdeal <: SetElem
   ideal::Ptr{Int}
   parent::PariIdealSet

   function PariIdeal(a::Ptr{Int}, par::PariIdealSet)
      r = new(gclone(a), par)
      finalizer(r, _pari_ideal_clear_fn)
      return r
   end
end

_pari_ideal_clear_fn(a::PariIdeal) = gunclone(a.ideal)

###############################################################################
#
#   PariFactor
#
###############################################################################

type PariFactor{T <: RingElem}
   data::Ptr{Int}
   len::Int
   parent::Set

   function PariFactor(p::Ptr{Int}, par::Set)
      col = reinterpret(Ptr{Int}, unsafe_load(p, 2))
      r = new(gclone(p), lg(col) - 1, par)
      finalizer(r, _PariFactor_unclone)
      return r
   end
end

_PariFactor_unclone(f::PariFactor) = gunclone(f.data)

include("code_words.jl")

