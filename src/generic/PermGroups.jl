###############################################################################
#
#   PermGroup / perm
#
###############################################################################

const PermID = ObjectIdDict()

type PermGroup <: Group
   n::Int

   function PermGroup(n::Int, cached=true)
      if haskey(PermID, n)
         return PermID[n]::PermGroup
      else
         z = new(n)
         if cached
            PermID[n] = z
         end
         return z
      end
   end
end

type perm <: GroupElem
   d::Array{Int, 1}
   cycles::Vector{Vector{Int}}
   parent::PermGroup

   function perm(n::Int)
      return new(collect(1:n))
   end

   function perm(a::Array{Int, 1})
      return new(a)
   end
end

export PermGroup, perm, parity, elements, cycles, character

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{perm}) = PermGroup

elem_type(::Type{PermGroup}) = perm

###############################################################################
#
#   Basic manipulation
#
###############################################################################

doc"""
    parent(a::perm)
> Return the parent of the given permutation group element.
"""
parent(a::perm) = a.parent

function deepcopy_internal(a::perm, dict::ObjectIdDict)
   G = parent(a)
   return G(deepcopy(a.d))
end

function Base.hash(a::perm, h::UInt)
   b = 0x595dee0e71d271d0%UInt
   for i in 1:length(a.d)
      b $= hash(a[i], h) $ h
      b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

doc"""
    parity(a::perm)
> Return the parity of the given permutation, i.e. the parity of the number of
> transpositions that compose it. The function returns $1$ if the parity is odd
> otherwise it returns $0$.
"""
# TODO: 2x slower than Flint
function parity(a::perm)
   to_visit = trues(a.d)
   parity = length(to_visit)
   k = 1
   while any(to_visit)
      parity -= 1
      k = findnext(to_visit, k)
      to_visit[k] = false
      next = a[k]
      while next != k
         to_visit[next] = false
         next = a[next]
      end
   end
   return parity%2
end

doc"""
    sign(a::perm)
> Returns the sign of the given permutations, i.e. `1` if `a` is even and `-1`
> if `a` is odd.
"""
sign(a::perm) = (-1)^parity(a)

function getindex(a::perm, n::Int)
   return a.d[n]
end

function setindex!(a::perm, d::Int, n::Int)
   a.d[n] = d
end

doc"""
    eye(G::PermGroup)
> Return the identity permutation for the given permutation group.
"""
eye(G::PermGroup) = G()

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, G::PermGroup)
   print(io, "Permutation group over $(G.n) elements")
end

type PermDisplayStyle
   format::Symbol
end

const perm_display_style = PermDisplayStyle(:array)

doc"""
    setpermstyle(format::Symbol)
> Nemo can display (in REPL or in general as string) permutations by either
> array of integers whose `n`-th position represents value at `n`, or as
> (an array of) disjoint cycles, where the value of permutation at `n` is
> represented as the entry immediately following `n` in the cycle.
> The style can be switched by calling `setpermstyle` with `:array` or
> `:cycles` argument.
"""
function setpermstyle(format::Symbol)
   format in (:array, :cycles) || throw("Permutations can be displayed
   only as :array or :cycles.")
   perm_display_style.format = format
   return format
end

function show(io::IO, a::perm)
   if perm_display_style.format == :array
      print(io, "[" * join(a.d, ", ") * "]")
   elseif perm_display_style.format == :cycles
      if a == parent(a)()
         print(io, "()")
      else
         print(io, join(["("*join(c, ",")*")" for c in cycles(a) if
            length(c)>1],""))
      end
   end
end

###############################################################################
#
#   Comparison
#
###############################################################################

doc"""
    ==(a::perm, b::perm)
> Return `true` if the given permutations are equal, otherwise return `false`.
"""
function ==(a::perm, b::perm)
   parent(a) == parent(b) || return false
   a.d == b.d || return false
   return true
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
    *(a::perm, b::perm)
> Return the composition of the two permutations, i.e. $a\circ b$. In other
> words, the permutation corresponding to applying $b$ first, then $a$, is
> returned.
"""
function *(a::perm, b::perm)
   d = similar(a.d)
   for i in 1:length(d)
      d[i] = a[b[i]]
   end
   G = parent(a)
   return G(d)
end

doc"""
    ^(a::perm, n::Int)
> Return the `n`-th power of a permutation `a`. By default Nemo computes powers
> by cycle decomposition of `a`. `Nemo.power_by_squaring` provides a different
> method for powering which may or may not be faster, depending on the
> particuar case. Due to caching of cycle structure repeatedly powering should
> be faster with the default method. NOTE: cycle structure is not computed
> for `n<4`.
"""
function ^(a::perm, n::Int)
   if n <0
      return inv(a)^-n
   elseif n == 0
      return parent(a)()
   elseif n == 1
      return deepcopy(a)
   elseif n == 2
      return parent(a)(a.d[a.d])
   elseif n == 3
      return parent(a)(a.d[a.d[a.d]])
   else
      new_perm = similar(a.d)
      cyls = cycles(a)
      for cycle in cyls
         k = n % length(cycle)
         shifted = circshift(cycle, -k)
         for (idx,j) in enumerate(cycle)
            new_perm[j] = shifted[idx]
         end
      end
      return parent(a)(new_perm)
   end
end

function power_by_squaring(a::perm, b::Int)
   if n <0
      return inv(a)^-n
   elseif n == 0
      return parent(a)()
   elseif n == 1
      return deepcopy(a)
   elseif n == 2
      return parent(a)(a.d[a.d])
   elseif n == 3
      return parent(a)(a.d[a.d[a.d]])
   else
      bit = ~((~UInt(0)) >> 1)
      while (UInt(bit) & b) == 0
         bit >>= 1
      end
      cache1 = deepcopy(a.d)
      cache2 = deepcopy(a.d)
      bit >>= 1
      while bit != 0
         cache2 = cache1[cache1]
         cache1 = cache2
         if (UInt(bit) & b) != 0
            cache1 = cache1[a.d]
         end
         bit >>= 1
      end
      return parent(a)(cache1)
   end
end

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(a::perm)
> Return the inverse of the given permutation, i.e. the permuation $a^{-1}$
> such that $a\circ a^{-1} = a^{-1}\circ a$ is the identity permutation.
"""
function inv(a::perm)
   d = similar(a.d)
   for i in 1:length(a.d)
      d[a[i]] = i
   end
   G = parent(a)
   return G(d)
end

# TODO: can we do that in place??
function inv!(a::perm)
   d = similar(a.d)
   for i in 1:length(a.d)
      d[a[i]] = i
   end
   a.d = d
end

###############################################################################
#
#   Iterating over all permutations
#
###############################################################################

doc"""
    AllPerms(n::Int)
> Returns an iterator over arrays representing all permutations of `1:n`.
> Similar to `Combinatorics.permutations(1:n)`
"""
immutable AllPerms
   n::Int
   all::Int
   AllPerms(n::Int) = new(n, factorial(n))
end

Base.start(A::AllPerms) = (collect(1:A.n), 1, 1, ones(Int, A.n))
Base.next(A::AllPerms, state) = all_perms(state...)
Base.done(A::AllPerms, state) = state[2] > A.all
Base.eltype(::Type{AllPerms}) = Vector{Int}
length(A::AllPerms) = A.all

function all_perms(elts, counter, i, c)
   if counter == 1
      return (copy(elts), (elts, counter+1, i, c))
   end
   n = length(elts)
   while i <= n
      if c[i] < i
         if isodd(i)
            elts[1], elts[i] = elts[i], elts[1]
         else
            elts[c[i]], elts[i] = elts[i], elts[c[i]]
         end
         c[i] += 1
         i = 1
         return (copy(elts), (elts, counter+1, i, c))
      else
         c[i] = 1
         i += 1
      end
   end
end

doc"""
    elements(G::PermGroup)
> Returns an iterator over all elements in the group $G$. You may use
> `collect(elements(G))` to get an array of all elements.
"""
elements(G::PermGroup) = (G(p) for p in AllPerms(G.n))

###############################################################################
#
#   Misc
#
###############################################################################

doc"""
    order(G::PermGroup)
> Returns the order of the full permutation group.
"""
order(G::PermGroup) = factorial(G.n)

doc"""
    cycles(a::perm)
> Decomposes permutation into disjoint cycles.
"""
function cycles(a::perm)
   if isdefined(a, :cycles)
      return a.cycles
   else
      to_visit = trues(a.d)
      cycles = Vector{Vector{Int}}()
      k = 1
      while any(to_visit)
         cycle = Vector{Int}()
         k = findnext(to_visit, k)
         to_visit[k] = false
         push!(cycle, k)
         next = a[k]
         while next != k
            push!(cycle, next)
            to_visit[next] = false
            next = a[next]
         end
         push!(cycles, cycle)
      end
      a.cycles = cycles
      return cycles
   end
end

doc"""
    order(a::perm)
> Returns the order of permutation `a`.
"""
order(a::perm) = lcm([length(c) for c in cycles(a)])

doc"""
    permtype(a::perm, rev=true)
> Returns the type of permutation `a`, i.e. lengths of disjoint cycles building
> `a`. This fully determines conjugacy class of `a`. the lengths are sorted in
> reverse order by default.
"""
permtype(a::perm) = sort([length(c) for c in cycles(a)], rev=true)

doc"""
    matrix_repr(a::perm)
> Return the permutation matrix representing `a` via natural embedding of the
> permutation group into general linear group over ZZ
"""
function matrix_repr(a::perm)
   A = eye(Int, parent(a).n)
   return A[a.d,:]
end

doc"""
    emb!(result::perm, p::perm, V::Vector{Int})
> Embedds permutation `p` into `result` on the indices given by `V`. This
> corresponds to natural embedding of $S_k$ into $S_n$ as the subgroup
> permuting points indexed by `V`.
"""
function emb!(result::perm, p::perm, V::Vector{Int})
    result.d[V] = (result.d[V])[p.d]
    return result
end

doc"""
    emb(G::PermGroup, V::Vector{Int})
> Returns the natural embedding of a permutation group into `G` as the
> subgroup permuting points indexed by `V`.
"""
function emb(G::PermGroup, V::Vector{Int}, check=true)
   if check
      @assert length(Base.Set(V)) == length(V)
      @assert all(V .<= G.n)
   end
   return p -> Nemo.emb!(G(), p, V)
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (G::PermGroup)()
   z = perm(G.n)
   z.parent = G
   return z
end

function (G::PermGroup)(a::Array{Int, 1}, check::Bool=true)
   length(a) != G.n && error("Unable to coerce to permutation: lengths differ")
   if check
      Base.Set(a) != Base.Set(1:length(a)) && error("Unable to coerce to
         permutation: non-unique elements in array")
   end
   z = perm(a)
   z.parent = G
   return z
end

function (G::PermGroup)(a::perm)
   parent(a) != G && error("Unable to coerce to permutation: wrong parent")
   return a
end

###############################################################################
#
#   PermGroup constructor
#
###############################################################################

# handled by inner constructors

##############################################################################
#
#   Irreducible Characters
#
##############################################################################

doc"""
    character(λ::Partition)
> Returns the λ-th irreducible character of permutation group on `sum(λ)`
> generators. The returned character function `χ(p::perm, check=true)` can be
> evaluated on a permutation `p::perm` of length `sum(λ)`. Checking this
> condition can be switched off by calling `χ(p, false)`.
> The values computed by `χ` are stored in look-up table.
>
> `χ` computes its values using the Murnaghan-Nakayama formula:
> $$\chi_\lambda(\sigma) = \sum_{\text{rimhook }\xi\subset \lambda}
(-1)^{ll(\lambda\backslash\xi)} \chi_{\lambda \backslash\xi}(\tilde\sigma)$$
> where $\lambda\backslash\xi$ denotes partition associated with Young Diagram
> of $\lambda$ with $\xi$ removed, $ll$ denotes the leg-length (i.e. number of
> rows - 1) and $\tilde\sigma$ is permutation obtained from $\sigma$ by the
> removal of the longest cycle. For more details see e.g. Chapter 2.8 of _Group
> Theory and Physics_ by S.Sternberg.
"""
function character(λ::Partition)
   R = partitionseq(λ)

   function char(p::perm, check=true)
      if check
         length(p.d) == sum(λ) || throw("Can't evaluate character on $p : lengths differ.")
      end
      μ = Partition(permtype(p))
      return MN1inner(R, Partition(μ), 1, _charvalsTable)
   end

   return char
end

doc"""
    character(λ::Partition, p::perm, check=true)
> Returns the value of `λ-th` irreducible character on permutation `p`.
"""
function character(λ::Partition, p::perm, check=true)
   if check
      sum(λ) == length(p.d) || throw("λ-th irreducible character can be evaluated only on permutations of length $(sum(λ)).")
   end

   return MN1inner(partitionseq(λ), permtype(p), 1, _charvalsTable)
end

doc"""
    character(λ::Partition, μ::Partition)
> Returns the value of `λ-th` irreducible character on the conjugacy class
> represented by partition `μ`. Values of characters computed by this method
> are NOT globally cached.
"""
character(λ::Partition, μ::Partition) = MN1inner(partitionseq(λ), μ, 1)
