export NonArchLocalField
export NonArchLocalFieldElem
export FlintLocalField
export FlintLocalFieldElem
export NALocalField
export NALocalFieldElem

parent_type(::Type{NALocalFieldElem}) = NALocalField

parent_type(::Type{FlintLocalFieldElem}) = FlintLocalField
