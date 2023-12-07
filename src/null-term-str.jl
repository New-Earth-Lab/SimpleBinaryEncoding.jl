
# We need to implement our own type-stable C-style null-terminated string to avoid allocations
# here. 
struct NullTermString{TBuf} <: AbstractString
    buffer::TBuf
    NullTermString(buffer::AbstractVector{<:UInt8}) = new{typeof(buffer)}(buffer)
    NullTermString(buffer::NTuple{N,UInt8}) where {N} = new{typeof(buffer)}(buffer)
end
function Base.ncodeunits(nstr::NullTermString)
    for i in 1:length(nstr.buffer)
        if nstr.buffer[i] == 0x00
            return i-1
        end
    end
    return length(nstr.buffer)
end
Base.codeunit(::NullTermString) = UInt8
Base.codeunit(nstr::NullTermString, i::Integer) = Char(nstr.buffer[i])
Base.isvalid(nstr::NullTermString, i::Integer) = true
Base.iterate(nstr::NullTermString, i::Integer=1) = if i <= Base.ncodeunits(nstr)
    nstr[i], i+1
else
    nothing
end

Base.eltype(::NullTermString) = Char
Base.sizeof(nstr::NullTermString) = sizeof(nstr.buffer)
Base.firstindex(::NullTermString) = 1
Base.lastindex(nstr::NullTermString) = Base.ncodeunits(nstr)
Base.isempty(nstr::NullTermString) = Base.ncodeunits(nstr) == 0
Base.getindex(nstr::NullTermString,i::Integer) = Char(nstr.buffer[i])
# Uses internals!
Base.Symbol(nstr::NullTermString) = Core._Symbol(pointer(nstr.buffer), Base.ncodeunits(nstr), nstr.buffer)