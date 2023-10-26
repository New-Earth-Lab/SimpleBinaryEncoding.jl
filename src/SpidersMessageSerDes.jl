module SpidersMessageSerDes

using LightXML
using StaticStrings

# Julia chars are UTF8. We want to wrap a simple UInt8 byte
# with a different type. This type will serve as a sentinal
# so that char arrays can be handled as StaticStrings.jl 
# StaticStrings with zero allocations.
struct ByteChar
    d::UInt8
end
# Since we're not actually using the ByteChar besides as a sentinal
# value there's no need to subtype and implement the AbsractChar interface.
Base.convert(::Type{ByteChar}, c::UInt8) = ByteChar(c)
Base.convert(::Type{NTuple{N,ByteChar}}, str::CStaticString{N}) where N = Base.convert(
    NTuple{N,ByteChar},
    Base.convert(NTuple{N,UInt8}, str)
)

primitive_type_map = Dict(
    # First value is the julia datatype
    # Second value is the length.
    # -1 means singleton, 0 means variable length, and a positive value means
    # an array of that type.
    "uint8" => UInt8,
    "uint16" => UInt16,
    "uint32" => UInt32,
    "uint64" => UInt64,
    "int64" => Int64,
    # We need some special handling to get char arrays appearing as strings
    # via StaticStrings with zero allocations.
    "char" => ByteChar,
)

abstract type CompositeDType end
abstract type VarLenDType <: AbstractVector{UInt8} end

"""
    evalschema(Main, "myschema.xml")

Given an XML file that defines a message in the "simple binary
encoding" format, create types in the provided module (via `eval`)
for encoding and decoding data.

If the schema defines
```xml
<sbe:message name="Image" id="1" description="Image message format">
    ...
</sbe:message>
```
Then a struct called `Image` will be defined that wraps
a byte array and provides an object style interface to it, ideally
with zero allocation overheads:
```julia
evalschema(Main, "image.xml")
buf = zeros(UInt8, 1000)
img = Image(buf)
img.height = 100
img.height # 0x00000064
```

Note that with bounds checking enabled (default) this is not "unsafe"
in any way. It's basically just a struct with a custom memory layout.
"""
function evalschema(Mod::Module, filename::AbstractString)
    xdoc = parse_file(filename)

    xroot = root(xdoc)  
    local dtype_map
    # traverse all its child nodes and print element names
    for e in child_elements(xroot)  # c is an instance of XMLNode
        if name(e) == "include"
            # @info "including linked file"
            href = attribute(e, "href")
            dtype_map = load_dtypes(Mod, href)
            dtype_map = merge(primitive_type_map, dtype_map)
        end
        if name(e) == "message"
            message_name = attribute(e, "name")
            message_description = attribute(e, "description")
            # @info "message type" message_name message_description
            fields = parse_message(e, dtype_map)
            generate_message_type(Mod, message_name, message_description, fields)
        end
    end
    free(xdoc)

    nothing
end

function load_dtypes(Mod, href)
    xdoc = parse_file(href)

    xroot = root(xdoc)  

    type_map = Dict()

    # traverse all its child nodes and print element names
    for e in child_elements(xroot)  # c is an instance of XMLNode
        if name(e) == "type" || name(e) == "composite"
            # Look up primitive type 
            if name(e) == "type"
                T = primitive_type_map[attribute(e, "primitiveType")]
            # Or check if we are defining a new composite type
            elseif name(e) == "composite"
                fields = parse_composite_type(e)
                if any(field->field.name == "varData", fields)
                    T = make_variable_length_type(Mod, e, fields)
                else
                    T = make_composite_type(Mod, e, fields)
                end
            end
            # Handle length field
            if has_attribute(e, "length")
                len = parse(Int64, attribute(e, "length"))
                if len > 0
                    T = NTuple{len,T}
                elseif len == 0
                    # Len 0 means a variable length block.
                    # We'll mark this as a Vector, not because we'll really
                    # use a vector, but just as a sentinal value 
                    # to trigger the variable length data handling
                    T = Vector{T}
                else
                    error("unsupported length field $len")
                end
            end
            type_map[attribute(e, "name")] = T
        end
    end
    # dtype_map = 
    return type_map
end

# Accepts message XML element
function parse_message(e, type_map)
    fields = map(child_elements(e)) do field_element
        field_name = attribute(field_element, "name")
        field_description = attribute(field_element, "description")
        field_type = attribute(field_element, "type")
        return (;name=field_name, description=field_description, type=type_map[field_type])
    end
    return fields
end

# function parse_composite_type(e)
#     fields = map(child_elements(e)) do field_element
#         field_name = attribute(field_element, "name")
#         field_description = attribute(field_element, "description")
#         field_type = attribute(field_element, "type")
#         return (;name=field_name, description=field_description, type=field_type)
#     end
#     return fields
# end

function parse_composite_type(element)
    fields = map(child_elements(element)) do field_element
        field_name = attribute(field_element, "name")
        field_type_name = attribute(field_element, "primitiveType")
        field_type = primitive_type_map[field_type_name]
        return (;name=field_name, type=field_type)
    end
    return fields
end


"""
Internal.
Generate a struct that wraps a byte buffer and has an interface
as described in a schema file as a "composite type", basically a
struct.
"""
function make_composite_type(Mod, element, fields)
    type_name = attribute(element, "name")
    type_description = attribute(element, "description")
    # @info "Defining composite type" type_name type_description fields
    
    @show Mod
    @eval Mod begin
        # Put description field into docstring
        $type_description
        struct $(Symbol(type_name)){T<: AbstractArray{UInt8}} <: $(CompositeDType)
            buffer::T
        end
    end
    
    # For autocomplete etc.
    @eval Mod function Base.propertynames(sbe::$(Symbol(type_name)))
        props = ($(
            (Meta.quot(Symbol(field.name)) for field in fields
        )...),)
        return props
    end

    # The offset to each field must be calculated dynamically at run-
    # time due to the precense of variable length fields.
    # We build up a list of expressions calculating offsets as we go.
    # Each subsequent field adds their calculation to the offsets as a 
    # new expression.
    offset_calc_exprs = Expr[:(offset = 0)]
    getfield_exrps = map(fields) do field
        DType = field.type
        offset_calc_expr = if DType <: CompositeDType || DType <: VarLenDType quote
            # offset += 1
            # TODO: calc dynamically?
            @show "TODO: must calculate composite subfield length dynamically"
        end else quote
            offset += sizeof($(DType))
        end end
        push!(offset_calc_exprs, offset_calc_expr)
        expr = quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(offset_calc_exprs[1:end-1]...) # Just interpolate in offsets for fields passed so far
                # @show offset:offset+sizeof($DType)
                # if $(DType) <: NTuple{N,Char} where N

                #     return N
                #     # return @inline reinterpret(NTuple{}, view(getfield(sbe, :buffer), offset+1:offset+sizeof($(DType))))[]
                # else
                    return @inline reinterpret($(DType), view(getfield(sbe, :buffer), offset+1:offset+sizeof($(DType))))[]
                # end
            end
        end
        return expr
    end
    @eval Mod function Base.sizeof(sbe::$(Symbol(type_name)))
        $(offset_calc_exprs...)
        return offset
    end
    @eval Mod @inline function Base.getproperty(sbe::$(Symbol(type_name)), prop::Symbol)
        # @info "getting property"
        $(getfield_exrps...)
        error(lazy"type has no property $prop")
    end

    offset_calc_exprs = Expr[:(offset = 0)]
    setfield_exprs = map(fields) do field
        DType = field.type
        offset_calc_expr = if DType <: CompositeDType || DType <: VarLenDType quote
            # offset += 1
            # TODO: calc dynamically?
            @show "TODO: must calculate composite subfield length dynamically"
        end else quote
            offset += sizeof($(DType))
        end end
        push!(offset_calc_exprs, offset_calc_expr)
        expr = quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(offset_calc_exprs[1:end-1]...) # Just interpolate in offsets for fields passed so far
                # @show offset:offset+sizeof($DType)
                return @inline reinterpret($(DType), view(getfield(sbe, :buffer), offset+1:offset+sizeof($(DType))))[]
            end
        end
        return expr
    end
    @eval Mod @inline function Base.setproperty!(sbe::$(Symbol(type_name)), prop::Symbol, value)
        # @info "getting property"
        $(setfield_exprs...)
        error(lazy"type has no property $prop")
    end


    return @eval Mod $(Symbol(type_name))

end


"""
Internal.
Generate a struct that wraps a byte buffer and has an interface
as described in a schema file as variable length encoded data.
"""
function make_variable_length_type(Mod, element, fields)
    type_name = attribute(element, "name")
    type_description = attribute(element, "description")
    @info "Defining variable length type" type_name type_description fields

    # Check that the definition matches what we support:
    # a single length field followed by a 
    if length(fields) < 2
        error("The `length` and `varData` fields are required for variable length types")
    end
    if fields[1].name != "length"
        error("`length`` field must be first")
    end
    if fields[2].name != "varData"
        error("`varData` field must come second")
    end
    if length(fields) > 2
        error("only the `length` and `varData` fields are supported for variable length types")
    end

    lenfield = fields[1]

    @eval Mod begin
        # Put description field into docstring
        $type_description
        struct $(Symbol(type_name)){B<: AbstractArray{UInt8}} <: $(VarLenDType)
            buffer::B
        end
    end

    @eval Mod Base.parent(sbe::$(Symbol(type_name))) = view(getfield(sbe,:buffer), $(sizeof(lenfield.type))+1:$(sizeof(lenfield.type))+length(sbe))
    
    # Forward all functions needed to implement the AbstractArray interface
    # to the parent array (a view into the buffer of calculated size)
    funcs = (
        :size,
        :getindex,
        :setindex!,
        :iterate,
        :similar,
        :axes,
    )
    for func in funcs
        @eval Mod Base.$(func)(arr::$(Symbol(type_name)), args...; kwargs...) = Base.$(func)(parent(arr), args...; kwargs...)
    end

    # For autocomplete etc. Hide internal fields.
    @eval Mod Base.propertynames(sbe::$(Symbol(type_name))) = tuple()

    @eval Mod function Base.sizeof(sbe::$(Symbol(type_name)))
        return $(sizeof(lenfield.type)) + length(sbe)
    end
    
    @eval Mod function Base.length(sbe::$(Symbol(type_name)))
        return reinterpret($(lenfield.type), view(getfield(sbe, :buffer), 1:$(sizeof(lenfield.type))))[]
    end

    # To set the size of the buffer, we implement Base.resize.
    # We just change the length field of the composite type.
    # We do check that the buffer is sufficiently large but 
    # if the user adjusts the buffer smaller and it can no longer
    # accomodate the size of this varData, then they will get
    # an out of bounds error when they try to access it.
    @eval Mod function Base.resize!(sbe::$(Symbol(type_name)), len)
        # TODO: throw error if not right sized at start
        if len < 0
            error("Cannot have a negative length")
        elseif len + $(sizeof(lenfield.type)) > length(getfield(sbe, :buffer))
            error("Backing buffer is too small to accomodate this resize! request.")            
        end
        return reinterpret($(lenfield.type), view(getfield(sbe, :buffer), 1:$(sizeof(lenfield.type))))[] = len
    end
    

    return @eval Mod $(Symbol(type_name))

end


"Return the length of a tuple (statically) given its type"
tuple_len(::Type{<:NTuple{N, Any}}) where {N} = N

"""
Internal.
Generate a struct that wraps a byte buffer and has an interface
as described in a schema file 
"""
function generate_message_type(Mod, message_name, message_description, fields)
    # @info "generate struct " message_name fields

    @eval Mod begin
        # Put description field into docstring
        $message_description
        struct $(Symbol(message_name)){T<: AbstractArray{UInt8}}
            buffer::T
        end
    end
    
    # For autocomplete etc.
    @eval Mod function Base.propertynames(sbe::$(Symbol(message_name)))
        props = ($(
            (Meta.quot(Symbol(field.name)) for field in fields
        )...),)
        return props
    end

    # For property access
    # construct expressions for each field access 

    # The offset to each field must be calculated dynamically at run-
    # time due to the precense of variable length fields.
    # We build up a list of expressions calculating offsets as we go.
    # Each subsequent field adds their calculation to the offsets as a 
    # new expression.
    offset_calc_exprs = Expr[:(offset = 0)]
    getprop_exrps = map(fields) do field
        DType = field.type
        offset_calc_expr = if DType <: CompositeDType || DType <: VarLenDType quote
            @info "calculating offset dynamically" $(field.name)
            @info "test" $(field.name)
            # len = sizeof(sbe.$(Symbol(field.name)))
            # @info "done"  len
            len = 1
            offset += len
        end else quote
            offset += sizeof($(DType))
        end end
        push!(offset_calc_exprs, offset_calc_expr)
        # Create an expression for accessing this property 
        return quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(offset_calc_exprs[1:end-1]...) # Just interpolate in offsets for fields passed so far
                $(
                    # If we have a char array, return it as a StaticString.
                    if DType <: NTuple{N,ByteChar} where N
                        BytesType = NTuple{tuple_len(DType),UInt8}
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($BytesType);
                            return @inline $(CStaticString)(reinterpret($BytesType,view(getfield(sbe, :buffer), offset+1:offset+sizeof($(BytesType))))[])
                        )
                     # If we have a variable length field, return it directly?
                    elseif DType <: CompositeDType || DType <: VarLenDType
                        @info "composite field"
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($BytesType);
                            # When constructing a composite field, just pass in the remainder of the buffer.
                            # We don't necessarily know how long it is, so we trust it not to touch 
                            # past it's own Base.sizeof(data::DType) which may be computed dynamically
                            # e.g. for variable length data.
                            return $(DType)(view(getfield(sbe, :buffer), offset+1:length(getfield(sbe, :buffer))))
                        )
                    # Otherwise just directly reinterpret
                    else
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($DType);
                            return @inline reinterpret($(DType), view(getfield(sbe, :buffer), offset+1:offset+sizeof($(DType))))[]
                        )
                    end
                )
            end
        end
    end
    @eval Mod @inline function Base.getproperty(sbe::$(Symbol(message_name)), prop::Symbol)
        # @info "getting property"
        $(getprop_exrps...)
        error(lazy"type has no property $prop")
    end

    @eval Mod function Base.sizeof(sbe::$(Symbol(message_name)))
        $(offset_calc_exprs...)
        return offset
    end

    offset_calc_exprs = Expr[:(offset = 0)]
    setprop_exprs = map(fields) do field
        DType = field.type
        offset_calc_expr = if DType <: CompositeDType || DType <: VarLenDType quote
            # offset += 1
            # TODO: calc dynamically?
            @show "TODO: must calculate composite subfield length dynamically"
        end else quote
            offset += sizeof($(DType))
        end end
        push!(offset_calc_exprs, offset_calc_expr)
        expr = quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(offset_calc_exprs[1:end-1]...) # Just interpolate in offsets for fields passed so far
                $(
                    # If we have a char array, return it as a StaticString.
                    if DType <: NTuple{N,ByteChar} where N
                        L = tuple_len(DType)
                        BytesType = NTuple{L,UInt8}
                        :(
                            # @show offset+1:offset+sizeof($BytesType);
                            return @inline reinterpret($BytesType,view(getfield(sbe, :buffer), offset+1:offset+sizeof($(BytesType))))[] = CStaticString{$L}(value)
                        )
                    # Otherwise just directly reinterpret
                    else
                        :(
                            # @show offset+1:offset+sizeof($DType);
                            return @inline reinterpret($(DType), view(getfield(sbe, :buffer), offset+1:offset+sizeof($(DType))))[] = value
                        )
                    end
                )
            end
        end
        return expr
    end
    @eval Mod @inline function Base.setproperty!(sbe::$(Symbol(message_name)), prop::Symbol, value)
        # @info "getting property"
        $(setprop_exprs...)
        error(lazy"type has no property $prop")
    end

    return @eval Mod $(Symbol(message_name))
end

# evalschema("../sbe-schemas/image.xml")

end;