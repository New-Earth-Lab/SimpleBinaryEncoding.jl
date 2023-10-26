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

function generate_accessors(filename)
    xdoc = parse_file(filename)

    xroot = root(xdoc)  
    local dtype_map
    # traverse all its child nodes and print element names
    for e in child_elements(xroot)  # c is an instance of XMLNode
        if name(e) == "include"
            @info "including linked file"
            href = attribute(e, "href")
            dtype_map = load_dtypes(href)
            dtype_map = merge(primitive_type_map, dtype_map)
        end
        if name(e) == "message"
            message_name = attribute(e, "name")
            message_description = attribute(e, "description")
            @info "message type" message_name message_description
            fields = parse_message(e, dtype_map)
            generate_struct(message_name, message_description, fields)
        end
    end
    free(xdoc)

    nothing
end

function load_dtypes(href)
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
                T = make_composite_type(e, fields)
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
        # TODO: lookup of field type via include 
        return (;name=field_name, description=field_description, type=type_map[field_type])
    end
    display(fields)
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
function make_composite_type(element, fields)
    type_name = attribute(element, "name")
    type_description = attribute(element, "description")
    @info "Defining composite type" type_name type_description fields
    
    @eval Main begin
        # Put description field into docstring
        $type_description
        struct $(Symbol(type_name)){T<: AbstractArray{UInt8}} <: $(CompositeDType)
            buffer::T
        end
    end
    
    # For autocomplete etc.
    @eval Main function Base.propertynames(sbe::$(Symbol(type_name)))
        props = ($(
            (Meta.quot(Symbol(field.name)) for field in fields
        )...),)
        return props
    end


    # # If has a varLength and a length field, add a Base.sizeof method.
    # # TODO:
    # if has

   
    # The offset to each field must be calculated dynamically at run-
    # time due to the precense of variable length fields.
    # We build up a list of expressions calculating offsets as we go.
    # Each subsequent field adds their calculation to the offsets as a 
    # new expression.
    offset_calc_exprs = Expr[:(offset = 0)]
    getfield_exrps = map(fields) do field
        DType = field.type
        offset_calc_expr = if DType <: CompositeDType quote
            # offset += 1
            # TODO: calc dynamically?
            @show "TODO: must calculate composite subfield length dynamically"
        end else quote
            offset += sizeof($(DType))
        end end
        push!(offset_calc_exprs, offset_calc_expr)
        expr = quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(offset_calc_exprs...) # Just interpolate in offsets for fields passed so far
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
    @eval Main @inline function Base.getproperty(sbe::$(Symbol(type_name)), prop::Symbol)
        # @info "getting property"
        $(getfield_exrps...)
        if !found
            error(lazy"type has no property $prop")
        end
    end

    offset_calc_exprs = Expr[:(offset = 0)]
    setfield_exprs = map(fields) do field
        DType = field.type
        offset_calc_expr = if DType <: CompositeDType quote
            # offset += 1
            # TODO: calc dynamically?
            @show "TODO: must calculate composite subfield length dynamically"
        end else quote
            offset += sizeof($(DType))
        end end
        push!(offset_calc_exprs, offset_calc_expr)
        expr = quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(offset_calc_exprs...) # Just interpolate in offsets for fields passed so far
                # @show offset:offset+sizeof($DType)
                return @inline reinterpret($(DType), view(getfield(sbe, :buffer), offset+1:offset+sizeof($(DType))))[]
            end
        end
        return expr
    end
    @eval Main @inline function Base.setproperty!(sbe::$(Symbol(type_name)), prop::Symbol, value)
        # @info "getting property"
        $(setfield_exprs...)
        error(lazy"type has no property $prop")
    end


    return @eval Main $(Symbol(type_name))

end

tuple_len(::Type{<:NTuple{N, Any}}) where {N} = N

function generate_struct(message_name, message_description, fields)
    @info "generate struct " message_name fields

    @eval Main begin
        # Put description field into docstring
        $message_description
        struct $(Symbol(message_name)){T<: AbstractArray{UInt8}}
            buffer::T
        end
    end
    
    # For autocomplete etc.
    @eval Main function Base.propertynames(sbe::$(Symbol(message_name)))
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
        offset_calc_expr = if DType <: CompositeDType quote
            # offset += 1
            # TODO: calc dynamically?
            @show "TODO: must calculate composite subfield length dynamically"
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
                            # @show offset+1:offset+sizeof($BytesType);
                            return @inline $(CStaticString)(reinterpret($BytesType,view(getfield(sbe, :buffer), offset+1:offset+sizeof($(BytesType))))[])
                        )
                    # Otherwise just directly reinterpret
                    else
                        :(
                            # @show offset+1:offset+sizeof($DType);
                            return @inline reinterpret($(DType), view(getfield(sbe, :buffer), offset+1:offset+sizeof($(DType))))[]
                        )
                    end
                )
            end
        end
    end
    @eval Main @inline function Base.getproperty(sbe::$(Symbol(message_name)), prop::Symbol)
        # @info "getting property"
        $(getprop_exrps...)
        error(lazy"type has no property $prop")
    end

    offset_calc_exprs = Expr[:(offset = 0)]
    setprop_exprs = map(fields) do field
        DType = field.type
        offset_calc_expr = if DType <: CompositeDType quote
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
    @eval Main @inline function Base.setproperty!(sbe::$(Symbol(message_name)), prop::Symbol, value)
        # @info "getting property"
        $(setprop_exprs...)
        error(lazy"type has no property $prop")
    end

    return @eval Main $(Symbol(message_name))
end

# generate_accessors("../sbe-schemas/image.xml")

end;