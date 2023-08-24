module SpidersMessageSerDes

using LightXML

const primitive_type_map = Dict(
    "uint8" => UInt8,
    "uint16" => UInt16,
    "uint32" => UInt32,
    "uint64" => UInt32,
    "int64" => Int64,
    "char" => UInt8,
)

abstract type CompositeDType end

function generate_accessors(filename)
    xdoc = parse_file(filename)

    xroot = root(xdoc)  
    local dtype_map
    # traverse all its child nodes and print element names
    for e in child_elements(xroot)  # c is an instance of XMLNode
        if name(e) == "include"
            href = attribute(e, "href")
            dtype_map = load_dtypes(href)
            dtype_map = merge(primitive_type_map, dtype_map)
            display(dtype_map)
        end
        if name(e) == "message"
            message_name = attribute(e, "name")
            message_description = attribute(e, "description")
            fields = parse_message(e, dtype_map)
            generate_struct(message_name, message_description, fields)
        end
    end
    free(xdoc)

end

function load_dtypes(href)
    xdoc = parse_file(href)

    xroot = root(xdoc)  

    type_map = Dict()

    # traverse all its child nodes and print element names
    for e in child_elements(xroot)  # c is an instance of XMLNode
        if name(e) == "type"
            type_map[attribute(e, "name")] = primitive_type_map[attribute(e, "primitiveType")]
        elseif name(e) == "composite"
            fields = parse_composite_type(e)
            type_map[attribute(e, "name")] = make_composite_type(e, fields)
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
    
    @eval begin
        # Put description field into docstring
        $type_description
        struct $(Symbol(type_name)){T<: AbstractArray{UInt8}} <: CompositeDType
            buffer::T
        end
    end
    
    # For autocomplete etc.
    @eval function Base.propertynames(sbe::$(Symbol(type_name)))
        props = ($(
            (Meta.quot(Symbol(field.name)) for field in fields
        )...),)
        return props
    end

    # For property access
    # construct expressions for each field access 
    field_access_exprs = map(fields) do field
        quote
            if prop == $(Meta.quot(Symbol(field.name)))
                # TODO: replace Int64 with the actual type and offset
                return reinterpret(Int64, view(getfield(sbe, :buffer), 9:16))[]
            end
        end
    end
    @eval function Base.getproperty(sbe::$(Symbol(type_name)), prop::Symbol)
        $(field_access_exprs...)
        error(lazy"type has no property $prop")
    end

    # If has a varLength and a length field, add a Base.sizeof method.
    # TODO:

    # For property setting
    # construct expressions for each field access 
    field_access_exprs = map(fields) do field
        quote
            if prop == $(Meta.quot(Symbol(field.name)))
                # TODO: replace Int64 with the actual type and offset
                return reinterpret(Int64, view(getfield(sbe, :buffer), 9:16))[] = value
            end
        end
    end
    @eval function Base.setproperty!(sbe::$(Symbol(type_name)), prop::Symbol, value)
        $(field_access_exprs...)
        error(lazy"type has no property $prop")
    end

    return @eval $(Symbol(type_name))

end

function generate_struct(message_name, message_description, fields)

    @eval begin
        # Put description field into docstring
        $message_description
        struct $(Symbol(message_name)){T<: AbstractArray{UInt8}}
            buffer::T
        end
    end
    
    # For autocomplete etc.
    @eval function Base.propertynames(sbe::$(Symbol(message_name)))
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
    offset_calc_exprs = Expr[:(offset = 1)]

    field_access_exprs = map(fields) do field
        DType = field.type
        if DType <: CompositeDType
            offset_calc_expr = quote
                # offset += 1
                # TODO: calc dynamically?
            end
        end
        offset_calc_expr = quote
            offset += sizeof(DType)
        end
        return quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(offset_calc_exprs...)
                # TODO: replace Int64 with the actual type and offset
                return reinterpret($DType, view(getfield(sbe, :buffer), offset:offset+sizeof($DType)))[]
            end
        end
    end
    @eval function Base.getproperty(sbe::$(Symbol(message_name)), prop::Symbol)
        $(field_access_exprs...)
        error(lazy"type has no property $prop")
    end

    # For property setting
    # construct expressions for each field access 
    field_access_exprs = map(fields) do field
        quote
            if prop == $(Meta.quot(Symbol(field.name)))
                # TODO: replace Int64 with the actual type and offset
                return reinterpret(Int64, view(getfield(sbe, :buffer), 9:16))[] = value
            end
        end
    end
    @eval function Base.setproperty!(sbe::$(Symbol(message_name)), prop::Symbol, value)
        $(field_access_exprs...)
        error(lazy"type has no property $prop")
    end

    return @eval $(Symbol(message_name))
end

generate_accessors("image.xml")

end;