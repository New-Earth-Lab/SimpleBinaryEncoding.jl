module SpidersMessageSerDes

using LightXML

function generate_accessors(filename)
    xdoc = parse_file(filename)

    xroot = root(xdoc)  

    # traverse all its child nodes and print element names
    for c in child_nodes(xroot)  # c is an instance of XMLNode
        if is_elementnode(c)
            e = XMLElement(c)  # this makes an XMLElement instance
            if name(e) == "message"
                message_name = attribute(e, "name")
                message_description = attribute(e, "description")
                fields = parse_message(e)
                generate_struct(message_name, message_description, fields)
            end
        end
    end
    free(xdoc)

end

# Accepts message XML element
function parse_message(e)
    fields = map(child_elements(e)) do field_element
        field_name = attribute(field_element, "name")
        field_description = attribute(field_element, "description")
        field_type = attribute(field_element, "type")
        # TODO: lookup of field type via include 
        return (;name=field_name, description=field_description, type=field_type)
    end
    return fields
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
    field_access_exprs = map(fields) do field
        quote
            if prop == $(Meta.quot(Symbol(field.name)))
                # TODO: replace Int64 with the actual type and offset
                return reinterpret(Int64, view(getfield(sbe, :buffer), 9:16))[]
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

end

generate_accessors("image.xml")

end;

# Testing it out in a REPL:
# ?MyModule.Image