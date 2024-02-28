module SimpleBinaryEncoding

using LightXML
using StaticStrings

include("null-term-str.jl")


# Julia chars are UTF8. We want to wrap a simple UInt8 byte
# with a different type. This type will serve as a sentinal
# so that char arrays can be handled using StaticStrings.jl 
# with zero allocations.
struct ByteChar
    d::UInt8
end
# Since we're not actually using the ByteChar besides as a sentinal
# value there's no need to subtype and implement the AbsractChar interface.
# Just create conversion functions to map back and forth from ByteChar
# to byte, and NTuple{N,ByteChar} to NTuple{N,UInt8}
Base.convert(::Type{ByteChar}, c::UInt8) = ByteChar(c)
Base.convert(::Type{NTuple{N,ByteChar}}, str::CStaticString{N}) where {N} = Base.convert(
    NTuple{N,ByteChar},
    Base.convert(NTuple{N,UInt8}, str)
)

const primitive_type_map = Dict(
    # First value is the julia datatype
    # Second value is the length.
    # -1 means singleton, 0 means variable length, and a positive value means
    # an array of that type.
    "uint8" => UInt8,
    "uint16" => UInt16,
    "uint32" => UInt32,
    "uint64" => UInt64,
    "int8" => Int8,
    "int16" => Int16,
    "int32" => Int32,
    "int64" => Int64,
    # We need some special handling to get char arrays appearing as strings
    # via StaticStrings with zero allocations.
    "char" => ByteChar,
)

abstract type AbstractMessage end
abstract type CompositeDType end
abstract type VarLenDType <: AbstractVector{UInt8} end

function schemainfo end
function templateinfo end
function blockLength end

blockLength(T::Type{<:Any}) = sizeof(T)
# blockLength(T::Type{<:VarLenDType}) = error("blockLength for variable length type fell back to generic implementation. Missing method!")
function blockLength(T::Type{<:VarLenDType})
    error("blockLength for variable length type fell back to generic implementation. Missing method!")
end


function Base.parent(sbe::AbstractMessage)
    return getfield(sbe, :buffer)
end
function Base.parent(sbe::CompositeDType)
    return getfield(sbe, :buffer)
end
function parent_occupied(sbe::AbstractMessage)
    return view(getfield(sbe, :buffer), 1:sizeof(sbe))
end
function parent_occupied(sbe::CompositeDType)
    return view(getfield(sbe, :buffer), 1:sizeof(sbe))
end


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

    # Ensure that if the schema changes, any calling code is invalidated and recompiles
    @eval Mod Base.include_dependency($filename)

    @eval Mod if !@isdefined _sbe_message_id_map
        const _sbe_message_id_map = Dict{Tuple{Int,Int},Symbol}()
        const _sbe_message_id_map_type = Dict{Tuple{Int,Int},Any}()
    end

    schema_info_nt = (;
        package=attribute(xroot, "package"),
        id=parse(Int, attribute(xroot, "id")),
        version=parse(Int, attribute(xroot, "version")),
        semanticVersion=attribute(xroot, "semanticVersion"),
        description=attribute(xroot, "description"),
        byteOrder=attribute(xroot, "byteOrder")
    )
    dtype_map = primitive_type_map
    # traverse all its child nodes and print element names
    for e in child_elements(xroot)  # c is an instance of XMLNode
        if name(e) == "include"
            # @info "including linked file"
            href = attribute(e, "href")
            new_dtypes_map = load_dtypes_once(Mod, joinpath(dirname(filename), href))
            dtype_map = merge(dtype_map, new_dtypes_map)
        end
        if name(e) == "types"
            new_dtypes_map = load_dtypes(Mod, e)
            dtype_map = merge(dtype_map, new_dtypes_map)
        end
        if name(e) == "message"
            message_name = attribute(e, "name")
            message_description = attribute(e, "description")
            # Create some accessor functions to query the schemaId, version etc.
            template_info_nt = (;
                name=message_name,
                id=parse(Int, attribute(e, "id")),
                description=message_description
            )
            # @info "message type" message_name message_description
            fields = parse_message(e, dtype_map)
            T = generate_message_type(Mod, message_name, message_description, schema_info_nt, template_info_nt, fields)
            @eval Mod _sbe_message_id_map[($(schema_info_nt.id),$(template_info_nt.id))] = $(Meta.quot(Symbol(message_name)))
            @eval Mod _sbe_message_id_map_type[($(schema_info_nt.id),$(template_info_nt.id))] = $T
        end
    end
    free(xdoc)

    # Define a function for them that allows for flexible decoding based 
    # on the message header id parameter.
    @eval Mod if !@isdefined sbedecode
        """
        decode(data::Vector{UInt8})::AbstractMessage

        Given a data buffer, return an AbstractMessage sub-type that wraps it (zero-copy)
        and provides access to the underlying fields.

        `SimpleBinaryEncoding.evalschema` must be called first to define one or more 
        message types.

        This function is type-unstable since it looks up the correct message type by
        the message ID. For type-stable access, just use the constructors defined
        from the schema. E.g. if your schema defines
        ```xml
        <sbe:message name="ImageMessage" id="1" description="">
        </sbe:message>
        ```
        then simply call `evalschema` once, and then use `ImageMessage(data::Vector{UInt8})`.

        """
        function sbedecode(buffer::AbstractArray{UInt8})
            head = messageHeader(buffer)
            schemaId = head.schemaId
            templateId = head.templateId
            return _sbe_message_id_map_type[(schemaId,templateId)](buffer)
            # TODO: check id matches in each contructor when wrapping a message.
            # TODO: check schema id matches.
        end
        function sbemessagename(buffer::AbstractArray{UInt8})
            head = messageHeader(buffer)
            schemaId = head.schemaId
            templateId = head.templateId
            return _sbe_message_id_map[(schemaId,templateId)]
        end
    end

    nothing
end
export evalschema

# We only need to include external files once for any given module we evaluate
# a schema into. Otherwise we will end up re-defining types that are included
# by multiple message schemas.
mod_dtype_map_map = Dict{Tuple{Module,String},Dict}()
function load_dtypes_once(Mod, href)
    key = (Mod, href)
    if haskey(mod_dtype_map_map, key)
        return mod_dtype_map_map[key]
    else
        xdoc = parse_file(href)
        @eval Mod Base.include_dependency($href)
        xroot = root(xdoc)
        return mod_dtype_map_map[key] = load_dtypes(Mod, xroot)
    end
end
function load_dtypes(Mod, xroot)

    type_map = Dict{String,Any}()

    # traverse all its child nodes and print element names
    for e in child_elements(xroot)  # c is an instance of XMLNode
        if name(e) == "type" || name(e) == "composite"
            # Look up primitive type 
            if name(e) == "type"
                T = primitive_type_map[attribute(e, "primitiveType")]
                # Or check if we are defining a new composite type
            elseif name(e) == "composite"
                fields = parse_composite_type(e)
                if any(field -> field.name == "varData", fields)
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
            # @info "" name =attribute(e, "name") T
        end
        if name(e) == "enum"
            T = make_enum_type(Mod, e)
            type_map[attribute(e, "name")] = T
        end
    end
    return type_map
end

# Accepts message XML element
function parse_message(e, type_map)
    fields = map(child_elements(e)) do field_element
        field_name = attribute(field_element, "name")
        field_description = attribute(field_element, "description")
        field_type = attribute(field_element, "type")
        if has_attribute(field_element, "length")
            field_length = parse(Int,attribute(field_element, "length"))
        else
            field_length = nothing
        end
        if !haskey(type_map, field_type)
            error("Data type \"$field_type\" not recognized. Valid values are $(join(keys(type_map), ','))")
        end
        return (; name=field_name, description=field_description, type=type_map[field_type], length=field_length)
    end
    return fields
end

# TODO: this is redundant with the above?
function parse_composite_type(element)
    fields = map(child_elements(element)) do field_element
        field_name = attribute(field_element, "name")
        field_type_name = attribute(field_element, "primitiveType")
        field_description = attribute(field_element, "description")
        field_type = primitive_type_map[field_type_name]
        if has_attribute(field_element, "length")
            field_length = parse(Int, attribute(field_element, "length"))
                if field_length > 0
                    field_type = NTuple{field_length,field_type}
                else
                    # @info "fields" field_name field_type field_length
                    # error("unsupported length field $field_length")
                end
        else
            field_length = nothing
        end
        return (; name=field_name, type=field_type, description=field_description, length=field_length)
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

    @eval Mod begin
        # Put description field into docstring
        @doc $type_description
        struct $(Symbol(type_name)){T<:AbstractArray{UInt8}} <: $(CompositeDType)
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

    # There are two sizes we need to keep track of. The `blockLength`
    # which does not include any variable length data, and the
    # `sizeof` which does. The former can be calculated statically
    # but the latter must be dynamic, as it will depend on field 
    # values.

    # We build up a list of expressions calculating offsets as we go.
    # Each subsequent field adds their calculation to the offsets as a 
    # new expression.
    blocklen_exprs = Expr[:(blocklen = 0)]
    sizeof_offset_exprs = Expr[:(offset = 0)]
    for field in fields
        DType = field.type
        push!(blocklen_exprs, :(blocklen += $(blockLength)($DType)))
        dynamic_offset = DType <: CompositeDType || DType <: VarLenDType
        offset_calc_expr = if dynamic_offset
            quote
                len = sizeof(sbe.$(Symbol(field.name)))
                offset += len
            end
        else
            quote
                offset += sizeof($(DType))
            end
        end
        push!(sizeof_offset_exprs, offset_calc_expr)
    end
    getprop_exprs = map(enumerate(fields)) do (i, field)
        DType = field.type
        # Create an expression for accessing this property 
        return quote
            $(sizeof_offset_exprs[i])
            if prop == $(Meta.quot(Symbol(field.name)))
                # $(sizeof_offset_exprs[1:i]...) # Just interpolate in offsets for fields passed so far
                $(
                    # If we have a char array, return it as a StaticString.
                    if DType <: NTuple{N,ByteChar} where {N}
                        BytesType = NTuple{tuple_len(DType),UInt8}
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($BytesType);
                            return @inline $(NullTermString)(view(buffer, offset+1:offset+$(sizeof(BytesType))))
                        )
                    # elseif DType <: NTuple{N,T} where {N,T}
                    #     :(
                    #         # @show offset+1:offset+sizeof($BytesType);
                    #         return reinterpret($(eltype(DType)), view(buffer, offset+1:offset+sizeof($(DType))))
                    #     )
                    # If we have a variable length field, return it directly?
                    elseif DType <: CompositeDType 
                        # @info "composite field"
                        :(
                            # Debug:    
                            # @show "B" offset+1:offset+sizeof($BytesType);
                            # When constructing a composite field, just pass in the remainder of the buffer.
                            # We don't necessarily know how long it is, so we trust it not to touch 
                            # past it's own Base.sizeof(data::DType) which may be computed dynamically
                            # e.g. for variable length data.
                            return @inline $(DType)(view(buffer, offset+1:length(buffer)))
                        )
                    elseif  DType <: VarLenDType
                        :(
                            return @inline $(DType)(view(buffer, offset+1:length(buffer)))
                            # return @inline view(buffer, offset+1:length(buffer))
                        )
                        # Otherwise just directly reinterpret
                    else
                        :(
                            # Debug:    
                            # @show "C" offset+1:offset+sizeof($DType) $DType;
                            dat = @inline view(buffer, offset+1:offset+sizeof($(DType)));
                            return @inline reinterpret($(DType), dat)[];
                        )
                    end
                )
            end
        end
    end

    @eval Mod Base.@constprop :aggressive @inline function Base.getproperty(sbe::$(Symbol(type_name)), prop::Symbol)
        buffer = getfield(sbe, :buffer)
        # @info "getting property"
        $(getprop_exprs...)
        error(lazy"type has no property $prop")
    end

    setprop_exprs = map(enumerate(fields)) do (i, field)
        DType = field.type
        expr = quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(sizeof_offset_exprs[1:i]...) # Just interpolate in offsets for fields passed so far
                $(
                    # If we have a char array, return it as a StaticString.
                    if DType <: NTuple{N,ByteChar} where {N}
                        L = tuple_len(DType)
                        BytesType = NTuple{L,UInt8}
                        :(
                            # @show offset+1:offset+sizeof($BytesType);
                            return reinterpret($BytesType, view(buffer, offset+1:offset+sizeof($(BytesType))))[] = $(CStaticString){$L}(value)
                        )
                        # If we have a variable length field, return it directly?
                    elseif DType <: CompositeDType || DType <: VarLenDType
                        # @info "composite field"
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($BytesType);
                            # When constructing a composite field, just pass in the remainder of the buffer.
                            # We don't necessarily know how long it is, so we trust it not to touch 
                            # past it's own Base.sizeof(data::DType) which may be computed dynamically
                            # e.g. for variable length data.
                            if eltype(value) != UInt8
                                error("You can only set variable length fields to an array of UInt8")
                            end;
                            resize!(sbe.$(Symbol(field.name)), length(value));
                            sbe.$(Symbol(field.name)) .= value;
                            return sbe.$(Symbol(field.name))
                        )
                        # Otherwise just directly reinterpret
                    else
                        :(
                            # @show offset+1:offset+sizeof($DType);
                            return @inline reinterpret($(DType), view(buffer, offset+1:offset+sizeof($(DType))))[] = value
                        )
                    end
                )
            end
        end
        return expr
    end
    @eval Mod Base.@constprop :aggressive @inline function Base.setproperty!(sbe::$(Symbol(type_name)), prop::Symbol, value)
        buffer = getfield(sbe, :buffer)
        $(setprop_exprs...)
        error(lazy"type has no property $prop")
    end

    @eval Mod function Base.sizeof(sbe::$(Symbol(type_name)))
        $(sizeof_offset_exprs...)
        return offset
    end

    # We don't count the size of the messageHeader in the blockLength of an overall header+message.
    if type_name == "messageHeader"
        @eval Mod $(SimpleBinaryEncoding).blockLength(::Type{<:$(Symbol(type_name))}) = 0
    else
        @eval Mod function $(SimpleBinaryEncoding).blockLength(::Type{<:$(Symbol(type_name))})
            $(blocklen_exprs...)
            return blocklen
        end
    end

    # Display it all nicely at REPL
    @eval Mod function Base.show(io::IO, ::MIME"text/plain", sbe::$(Symbol(type_name)){T}) where {T}
        println(io, $(Symbol(type_name)), " view over a $T")
        maxproplen = 0
        for prop in propertynames(sbe)
            len = length(string(prop))
            if len > maxproplen
                maxproplen = len
            end
        end
        for prop in propertynames(sbe)
            s = string(prop)
            len = length(s)
            println(io, s, " "^(maxproplen-len), " = ", repr(getproperty(sbe, prop), context=:compact => true))
        end
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
    # @info "Defining variable length type" type_name type_description fields

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
        @doc $type_description
        struct $(Symbol(type_name)){B<:AbstractArray{UInt8}} <: $(VarLenDType)
            buffer::B
        end
    end

    # The block length is only the length of the fixed "length" parameter type and 
    # doesn't include the variable length component (wheras `sizeof` does include the
    # variable length component).
    @eval Mod $(SimpleBinaryEncoding).blockLength(::Type{<:$(Symbol(type_name))}) = 0 # $(sizeof(lenfield.type))

    @eval Mod Base.@constprop :aggressive @inline function Base.getproperty(sbe::$(Symbol(type_name)), prop::Symbol)
        buffer = getfield(sbe, :buffer)
        if prop == :data
            start = $(sizeof(lenfield.type))
            len = Int(reinterpret($(lenfield.type), view(getfield(sbe, :buffer), 1:start))[])
            return view(getfield(sbe, :buffer), start+1:start+len)
        end
        error(lazy"type has no property $prop")
    end


    @eval Mod @inline function Base.sizeof(sbe::$(Symbol(type_name)))
        return $(sizeof(lenfield.type)) + length(sbe)
    end

    @eval Mod @inline function Base.length(sbe::$(Symbol(type_name)))
        return Int(reinterpret($(lenfield.type), view(getfield(sbe, :buffer), 1:$(sizeof(lenfield.type))))[])
    end

    # To set the size of the buffer, we implement Base.resize.
    # We just change the length field of the composite type.
    # We do check that the buffer is sufficiently large but 
    # if the user adjusts the buffer smaller and it can no longer
    # accomodate the size of this varData, then they will get
    # an out of bounds error when they try to access it.
    @eval Mod function Base.resize!(sbe::$(Symbol(type_name)), len)
        len_needed = len + $(sizeof(lenfield.type))
        if len < 0
            error("Cannot have a negative length")
        elseif len_needed > length(getfield(sbe, :buffer))
            error(lazy"Backing buffer is too small to accommodate this data. Backing buffer requires an additional $(len_needed - length(getfield(sbe, :buffer))) bytes of storage.")
        end
        return reinterpret($(lenfield.type), view(getfield(sbe, :buffer), 1:$(sizeof(lenfield.type))))[] = len
    end

    # Display it all nicely at REPL
    @eval Mod function Base.show(io::IO, mime::MIME"text/plain", sbe::$(Symbol(type_name)){T}) where {T}
        println(io, $(Symbol(type_name)), " variable length view over a $T")
        println(io, "length = ", length(sbe))
        print(io, "data   = ")
        show(io, sbe.data)
    end

    return @eval Mod $(Symbol(type_name))

end


"""
Internal.
Generate a struct that wraps a byte buffer and has an interface
as described in a schema file as variable length encoded data.
"""
function make_enum_type(Mod, element)
    type_name = attribute(element, "name")
    type_description = attribute(element, "description")
    encoding_type = attribute(element, "encodingType")
    T = primitive_type_map[encoding_type]

    validValues = map(child_elements(element)) do value_element
        name = attribute(value_element, "name")
        # Parse the value in a format that can losslessly hold any int or uint 64
        value = parse(Int128,LightXML.content(first(child_nodes(value_element))))
        return (; name, value)
    end
    member_exprs = map(validValues) do (;name, value)
        return :($(Symbol(name))=$value)
    end
    
    @eval Mod begin
        # Put description field into docstring
        @doc $type_description
        @enum $(Symbol(type_name))::$T $(member_exprs...)
    end
    
    return @eval Mod $(Symbol(type_name))
end



"Return the length of a tuple (statically) given its type"
tuple_len(::Type{<:NTuple{N,Any}}) where {N} = N

"""
Internal.
Generate a struct that wraps a byte buffer and has an interface
as described in a schema file 
"""
function generate_message_type(Mod, message_name, message_description, schema_info_nt, template_info_nt, fields)
    # @info "generate struct " message_name fields

    @eval Mod begin
        # Put description field into docstring
        @doc $message_description
        struct $(Symbol(message_name)){T<:AbstractArray{UInt8}} <: $(SimpleBinaryEncoding.AbstractMessage)
            buffer::T
            # Write a constructor that, after initialization, fills in the message header appropriately
            # If `initialize` is true, fill in schema etc. If initialize is false, check that data matches.
            # If `initialize` is nothing, check if data is not all zero and assume initialize=true if so.
            function $(Symbol(message_name)){BufferType}(buffer; initialize::Union{Nothing,Bool}=nothing) where BufferType
                msg = new{BufferType}(buffer)
                Msg = typeof(msg)
                if isnothing(initialize)
                    doinit = (
                        msg.messageHeader.schemaId ==
                        msg.messageHeader.templateId ==
                        msg.messageHeader.blockLength == 0
                    )
                else
                    doinit = initialize
                end
                # Set up header
                if doinit
                    msg.messageHeader.schemaId = SimpleBinaryEncoding.schemainfo(Msg).id
                    msg.messageHeader.templateId = SimpleBinaryEncoding.templateinfo(Msg).id
                    msg.messageHeader.blockLength = SimpleBinaryEncoding.blockLength(Msg)
                
                # This error check is not actually helpful; sometimes we want to wrap data in a message without it matching.
                # else
                #     if (
                #         msg.messageHeader.schemaId != SimpleBinaryEncoding.schemainfo(Msg).id ||
                #         msg.messageHeader.templateId != SimpleBinaryEncoding.templateinfo(Msg).id ||
                #         msg.messageHeader.blockLength != SimpleBinaryEncoding.blockLength(Msg)
                #     )
                #         error(string($(Symbol(message_name), " was initialized with schemaId, templateId, or blockLength not matching. Set `initialize=true` to override.")))
                #     end
                end
                return msg
            end
            function $(Symbol(message_name))(buffer; initialize=nothing)
                msg = $(Symbol(message_name)){typeof(buffer)}(buffer; initialize)
                return msg
            end
        end
    end

    # We put a message header before our payload to indicate its type and nominal size.
    # Fake it by making it the first "field" of our struct.
    fields = vcat([(; name="messageHeader", type=@eval(Mod, messageHeader), description="", length=nothing)], fields)

    # For autocomplete etc.
    @eval Mod function Base.propertynames(sbe::$(Symbol(message_name)))
        props = ($(
            (Meta.quot(Symbol(field.name)) for field in fields
            )...),)
        return props
    end

    # For property access
    # construct expressions for each field access 

    # We build up a list of expressions calculating offsets as we go.
    # Each subsequent field adds their calculation to the offsets as a 
    # new expression.
    blocklen_exprs = Expr[:(blocklen = 0)]
    sizeof_offset_exprs = Expr[:(offset = 0)]

    fields = map(fields) do field
        DType = field.type
        len = field.length
        if isnothing(len)
        elseif len > 0
            DType = NTuple{len,DType}
        else
            error("unsupported length field $len")
        end
        return (;field..., type=DType)
    end

    for field in fields
        DType = field.type
        push!(blocklen_exprs, :(blocklen += $(blockLength)($DType)))
        dynamic_offset = DType <: CompositeDType || DType <: VarLenDType
        offset_calc_expr = if dynamic_offset
            quote
                # the rest of the buffer, then query it's length
                varlen_or_composite_element = $(DType)(view(buffer, offset+1:length(buffer)))
                len = sizeof(varlen_or_composite_element)
                offset += len
            end
        else
            quote
                offset += sizeof($(DType))
            end
        end
        push!(sizeof_offset_exprs, offset_calc_expr)
    end # This is the problem! it's recursive unless inlined!

    # The offset to each field must be calculated dynamically at run-
    # time due to the precense of variable length fields.
    # We build up a list of expressions calculating offsets as we go.
    # Each subsequent field adds their calculation to the offsets as a 
    # new expression.
    getprop_exprs = map(enumerate(fields)) do (i, field)
        DType = field.type
        # Create an expression for accessing this property 
        return quote
            $(sizeof_offset_exprs[i])
            if prop == $(Meta.quot(Symbol(field.name)))
                # $(sizeof_offset_exprs[1:i]...) # Just interpolate in offsets for fields passed so far
                $(
                    # If we have a char array, return it as a StaticString.
                    if DType <: NTuple{N,ByteChar} where {N}
                        BytesType = NTuple{tuple_len(DType),UInt8}
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($BytesType);
                            return @inline $(NullTermString)(view(buffer, offset+1:offset+$(sizeof(BytesType))))
                        )
                    # elseif DType <: NTuple{N,T} where {N,T}
                    #     :(
                    #         # @show offset+1:offset+sizeof($BytesType);
                    #         return reinterpret($(eltype(DType)), view(buffer, offset+1:offset+sizeof($(DType))))
                    #     )
                    # If we have a variable length field, return it directly?
                    elseif DType <: CompositeDType
                        # @info "composite field"
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($BytesType);
                            # When constructing a composite field, just pass in the remainder of the buffer.
                            # We don't necessarily know how long it is, so we trust it not to touch 
                            # past it's own Base.sizeof(data::DType) which may be computed dynamically
                            # e.g. for variable length data.
                            return @inline $(DType)(view(buffer, offset+1:length(buffer)))
                        )
                    elseif DType <: VarLenDType
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($BytesType);
                            # When constructing a composite field, just pass in the remainder of the buffer.
                            # We don't necessarily know how long it is, so we trust it not to touch 
                            # past it's own Base.sizeof(data::DType) which may be computed dynamically
                            # e.g. for variable length data.
                            return @inline $(DType)(view(buffer, offset+1:length(buffer)))
                            # return @inline view(buffer, offset+1:length(buffer))
                        )
                        # Otherwise just directly reinterpret
                    else
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($DType) $DType;
                            dat = @inline view(buffer, offset+1:offset+sizeof($(DType)));
                            return @inline reinterpret($(DType), dat)[];
                        )
                    end
                )
            end
        end
    end

    @eval Mod Base.@constprop :aggressive @inline function Base.getproperty(sbe::$(Symbol(message_name)), prop::Symbol)
        buffer = getfield(sbe, :buffer)
        # @info "getting property"
        $(getprop_exprs...)
        error(lazy"type has no property $prop")
    end

    @eval Mod function Base.sizeof(sbe::$(Symbol(message_name)))
        buffer = getfield(sbe, :buffer)
        $(sizeof_offset_exprs...)
        return offset
    end
    setprop_exprs = map(enumerate(fields)) do (i, field)
        DType = field.type
        expr = quote
            if prop == $(Meta.quot(Symbol(field.name)))
                $(sizeof_offset_exprs[1:i]...) # Just interpolate in offsets for fields passed so far
                $(
                    # If we have a char array, return it as a StaticString.
                    if DType <: NTuple{N,ByteChar} where {N}
                        L = tuple_len(DType)
                        BytesType = NTuple{L,UInt8}
                        :(
                            # @show offset+1:offset+sizeof($BytesType);
                            return reinterpret($BytesType, view(buffer, offset+1:offset+sizeof($(BytesType))))[] = $(CStaticString){$L}(value)
                        )
                        # If we have a variable length field, return it directly?
                    elseif DType <: CompositeDType || DType <: VarLenDType
                        # @info "composite field"
                        :(
                            # Debug:    
                            # @show offset+1:offset+sizeof($BytesType);
                            # When constructing a composite field, just pass in the remainder of the buffer.
                            # We don't necessarily know how long it is, so we trust it not to touch 
                            # past it's own Base.sizeof(data::DType) which may be computed dynamically
                            # e.g. for variable length data.
                            if eltype(value) != UInt8
                                error("You can only set variable length fields to an array of UInt8")
                            end;
                            resize!(sbe.$(Symbol(field.name)), length(value));
                            sbe.$(Symbol(field.name)) .= value;
                            return sbe.$(Symbol(field.name))
                        )
                        # Otherwise just directly reinterpret
                    else
                        :(
                            # @show offset+1:offset+sizeof($DType);
                            return @inline reinterpret($(DType), view(buffer, offset+1:offset+sizeof($(DType))))[] = value
                        )
                    end
                )
            end
        end
        return expr
    end
    @eval Mod Base.@constprop :aggressive @inline function Base.setproperty!(sbe::$(Symbol(message_name)), prop::Symbol, value)
        buffer = getfield(sbe, :buffer)
        $(setprop_exprs...)
        error(lazy"type has no property $prop")
    end

    # Create some accessor functions to query the schemaId, version etc.
    @eval Mod $(SimpleBinaryEncoding).schemainfo(::Type{<:$(Symbol(message_name))}) = $schema_info_nt
    @eval Mod $(SimpleBinaryEncoding).schemainfo(sbe::$(Symbol(message_name))) = $(SimpleBinaryEncoding).schemainfo(typeof(sbe))


    @eval Mod $(SimpleBinaryEncoding).templateinfo(::Type{<:$(Symbol(message_name))}) = $template_info_nt
    @eval Mod $(SimpleBinaryEncoding).templateinfo(sbe::$(Symbol(message_name))) = $(SimpleBinaryEncoding).templateinfo(typeof(sbe))

    @eval Mod function $(SimpleBinaryEncoding).blockLength(::Type{<:$(Symbol(message_name))})
        $(blocklen_exprs...)
        return blocklen
    end

    # Display it all nicely at REPL
    @eval Mod function Base.show(io::IO, mime::MIME"text/plain", sbe::$(Symbol(message_name)){T}) where {T}

        # display header first
        Base.show(io, mime, sbe.messageHeader)
        println()

        println(io, $(Symbol(message_name)), " view over a $T")
        maxproplen = 0
        pns = propertynames(sbe)[2:end]
        for prop in pns
            len = length(string(prop))
            if len > maxproplen
                maxproplen = len
            end
        end
        for prop in pns
            s = string(prop)
            len = length(s)
            val = getproperty(sbe, prop)
            if isbits(val)
                println(io, s, " "^(maxproplen-len), " = ", repr(val, context=:compact => true))
            else
                println(io, s,  " "^(maxproplen-len), " = ")
                Base.show(io, mime, val)
                println()
            end
        end
    end

    return @eval Mod $(Symbol(message_name))
end


end;