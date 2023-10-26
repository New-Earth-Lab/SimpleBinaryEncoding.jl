# SimpleBinaryEncoding.jl
Serialization and deserialization to/from binary using the Simple-Binary-Encoding format:
https://github.com/real-logic/simple-binary-encoding

Message types are described by XML schemas. Running `evalschema(MyModule, "myschema.xml")` 
will create wrapper types. These types wrap a buffer of `UInt8` and allow one
to access and mutate it as if it were an object, or nested objects, following the
defintions of the schema.

Current status: basic functionality works but has not been tested outside of the schema formats
needed in our application.
