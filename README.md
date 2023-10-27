# SimpleBinaryEncoding.jl
Serialization and deserialization to/from binary using the Simple-Binary-Encoding format:
https://github.com/real-logic/simple-binary-encoding

Message types are described by XML schemas. Running `evalschema(MyModule, "myschema.xml")` 
will create wrapper types. These types wrap a buffer of `UInt8` and allow one
to access and mutate it as if it were an object, or nested objects, following the
defintions of the schema.

Current status: basic functionality works but has not been tested outside of the schema formats
needed in our application.


## Examples

Load a schema:
```julia
evalschema(Main, "image.xml")
```

The first argument is the module in which to create the type definitions.

Create a backing byte buffer of at least the size needed for
the type:
```julia
buf1 = zeros(UInt8, 1000);
```

Instantiate the wrapper around this buffer:
```julia
img = Image(buf1)
```
The type name is generated from the schema file. No data copying occurs.
If the wrapper does not escape, no allocations should occur.

Access properties directly:
```julia
img.timestamp = 10
```

Variable length data is supported if defined in the schema. The buffer size must be large enough, or a bounds error will occur:
```julia
resize!(img.frameBuffer, 100*100)
img.frameBuffer .= rand(100*100)
```

Now let's send the buffer somewhere else. You could write it to a file, send it over the network, etc.
Let's try writing to a file and reading it back:
```julia
write("tmp.dat", buf)
```
Note that you could use Aeron.jl or shared memory with Mmap.jl to communicate this data between processes in a truly zero-allocation way.

Let's read it back and interpret it using the same wrapper type:
```julia
buf2 = read("tmp.dat")
img2 = Image(buf2)
```