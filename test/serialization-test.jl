# DataType
serialization_stream() do s # standard types
    typ1 = UInt8
    serialize(s, typ1)
    typ2 = Bool
    serialize(s, typ2)

    seek(s, 0)
    @test deserialize(s) == typ1
    @test deserialize(s) == typ2
end

serialization_stream() do s # user-defined type
    usertype = "SomeType"
    utype = eval(Meta.parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) === utype
end

serialization_stream() do s # user-defined type
    usertype = "SomeType1"
    utype = eval(Meta.parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) === utype
end

serialization_stream() do s # user-defined type
    usertype = "SomeType2"
    utype = eval(Meta.parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) == utype
end
