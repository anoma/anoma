defmodule SerialisedStruct do
  use TypedStruct

  defmacro __using__(_) do
    quote do
      use TypedStruct
      import SerialisedStruct, only: [deftypes: 1]
    end
  end

  defmodule ProtoField do
    @enforce_keys [:name, :type]
    defstruct [:name, :type, repeated: false, optional: false]
  end

  defmodule Proto do
    @enforce_keys [:mod, :elixir_type, :serialise, :deserialise]
    defstruct [:mod, :elixir_type, :serialise, :deserialise, :fields, :prim_proto_type, oneof: false]
  end

  defmodule StructField do
    @enforce_keys [:name, :type]
    defstruct [:name, :type, :optional, :default_p, :default]
  end

  defmodule Struct do
    @enforce_keys [:mod, :fields]
    defstruct [:mod, :fields, oneof: false]
  end

  defmodule Spec do
    # protos maps from a name to a Proto; structs maps from a name to a Struct
    # we have to emit definitions in order, hence proto/struct_list; these are in reverse order
    defstruct [protos: Map.new(), structs: Map.new(), proto_list: [], struct_list: []]
  end

  defp add_proto(spec, k, v) do
    %{spec |
      protos: Map.put(spec.protos, k, v),
      proto_list: [v | spec.proto_list]}
  end
  defp add_struct(spec, k, v) do
    %{spec |
      structs: Map.put(spec.structs, k, v),
      struct_list: [v | spec.struct_list]}
  end

  defmacro deftypes(do: block) do
    {:__block__, _loc, decls} = block
    spec = Enum.reduce(decls, %Spec{}, &parse_decl/2)
    quote do
      unquote_splicing(Enum.map(Enum.reverse(spec.struct_list), &emit_struct/1))
      unquote_splicing(Enum.map(Enum.reverse(spec.proto_list), &emit_proto/1))
    end
  end

  defp emit_proto(proto) do
    fields = proto.fields || []
    quote do
      defmodule unquote(proto.mod) do
        use Protobuf, syntax: :proto3
        unquote_splicing(
          if proto.oneof do
            [quote do oneof :alternatives, 0 end]
          else
            []
          end)
        unquote_splicing(
          # protobuf fields are numbered starting from 1
          Enum.map(Enum.zip(fields, 1..length(fields)),
            fn {field, i} ->
              quote do field unquote(field.name), unquote(i), unquote([repeated: field.repeated, type: field.type, proto3_optional: field.optional] ++ if proto.oneof do [oneof: 0] else [] end) end
            end))
      end
      defmodule unquote(proto.mod).Helper do
        def serialise(x) do
          unquote(proto.serialise).(x)
        end
        def deserialise(x) do
          unquote(proto.deserialise).(x)
        end
      end
    end
  end

  defp emit_struct(%Struct{mod: mod, fields: fields, oneof: oneof}) do
    quote do
      defmodule unquote(mod) do
        unquote(if oneof do
          quote do
            # mod1.t() | mod2.t() | ...
            @type t() :: unquote(Enum.reduce(Enum.map(fields, fn field -> field.type end),
              fn t1, t2 -> quote do unquote(t1) | unquote(t2) end end))
          end
          else
          quote do
            use TypedStruct
            typedstruct do
              unquote_splicing(Enum.map(fields,
                fn field -> quote do field(unquote(field.name), unquote(field.type),
                  unquote([{:enforce, !field.optional && !field.default_p}
                    | if field.default_p do [default: field.default] else [] end])) end end))
            end
          end
        end)
        def serialise(x) do
          unquote(mod).Proto.encode(
            unquote(mod).Proto.Helper.serialise(x))
        end
        def deserialise(x) do
          unquote(mod).Proto.Helper.deserialise(
            unquote(mod).Proto.decode(x))
        end
      end
    end
  end

  defp parse_decl({:proto_prim, _loc, body}, spec) do
    [mod, elixir_type, proto_type, serialise, deserialise] = body
    name = mod_name(mod)
    if Map.has_key?(spec.protos, name) do
      raise ArgumentError, "#{inspect(elixir_type)} multiply defined!"
    end
    add_proto(spec, name,
      %Proto{mod: mod, prim_proto_type: proto_type, elixir_type: elixir_type, serialise: serialise, deserialise: deserialise})
  end

  defp parse_decl({:proto, _loc, body}, spec) do
    [mod, elixir_type, serialise, deserialise, [do: block]] = body
    name = mod_name(mod)
    fields = block_contents(block)
    if Map.has_key?(spec.protos, name) do
      raise ArgumentError, "#{inspect(name)} multiply defined!"
    end
    add_proto(spec, name,
      %Proto{mod: mod, elixir_type: elixir_type, serialise: serialise, deserialise: deserialise,
        fields: Enum.map(fields, &parse_proto_field/1)})
  end
  defp parse_proto_field(field) do
    {:field, _loc, [name, type | options]} = field
    field = %ProtoField{name: name, type: type}
    Enum.reduce(options, field,
      fn option, field ->
        case option do
          :repeated -> %{field | repeated: true}
          :optional -> %{field | optional: true}
        end
      end)
  end

  defp parse_decl({:union, _loc, body}, spec) do
    [mod, [do: block]] = body
    name = mod_name(mod)
    fields = block_contents(block)
    if Map.has_key?(spec.structs, name) do
      raise ArgumentError, "#{inspect(mod)} multiply defined!"
    end
    fields = Enum.map(fields,
      fn {:field, _loc, [name, mod]} ->
        if not (Map.has_key?(spec.structs, mod_name(mod)) || Map.has_key?(spec.protos, mod_name(mod))) do
          raise ArgumentError, "#{inspect(mod)} not defined"
        end
        {name, mod}
      end)
    serialise = quote do fn x ->
      %unquote(mod).Proto{
        alternatives:
        unquote(case_form(quote do x.__struct__ end,
          Enum.map(fields, fn {name, mod} ->
            {mod, quote do {unquote(name), unquote(mod).Proto.Helper.serialise(x)} end}
            end)))}
    end
    end

    deserialise = quote do fn %unquote(mod).Proto{alternatives: {name, value}} ->
      unquote(case_form(quote do name end,
        Enum.map(fields,
          fn {oname, mod} ->
            {oname, quote do unquote(mod).Proto.Helper.deserialise(value) end}
          end)))
    end
    end

    types = Enum.map(fields, fn {_, mod} -> quote do unquote(mod).t() end end)
    type = Enum.reduce(types,
      fn t1, t2 -> quote do unquote(t1) | unquote(t2) end end)


    spec = add_proto(spec, name ++ [:Proto],
      %Proto{mod: quote do unquote(mod).Proto end, elixir_type: type, oneof: true,
      serialise: serialise, deserialise: deserialise,
      fields: Enum.map(fields, fn {name, mod} -> %ProtoField{name: name, type: quote do unquote(mod).Proto end} end)})
    spec = add_struct(spec, name,
      %Struct{mod: mod, fields: Enum.map(Enum.zip(types, fields), fn {type, {name, _}} -> %StructField{type: type, name: name} end), oneof: true})
    spec
  end

  defp parse_decl({:struct, _loc, body}, spec) do
    [mod, [do: block]] = body
    name = mod_name(mod)
    fields = block_contents(block)
    if Map.has_key?(spec.structs, name) do
      raise ArgumentError, "#{inspect(mod)} multiply defined!"
    end
    fields = Enum.map(fields, fn {:field, _loc, [name, type | options]} -> parse_field(spec, name, type, if options == [] do [] else hd(options) end) end)
    deserialise = quote do fn x ->
      %unquote(mod){
        unquote_splicing(
          Enum.map(fields, fn {%ProtoField{name: name, repeated: repeated, optional: optional}, _, _, deserialise} ->
            {name,
              cond do
                repeated -> quote do Enum.map(x.unquote(name), unquote(deserialise)) end
                optional -> quote do if x.unquote(name) == nil do nil else unquote(deserialise).(x.unquote(name)) end end
                true -> quote do unquote(deserialise).(x.unquote(name)) end
              end}
          end))}
    end
    end
    serialise = quote do fn x ->
      %unquote(mod).Proto{
        unquote_splicing(
          Enum.map(fields, fn {%ProtoField{name: name, repeated: repeated, optional: optional}, _, serialise, _} ->
            {name,
              cond do
                repeated -> quote do Enum.map(x.unquote(name), unquote(serialise)) end
                optional -> quote do if x.unquote(name) == nil do nil else unquote(serialise).(x.unquote(name)) end end
                true -> quote do unquote(serialise).(x.unquote(name)) end
              end}
          end))}
    end
    end

    spec = add_struct(spec, name, %Struct{mod: mod, fields: Enum.map(fields, &elem(&1, 1))})
    spec = add_proto(spec, name ++ [:Proto],
      %Proto{mod: quote do unquote(mod).Proto end, serialise: serialise, deserialise: deserialise, elixir_type: quote do unquote(mod).t() end,
      fields: Enum.map(fields, &elem(&1, 0))})
    spec
  end

  # returns a tuple {proto_field, struct_field, serialise, deserialise}
  defp parse_field(spec, name, type, options) do
    # only support one of repeated and optional, as I'm not sure if protobuf is up to handling both
    {type, repeated, optional} = case type do
      {:list, _loc, [type]} -> {type, true, false}
      {:optional, _loc, [type]} -> {type, false, true}
      type -> {type, false, false}
    end
    type = mod_name(type)
    type = if Map.has_key?(spec.structs, type) do type ++ [:Proto] else type end
    %Proto{mod: mod, elixir_type: elixir_type, prim_proto_type: prim_proto_type} = Map.get(spec.protos, type) || raise ArgumentError, "#{inspect(type)} not defined"
    elixir_type = if repeated do quote do list(unquote(elixir_type)) end else elixir_type end
    elixir_field = %StructField{name: name, type: elixir_type, optional: optional}
    elixir_field = Enum.reduce(options, elixir_field, fn option, field ->
      case option do
        {:default, default} -> %{field | default_p: true, default: default}
      end
    end)
    {%ProtoField{name: name, type: prim_proto_type || mod, repeated: repeated, optional: optional},
      elixir_field,
      quote do &unquote(mod).Helper.serialise/1 end, quote do &unquote(mod).Helper.deserialise/1 end}
  end

  # for some reason, do x end is completely different from do x \n y end
  # this papers over the differences
  defp block_contents({:__block__, _loc, fields}) do fields end
  defp block_contents(x) do [x] end

  # could not figure out how to express this nicely with unquote
  defp case_form(discriminator, cases) do
    {:case, [], [discriminator,
      [do: Enum.map(cases, fn {x, y} -> {:->, [], [[x], y]} end)]]}
  end

  defp mod_name({:__aliases__, opts, name}) do
    if Enum.any?(opts, fn x -> elem(x, 0) == :alias and elem(x, 1) != false end) do
      raise ArgumentError, "module #{inspect(name)} is aliased"
    end
    name
  end
end
