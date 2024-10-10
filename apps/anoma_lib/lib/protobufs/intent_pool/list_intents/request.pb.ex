defmodule Protobufs.IntentPool.ListIntents.Request do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "Request",
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          name: "sender_info",
          extendee: nil,
          number: 1,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.NodeInfo",
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "senderInfo",
          proto3_optional: nil,
          __unknown_fields__: []
        }
      ],
      nested_type: [],
      enum_type: [],
      extension_range: [],
      extension: [],
      options: nil,
      oneof_decl: [],
      reserved_range: [],
      reserved_name: [],
      __unknown_fields__: []
    }
  end

  field :sender_info, 1, type: Protobufs.NodeInfo, json_name: "senderInfo"
end