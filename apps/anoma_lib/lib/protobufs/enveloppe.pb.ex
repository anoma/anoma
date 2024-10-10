defmodule Protobufs.Enveloppe do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "Enveloppe",
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
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "message_id",
          extendee: nil,
          number: 2,
          label: :LABEL_OPTIONAL,
          type: :TYPE_BYTES,
          type_name: nil,
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "messageId",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "announcement",
          extendee: nil,
          number: 3,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.Announcement",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "announcement",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "list_intents_request",
          extendee: nil,
          number: 4,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.IntentPool.ListIntents.Request",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "listIntentsRequest",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "list_intents_response",
          extendee: nil,
          number: 5,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.IntentPool.ListIntents.Response",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "listIntentsResponse",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "add_intent_request",
          extendee: nil,
          number: 6,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.IntentPool.AddIntent.Request",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "addIntentRequest",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "add_intent_response",
          extendee: nil,
          number: 7,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.IntentPool.AddIntent.Response",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "addIntentResponse",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "nullifiers_request",
          extendee: nil,
          number: 8,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.Indexer.Nullifiers.Request",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "nullifiersRequest",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "nullifiers_response",
          extendee: nil,
          number: 9,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.Indexer.Nullifiers.Response",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "nullifiersResponse",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "unrevealed_commits_request",
          extendee: nil,
          number: 10,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.Indexer.UnrevealedCommits.Request",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "unrevealedCommitsRequest",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "unrevealed_commits_response",
          extendee: nil,
          number: 11,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.Indexer.UnrevealedCommits.Response",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "unrevealedCommitsResponse",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "unspent_resources_request",
          extendee: nil,
          number: 12,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.Indexer.UnspentResources.Request",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "unspentResourcesRequest",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "unspent_resources_response",
          extendee: nil,
          number: 13,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.Indexer.UnspentResources.Response",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "unspentResourcesResponse",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "mempool_dump_request",
          extendee: nil,
          number: 14,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.MemPool.Dump.Request",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "mempoolDumpRequest",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "mempool_dump_response",
          extendee: nil,
          number: 15,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".Protobufs.MemPool.Dump.Response",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "mempoolDumpResponse",
          proto3_optional: nil,
          __unknown_fields__: []
        }
      ],
      nested_type: [],
      enum_type: [],
      extension_range: [],
      extension: [],
      options: nil,
      oneof_decl: [
        %Google.Protobuf.OneofDescriptorProto{
          name: "inner_message",
          options: nil,
          __unknown_fields__: []
        }
      ],
      reserved_range: [],
      reserved_name: [],
      __unknown_fields__: []
    }
  end

  oneof :inner_message, 0

  field :sender_info, 1, type: Protobufs.NodeInfo, json_name: "senderInfo"
  field :message_id, 2, type: :bytes, json_name: "messageId"
  field :announcement, 3, type: Protobufs.Announcement, oneof: 0

  field :list_intents_request, 4,
    type: Protobufs.IntentPool.ListIntents.Request,
    json_name: "listIntentsRequest",
    oneof: 0

  field :list_intents_response, 5,
    type: Protobufs.IntentPool.ListIntents.Response,
    json_name: "listIntentsResponse",
    oneof: 0

  field :add_intent_request, 6,
    type: Protobufs.IntentPool.AddIntent.Request,
    json_name: "addIntentRequest",
    oneof: 0

  field :add_intent_response, 7,
    type: Protobufs.IntentPool.AddIntent.Response,
    json_name: "addIntentResponse",
    oneof: 0

  field :nullifiers_request, 8,
    type: Protobufs.Indexer.Nullifiers.Request,
    json_name: "nullifiersRequest",
    oneof: 0

  field :nullifiers_response, 9,
    type: Protobufs.Indexer.Nullifiers.Response,
    json_name: "nullifiersResponse",
    oneof: 0

  field :unrevealed_commits_request, 10,
    type: Protobufs.Indexer.UnrevealedCommits.Request,
    json_name: "unrevealedCommitsRequest",
    oneof: 0

  field :unrevealed_commits_response, 11,
    type: Protobufs.Indexer.UnrevealedCommits.Response,
    json_name: "unrevealedCommitsResponse",
    oneof: 0

  field :unspent_resources_request, 12,
    type: Protobufs.Indexer.UnspentResources.Request,
    json_name: "unspentResourcesRequest",
    oneof: 0

  field :unspent_resources_response, 13,
    type: Protobufs.Indexer.UnspentResources.Response,
    json_name: "unspentResourcesResponse",
    oneof: 0

  field :mempool_dump_request, 14,
    type: Protobufs.MemPool.Dump.Request,
    json_name: "mempoolDumpRequest",
    oneof: 0

  field :mempool_dump_response, 15,
    type: Protobufs.MemPool.Dump.Response,
    json_name: "mempoolDumpResponse",
    oneof: 0
end