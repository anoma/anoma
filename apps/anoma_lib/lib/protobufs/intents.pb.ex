defmodule Protobufs.Intents.Service do
  @moduledoc false

  use GRPC.Service, name: "Protobufs.Intents", protoc_gen_elixir_version: "0.13.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      name: "Intents",
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          name: "ListIntents",
          input_type: ".Protobufs.IntentPool.ListIntents.Request",
          output_type: ".Protobufs.IntentPool.ListIntents.Response",
          options: %Google.Protobuf.MethodOptions{
            deprecated: false,
            idempotency_level: :IDEMPOTENCY_UNKNOWN,
            uninterpreted_option: [],
            __pb_extensions__: %{},
            __unknown_fields__: []
          },
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        },
        %Google.Protobuf.MethodDescriptorProto{
          name: "AddIntent",
          input_type: ".Protobufs.IntentPool.AddIntent.Request",
          output_type: ".Protobufs.IntentPool.AddIntent.Response",
          options: %Google.Protobuf.MethodOptions{
            deprecated: false,
            idempotency_level: :IDEMPOTENCY_UNKNOWN,
            uninterpreted_option: [],
            __pb_extensions__: %{},
            __unknown_fields__: []
          },
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        },
        %Google.Protobuf.MethodDescriptorProto{
          name: "ListNullifiers",
          input_type: ".Protobufs.Indexer.Nullifiers.Request",
          output_type: ".Protobufs.Indexer.Nullifiers.Response",
          options: %Google.Protobuf.MethodOptions{
            deprecated: false,
            idempotency_level: :IDEMPOTENCY_UNKNOWN,
            uninterpreted_option: [],
            __pb_extensions__: %{},
            __unknown_fields__: []
          },
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        },
        %Google.Protobuf.MethodDescriptorProto{
          name: "ListUnrevealedCommits",
          input_type: ".Protobufs.Indexer.UnrevealedCommits.Request",
          output_type: ".Protobufs.Indexer.UnrevealedCommits.Response",
          options: %Google.Protobuf.MethodOptions{
            deprecated: false,
            idempotency_level: :IDEMPOTENCY_UNKNOWN,
            uninterpreted_option: [],
            __pb_extensions__: %{},
            __unknown_fields__: []
          },
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        },
        %Google.Protobuf.MethodDescriptorProto{
          name: "ListUnspentResources",
          input_type: ".Protobufs.Indexer.UnspentResources.Request",
          output_type: ".Protobufs.Indexer.UnspentResources.Response",
          options: %Google.Protobuf.MethodOptions{
            deprecated: false,
            idempotency_level: :IDEMPOTENCY_UNKNOWN,
            uninterpreted_option: [],
            __pb_extensions__: %{},
            __unknown_fields__: []
          },
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        },
        %Google.Protobuf.MethodDescriptorProto{
          name: "Prove",
          input_type: ".Protobufs.Prove.Request",
          output_type: ".Protobufs.Prove.Response",
          options: %Google.Protobuf.MethodOptions{
            deprecated: false,
            idempotency_level: :IDEMPOTENCY_UNKNOWN,
            uninterpreted_option: [],
            __pb_extensions__: %{},
            __unknown_fields__: []
          },
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        }
      ],
      options: nil,
      __unknown_fields__: []
    }
  end

  rpc :ListIntents,
      Protobufs.IntentPool.ListIntents.Request,
      Protobufs.IntentPool.ListIntents.Response

  rpc :AddIntent, Protobufs.IntentPool.AddIntent.Request, Protobufs.IntentPool.AddIntent.Response

  rpc :ListNullifiers, Protobufs.Indexer.Nullifiers.Request, Protobufs.Indexer.Nullifiers.Response

  rpc :ListUnrevealedCommits,
      Protobufs.Indexer.UnrevealedCommits.Request,
      Protobufs.Indexer.UnrevealedCommits.Response

  rpc :ListUnspentResources,
      Protobufs.Indexer.UnspentResources.Request,
      Protobufs.Indexer.UnspentResources.Response

  rpc :Prove, Protobufs.Prove.Request, Protobufs.Prove.Response
end

defmodule Protobufs.Intents.Stub do
  @moduledoc false

  use GRPC.Stub, service: Protobufs.Intents.Service
end