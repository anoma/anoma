defmodule Protobufs.Announcement do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field(:node_info, 1, type: Protobufs.NodeInfo, json_name: "nodeInfo")
  field(:engines, 2, repeated: true, type: :string)
end

defmodule Protobufs.NodeInfo do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field(:sign, 1, type: :bytes)
  field(:encrypt, 2, type: :bytes)
end

defmodule Protobufs.IntentPool.AddIntent.Request do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.IntentPool.AddIntent.Response do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field(:result, 1, type: :string)
end

defmodule Protobufs.IntentPool.AddIntent do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.IntentPool.ListIntents.Request do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.IntentPool.ListIntents.Response do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field(:intents, 1, repeated: true, type: :string)
end

defmodule Protobufs.IntentPool.ListIntents do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.IntentPool do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.Indexer.Nullifiers.Request do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.Indexer.Nullifiers.Response do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field(:nullifiers, 1, repeated: true, type: :string)
end

defmodule Protobufs.Indexer.Nullifiers do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.Indexer.UnrevealedCommits.Request do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.Indexer.UnrevealedCommits.Response do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field(:commits, 1, repeated: true, type: :string)
end

defmodule Protobufs.Indexer.UnrevealedCommits do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.Indexer.UnspentResources.Request do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.Indexer.UnspentResources.Response do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field(:unspent_resources, 1,
    repeated: true,
    type: :string,
    json_name: "unspentResources"
  )
end

defmodule Protobufs.Indexer.UnspentResources do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.Indexer do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.MemPool.Dump.Request do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.MemPool.Dump.Response do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field(:dumps, 1, repeated: true, type: :string)
end

defmodule Protobufs.MemPool.Dump do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.MemPool do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"
end

defmodule Protobufs.Enveloppe do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  oneof(:inner_message, 0)

  field(:sender_info, 1, type: Protobufs.NodeInfo, json_name: "senderInfo")
  field(:message_id, 2, type: :bytes, json_name: "messageId")
  field(:announcement, 3, type: Protobufs.Announcement, oneof: 0)

  field(:list_intents_request, 4,
    type: Protobufs.IntentPool.ListIntents.Request,
    json_name: "listIntentsRequest",
    oneof: 0
  )

  field(:list_intents_response, 5,
    type: Protobufs.IntentPool.ListIntents.Response,
    json_name: "listIntentsResponse",
    oneof: 0
  )

  field(:add_intent_request, 6,
    type: Protobufs.IntentPool.AddIntent.Request,
    json_name: "addIntentRequest",
    oneof: 0
  )

  field(:add_intent_response, 7,
    type: Protobufs.IntentPool.AddIntent.Response,
    json_name: "addIntentResponse",
    oneof: 0
  )

  field(:nullifiers_request, 8,
    type: Protobufs.Indexer.Nullifiers.Request,
    json_name: "nullifiersRequest",
    oneof: 0
  )

  field(:nullifiers_response, 9,
    type: Protobufs.Indexer.Nullifiers.Response,
    json_name: "nullifiersResponse",
    oneof: 0
  )

  field(:unrevealed_commits_request, 10,
    type: Protobufs.Indexer.UnrevealedCommits.Request,
    json_name: "unrevealedCommitsRequest",
    oneof: 0
  )

  field(:unrevealed_commits_response, 11,
    type: Protobufs.Indexer.UnrevealedCommits.Response,
    json_name: "unrevealedCommitsResponse",
    oneof: 0
  )

  field(:unspent_resources_request, 12,
    type: Protobufs.Indexer.UnspentResources.Request,
    json_name: "unspentResourcesRequest",
    oneof: 0
  )

  field(:unspent_resources_response, 13,
    type: Protobufs.Indexer.UnspentResources.Response,
    json_name: "unspentResourcesResponse",
    oneof: 0
  )

  field(:mempool_dump_request, 14,
    type: Protobufs.MemPool.Dump.Request,
    json_name: "mempoolDumpRequest",
    oneof: 0
  )

  field(:mempool_dump_response, 15,
    type: Protobufs.MemPool.Dump.Response,
    json_name: "mempoolDumpResponse",
    oneof: 0
  )
end
