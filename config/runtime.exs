import Config

# This configuration file is always evaluated at runtime. The other config files
# are evaluated at compile time.

if Mix.env() == :prod do
  # ----------------------------------------------------------------------------
  # Anoma Client Web Endpoint

  config :anoma_client, Anoma.Client.Web.Endpoint,
    http: [
      ip: {127, 0, 0, 1},
      port: String.to_integer(System.get_env("CLIENT_HTTP_PORT") || "4001")
    ]

  # ----------------------------------------------------------------------------
  # Anoma Client

  config :anoma_client,
    grpc_port:
      String.to_integer(System.get_env("CLIENT_GRPC_PORT") || "40051")

  # ----------------------------------------------------------------------------
  # Anoma Node

  config :anoma_node,
    grpc_port: String.to_integer(System.get_env("NODE_GRPC_PORT") || "50051")
end
