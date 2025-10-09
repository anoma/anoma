import Config

# ----------------------------------------------------------------------------
# Endpoint

# Configures the endpoint
config :anoma_client, Anoma.Client.Web.Endpoint,
  http: [
    ip: {127, 0, 0, 1},
    port: String.to_integer(System.get_env("CLIENT_HTTP_PORT") || "4000")
  ]

config :anoma_client,
  grpc_port: String.to_integer(System.get_env("CLIENT_GRPC_PORT") || "40051")
