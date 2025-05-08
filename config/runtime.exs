import Config

# ----------------------------------------------------------------------------
# Endpoint

# Configures the endpoint
config :anoma_local_domain, Anoma.LocalDomain.Web.Endpoint,
  http: [
    ip: {127, 0, 0, 1},
    port: String.to_integer(System.get_env("CLIENT_HTTP_PORT") || "4000")
  ]

config :anoma_local_domain,
  grpc_port: String.to_integer(System.get_env("CLIENT_GRPC_PORT") || "40051")
