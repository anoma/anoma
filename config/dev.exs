import Config

# refresh the open api spec during development
config :open_api_spex, :cache_adapter, OpenApiSpex.Plug.NoneCache
