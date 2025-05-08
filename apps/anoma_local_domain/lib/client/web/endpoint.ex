defmodule Anoma.LocalDomain.Web.Endpoint do
  use Phoenix.Endpoint, otp_app: :anoma_local_domain

  socket("/socket", Anoma.LocalDomain.Web.Socket,
    websocket: true,
    longpoll: true
  )

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  @session_options [
    store: :cookie,
    key: "some_key",
    signing_salt: "hLgmudiI",
    same_site: "Laxx"
  ]

  plug(Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()
  )

  plug(Plug.MethodOverride)
  plug(Plug.Head)
  plug(Plug.Session, @session_options)
  plug(Anoma.LocalDomain.Web.Router)
end
