defmodule Mix.Tasks.Anoma.Doctor do
  use Mix.Task
  @shortdoc "Checks local toolchain: OTP/Elixir, protoc, plugins, Rust"

  @impl true
  def run(_args) do
    Mix.shell().info("==> Checking Erlang/Elixir")
    otp = :erlang.system_info(:otp_release) |> to_string()
    Mix.shell().info("OTP: #{otp}")
    Mix.shell().info(System.cmd("elixir", ["-v"]) |> elem(0))

    Mix.shell().info("==> Checking protoc & plugins")
    check!("protoc", "sudo apt install -y protobuf-compiler")
    escripts = System.user_home!() <> "/.mix/escripts"
    path = System.get_env("PATH", "")
    unless String.contains?(path, escripts), do:
      Mix.shell().info("Hint: export PATH=\"#{escripts}:\$PATH\"")

    check!("protoc-gen-elixir", "mix escript.install hex protobuf --force")

    Mix.shell().info("==> Checking Rust (cargo)")
    check!("cargo", "curl -sSf https://rustup.rs | sh -s -- -y && source ~/.cargo/env")

    Mix.shell().info("==> Mix deps/build/test (dry run)")
    run!("mix", ["local.hex", "--force"])
    run!("mix", ["local.rebar", "--force"])
    run!("mix", ["deps.get"])
    Mix.shell().info("OK. You can now run: mix compile && MIX_ENV=test mix test")
  end

  defp check!(exe, fix) do
    case System.find_executable(exe) do
      nil -> Mix.shell().error("[missing] #{exe}  →  fix: #{fix}")
      _ -> Mix.shell().info("[found]   #{exe}")
    end
  end

  defp run!(cmd, args) do
    {out, code} = System.cmd(cmd, args, stderr_to_stdout: true)
    Mix.shell().info(out)
    if code != 0, do: Mix.raise("#{cmd} #{Enum.join(args, " ")} failed")
  end
end
