defmodule Anoma.System.Directories do
  @moduledoc """
  I provide out utilities for ensuring user Data, Config,
  etc. directories are properly setup for the host operating system.

  Please use me when trying to write to user directories.
  """

  @doc """
  I provide a proper translation from a user directory to the
  corresponding system data directory.

  ### Parameters

  - `file_path` - the path given by the user
  - `env` (optional) - the system environment. It defaults to the
    application environment. The `:test` environment uses the local
    directory instead of the system directory

  ### Returns

  The correct environment which to save data details

  """
  @spec data(Path.t()) :: Path.t()
  @spec data(Path.t(), atom()) :: Path.t()
  def data(file_path, env \\ Application.get_env(:anoma, :env)) do
    determine_path(file_path, :data, env)
  end

  @doc """
  I provide a proper translation from a user directory to the
  corresponding system configuration directory.

  ### Parameters

  - `file_path` - the path given by the user
  - `env` (optional) - the system environment. It defaults to the
    application environment. The `:test` environment uses the local
    directory instead of the system directory

  ### Returns

  The correct environment which to save configuration details

  """
  @spec configuration(Path.t()) :: Path.t()
  @spec configuration(Path.t(), atom()) :: Path.t()
  def configuration(file_path, env \\ Application.get_env(:anoma, :env)) do
    determine_path(file_path, :config, env)
  end

  @spec determine_path(Path.t(), atom(), atom()) :: Path.t()
  defp determine_path(file_path, type, env) do
    if env == :test do
      Path.relative(file_path)
    else
      Path.join(Application.get_env(:anoma, type), file_path)
    end
  end
end
