defmodule ProbnikQR do
  @moduledoc """
  Generate QR codes for pairing with the Probnik BEAM node monitor.

  ## Usage

  On the node you want to monitor, call:

      iex> ProbnikQR.show()

  This prints an ANSI ASCII QR code to the terminal. Scan it with the
  Probnik mobile app to establish a connection.

  The underlying implementation is in the Erlang module `:probnik_qr`,
  so Erlang and Elixir apps can share the same dependency.

  ProbnikQR is deliberately small. It exists to be forgotten: a quick pairing
  ritual that disappears into the muscle memory of starting a node, printing a
  QR, scanning it, and moving on to observe the system.

  ## Requirements

  The node must be started with distribution enabled:

      iex --sname myapp@localhost --cookie secret -S mix

  Or for long names:

      iex --name myapp@myhost.local --cookie secret -S mix
  """

  @doc """
  Print a QR code for pairing with Probnik.

  The QR encodes an Erlang term with the node name, cookie, and name mode:

      {probnik_pair, 'myapp@localhost', secret, [{mode, shortnames}]}

  ## Example

      iex> ProbnikQR.show()
      # Prints ASCII QR code
      :ok
  """
  def show do
    :probnik_qr.show()
  end

  @doc """
  Returns the pairing payload as a binary (for custom QR generation).
  """
  def payload do
    :probnik_qr.payload()
  end
end
