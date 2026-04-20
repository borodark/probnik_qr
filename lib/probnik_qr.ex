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

  The default payload uses local IPv4 address and port 4040.
  Override with environment variables for the net format:

      PROBNIKOFF_NET_HOST=192.168.0.249
      PROBNIKOFF_NET_PORT=4040

  ## Payload helpers

  If you want the raw payload (for custom QR rendering):

      iex> ProbnikQR.payload_net()
      {probnikoff_net, {{192, 168, 0, 249}, 4040}}

      iex> ProbnikQR.payload_pair()
      {probnik_pair, 'myapp@localhost', secret, [{mode, shortnames}]}

      1> probnik_qr:payload_net().
      {probnikoff_net, {{192, 168, 0, 249}, 4040}}

      2> probnik_qr:payload_pair().
      {probnik_pair, 'myapp@localhost', secret, [{mode, shortnames}]}
  """

  @doc """
  Print a QR code for pairing with Probnik.

  The QR encodes an Erlang term for remote rendering over TCP:

      {probnikoff_net, {{192, 168, 0, 249}, 4040}}

  For distributed Erlang pairing, use `show_pair/0` which encodes:

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
  Print a QR code for remote rendering over TCP (probnikoff_net).
  """
  def show_net do
    :probnik_qr.show_net()
  end

  @doc """
  Print a QR code for distributed Erlang pairing (probnik_pair).
  """
  def show_pair do
    :probnik_qr.show_pair()
  end

  @doc """
  Returns the pairing payload as a binary (for custom QR generation).
  """
  def payload do
    :probnik_qr.payload()
  end

  @doc """
  Returns the net payload as a binary (probnikoff_net).
  """
  def payload_net do
    :probnik_qr.payload_net()
  end

  @doc """
  Returns the distributed pairing payload as a binary (probnik_pair).
  """
  def payload_pair do
    :probnik_qr.payload_pair()
  end

  @doc """
  Print an ANSI QR code encoding an arbitrary Erlang term.

  Consumers like zed use this to render their own pairing payloads
  without needing to add new tag-specific functions to this library.
  The term is serialised with `io_lib:format("~p", [Term])` — same
  wire format as `payload_pair/0`.

  ## Example

      iex> ProbnikQR.show_term({:zed_admin, node(), {192, 168, 0, 33}, 4040, "sha256:...", "ott_...", 0})
      :ok
  """
  def show_term(term), do: :probnik_qr.show_term(term)

  @doc """
  Return the ANSI-rendered QR for `term` without printing it.

  Returns `{:ok, iodata}` on success, `{:error, reason}` if the QR
  encoder is unavailable or failed. Prefer this for tests, LiveView
  embedding, or any caller controlling its own output.
  """
  def render_term(term), do: :probnik_qr.render_term(term)

  @doc """
  Serialise an arbitrary Erlang term to the same wire format used by
  `payload_pair/0`. Useful for cross-language consumers (mobile apps
  with Erlang-term regex parsers) that need the binary without the
  QR rendering.
  """
  def payload_term(term), do: :probnik_qr.payload_term(term)
end
