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
end
