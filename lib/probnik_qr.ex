defmodule ProbnikQR do
  @moduledoc """
  Generate QR codes for pairing with Probnik BEAM node monitor.

  ## Usage

  On the node you want to monitor, call:

      iex> ProbnikQR.show()

  This prints an ASCII QR code to the terminal. Scan it with the
  Probnik Android app to establish a connection.

  ## Requirements

  The node must be started with distribution enabled:

      iex --sname myapp@localhost --cookie secret -S mix

  Or for long names:

      iex --name myapp@myhost.local --cookie secret -S mix
  """

  @pair_tag :probnik_pair

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
    node = node()

    if node == :nonode@nohost do
      IO.puts("Error: Node not started with distribution enabled.")
      IO.puts("Start with: iex --sname myapp@localhost --cookie secret -S mix")
      :error
    else
      cookie = Node.get_cookie()
      mode = detect_name_mode(node)
      term = {@pair_tag, node, cookie, [{:mode, mode}]}

      payload =
        :io_lib.format("~p", [term])
        |> IO.iodata_to_binary()

      qr =
        payload
        |> EQRCode.encode()
        |> EQRCode.render()

      IO.puts(qr)
      IO.puts("\nNode: #{node}")
      IO.puts("Mode: #{mode}")
      :ok
    end
  end

  @doc """
  Returns the pairing payload as a string (for custom QR generation).
  """
  def payload do
    node = node()
    cookie = Node.get_cookie()
    mode = detect_name_mode(node)
    term = {@pair_tag, node, cookie, [{:mode, mode}]}

    :io_lib.format("~p", [term])
    |> IO.iodata_to_binary()
  end

  defp detect_name_mode(node) when is_atom(node) do
    node
    |> Atom.to_string()
    |> detect_name_mode()
  end

  defp detect_name_mode(node) when is_binary(node) do
    case String.split(node, "@") do
      [_, host] ->
        if String.contains?(host, ".") do
          :longnames
        else
          :shortnames
        end

      _ ->
        :shortnames
    end
  end
end
