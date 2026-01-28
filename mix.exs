defmodule ProbnikQR.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :probnik_qr,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:eqrcode, "~> 0.1.10"}
    ]
  end

  defp description do
    """
    Generate QR codes for pairing with Probnik BEAM node monitor.
    Add this to any Elixir/Erlang app to enable QR-based pairing.
    """
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{}
    ]
  end
end
