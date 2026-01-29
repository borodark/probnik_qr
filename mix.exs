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
      {:qrcode,
       git: "https://github.com/komone/qrcode",
       branch: "master",
       compile: "erlc -o ebin src/*.erl",
       app: false}
    ]
  end

  defp description do
    """
    Generate ASCII QR codes for pairing with Probnik BEAM node monitor.
    Erlang core with an Elixir wrapper for dual-runtime use.
    """
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{}
    ]
  end
end
