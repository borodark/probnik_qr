defmodule ProbnikQR.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :probnik_qr,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      source_url: "https://github.com/borodark/probnik_qr",
      homepage_url: "https://github.com/borodark/probnik_qr",
      deps: deps(),
      description: description(),
      package: package(),
      docs: docs()
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
       app: false},
      {:ex_doc, "~> 0.34", only: :dev, runtime: false}
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

  defp docs do
    [
      main: "ProbnikQR",
      extras: ["README.md", "CHANGELOG.md"],
      source_ref: "v#{@version}",
      source_url: "https://github.com/borodark/probnik_qr"
    ]
  end
end
