# ProbnikQR

Generate ASCII QR codes for pairing with the Probnik BEAM node monitor.
The core implementation is Erlang (`:probnik_qr`) with a thin Elixir wrapper
(`ProbnikQR`), so both Erlang and Elixir apps can depend on the same library.

## Dependencies

For now, the library pulls the QR encoder via a git dependency on
`https://github.com/komone/qrcode`.

### Mix (Elixir)

```elixir
def deps do
  [
    {:probnik_qr, path: "../probnik_qr"}
  ]
end
```

The dependency uses a custom compile command because `qrcode` does not ship
with a Mix or Rebar config. This is already set in `mix.exs`.

### Rebar3 (Erlang)

```erlang
{deps, [
    {probnik_qr, {git, "YOUR_REPO_URL", {branch, "main"}}}
]}.
```

## Usage

Start a distributed node and call `show/0`:

### Elixir

```
iex --sname myapp@localhost --cookie secret -S mix
iex> ProbnikQR.show()
```

### Erlang

```
erl -sname myapp@localhost -setcookie secret
1> probnik_qr:show().
```

The QR encodes this Erlang term:

```
{probnik_pair, 'myapp@localhost', secret, [{mode, shortnames}]}
```
