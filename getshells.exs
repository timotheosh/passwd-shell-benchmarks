#!elixir

"passwd"
|> File.stream!()
|> Enum.map(&(String.split(String.trim(&1), ":") |> List.last()))
|> Enum.frequencies()
|> Enum.each(fn {shell, n} ->
  IO.puts("#{String.pad_trailing(shell, 20)}: #{n}")
end)
