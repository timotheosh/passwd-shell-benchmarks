#!/usr/bin/env elixir

File.stream!("passwd")
|> Enum.reduce(%{}, fn line, counts ->
  shell =
    line
    |> String.trim_trailing()
    |> :binary.split(":", [:global])
    |> List.last()

  Map.update(counts, shell, 1, &(&1 + 1))
end)
|> Enum.each(fn {shell, n} ->
  IO.puts("#{shell} : #{n}")
end)
