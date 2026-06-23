#!/usr/bin/env elixir

defmodule GetShells do
  @bufsize 1024 * 1024

  def run do
    {:ok, fd} = :file.open("passwd", [:read, :raw, :binary])

    counts =
      scan(fd, %{}, 0, false, [])

    :file.close(fd)

    Enum.each(counts, fn {shell, n} ->
      IO.puts("#{shell} : #{n}")
    end)
  end

  defp scan(fd, counts, colon_count, capturing, shell) do
    case :file.read(fd, @bufsize) do
      {:ok, data} ->
        {counts, colon_count, capturing, shell} =
          parse_chunk(
            data,
            counts,
            colon_count,
            capturing,
            shell
          )

        scan(
          fd,
          counts,
          colon_count,
          capturing,
          shell
        )

      :eof ->
        counts
    end
  end

  defp parse_chunk(
         <<>>,
         counts,
         colon_count,
         capturing,
         shell
       ) do
    {counts, colon_count, capturing, shell}
  end

  defp parse_chunk(
         <<?:, rest::binary>>,
         counts,
         colon_count,
         _capturing,
         shell
       ) do
    parse_chunk(
      rest,
      counts,
      colon_count + 1,
      colon_count + 1 == 6,
      if(colon_count + 1 == 6, do: [], else: shell)
    )
  end

  defp parse_chunk(
         <<?\n, rest::binary>>,
         counts,
         _colon_count,
         _capturing,
         shell
       ) do
    counts =
      if shell == [] do
        counts
      else
        shell =
          shell
          |> Enum.reverse()
          |> :erlang.list_to_binary()

        Map.update(counts, shell, 1, &(&1 + 1))
      end

    parse_chunk(rest, counts, 0, false, [])
  end

  defp parse_chunk(
         <<c, rest::binary>>,
         counts,
         colon_count,
         true,
         shell
       ) do
    parse_chunk(
      rest,
      counts,
      colon_count,
      true,
      [c | shell]
    )
  end

  defp parse_chunk(
         <<_, rest::binary>>,
         counts,
         colon_count,
         capturing,
         shell
       ) do
    parse_chunk(
      rest,
      counts,
      colon_count,
      capturing,
      shell
    )
  end
end

GetShells.run()
